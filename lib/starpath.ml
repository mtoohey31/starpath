module type Token = sig
  type t

  val string_of_token : t -> string
end

module type Pos = sig
  type t
  type pos0_arg

  val pos0 : pos0_arg -> t
  val compare : t -> t -> int
  val string_of_pos : t -> string
end

module type Combinators = sig
  type token
  type pos
  type pos0_arg
  type parse_error = { pos : pos; expected : string list; actual : string }

  val string_of_parse_error : parse_error -> string

  type 'a t

  val parse :
    pos0_arg -> (pos * token) Seq.t -> 'a t -> ('a, parse_error) result

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( @> ) : 'a t -> 'b t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val eof : unit t
  val fail : parse_error -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val lookahead : 'a t -> 'a t
  val optional : 'a t -> 'a option t
  val optional_or : 'a t -> default:'a -> 'a t
  val optional_or_else : 'a t -> default_f:(unit -> 'a) -> 'a t
  val peek : token option t
  val pos : 'a t -> (pos * 'a) t
  val repeat : 'a t -> 'a list t
  val repeat1 : 'a t -> 'a list t
  val return : 'a -> 'a t
  val return_at : pos -> 'a -> 'a t
  val satisfy : expected:string list -> (token -> bool) -> token t
  val satisfy_map : expected:string list -> (token -> 'a option) -> 'a t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val skip_while : (token -> bool) -> unit t
  val take_while1 : expected:string list -> (token -> bool) -> token list t
  val take_while : (token -> bool) -> token list t
  val token_not : token -> token t
  val token : token -> token t
end

module Make (Token : Token) (Pos : Pos) = struct
  type token = Token.t
  type pos = Pos.t
  type pos0_arg = Pos.pos0_arg
  type parse_error = { pos : pos; expected : string list; actual : string }

  let string_of_parse_error { pos; expected; actual } =
    Printf.sprintf "%s: expected %s, found %s" (Pos.string_of_pos pos)
      (String.concat " | " expected)
      actual

  type state = { input : (pos * Token.t) Seq.t; last_pos : pos }
  type 'a with_state = state -> 'a
  type ('a, 'b) success = (pos * 'a -> ('b, parse_error) result) with_state
  type 'a failure = parse_error -> ('a, parse_error) result

  type 'a t = {
    run :
      'b.
      (('a, 'b) success -> 'b failure -> ('b, parse_error) result) with_state;
  }

  let ( >>| ) r f =
    let run st succ =
      let succ' st' (p, v) = succ st' (p, f v) in
      r.run st succ'
    in
    { run }

  let ( >>= ) r f =
    let run st succ fail =
      let succ' st' (p, v) =
        let succ'' st'' (_, v') = succ st'' (p, v') in
        (f v).run st' succ'' fail
      in
      r.run st succ' fail
    in
    { run }

  let ( <|> ) r1 r2 =
    let run st succ fail =
      let fail' pe =
        let fail'' pe' =
          let compare = Pos.compare pe.pos pe'.pos in
          if compare < 0 then fail pe'
          else if compare = 0 && pe.actual = pe'.actual then
            let expected =
              List.sort_uniq String.compare (pe.expected @ pe'.expected)
            in
            fail { pe with expected }
          else fail pe
        in
        r2.run st succ fail''
      in
      r1.run st succ fail'
    in
    { run }

  let ( <* ) r1 r2 =
    let run st succ fail =
      let succ' st' pv =
        let succ'' st'' _ = succ st'' pv in
        r2.run st' succ'' fail
      in
      r1.run st succ' fail
    in
    { run }

  let ( *> ) r1 r2 =
    let run st succ fail =
      let succ' st' _ = r2.run st' succ fail in
      r1.run st succ' fail
    in
    { run }

  let ( @> ) r1 r2 =
    let run st succ fail =
      let succ' st' (p, _) =
        let succ'' st'' (_, v) = succ st'' (p, v) in
        r2.run st' succ'' fail
      in
      r1.run st succ' fail
    in
    { run }

  let ( let+ ) = ( >>| )
  let ( let* ) = ( >>= )

  let eof =
    let run st succ fail =
      match st.input () with
      | Nil -> succ st (st.last_pos, ())
      | Cons ((pos, t), _) ->
          fail { pos; expected = [ "EOF" ]; actual = Token.string_of_token t }
    in
    { run }

  let fail pe =
    let run _ _ fail = fail pe in
    { run }

  let fix f =
    let rec p = lazy (f r)
    and r = { run = (fun st succ fail -> (Lazy.force p).run st succ fail) } in
    r

  let lookahead r =
    let run st succ fail =
      let succ' _ pv = succ st pv in
      r.run st succ' fail
    in
    { run }

  let optional r =
    let run st succ _ =
      let succ' st' (p, v) = succ st' (p, Some v) in
      let fail' _ = succ st (st.last_pos, None) in
      r.run st succ' fail'
    in
    { run }

  let optional_or r ~default = optional r >>| Option.value ~default

  let optional_or_else r ~default_f =
    let+ v = optional r in
    match v with Some v -> v | None -> default_f ()

  let peek =
    let run st succ _ =
      let v =
        match st.input () with
        | Cons ((p, v), _) -> (p, Some v)
        | Nil -> (st.last_pos, None)
      in
      succ st v
    in
    { run }

  let pos r =
    let run st succ fail =
      let succ' st' (p, v) = succ st' (p, (p, v)) in
      r.run st succ' fail
    in
    { run }

  let return v =
    let run st succ _ =
      let p =
        match st.input () with Cons ((p, _), _) -> p | Nil -> st.last_pos
      in
      succ st (p, v)
    in
    { run }

  let return_at p v =
    let run st succ _ = succ st (p, v) in
    { run }

  let repeat r =
    fix (fun r' -> r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> return [])

  let repeat1 r =
    fix (fun r' ->
        r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> (r >>| fun x -> [ x ]))

  let satisfy ~expected f =
    let run st succ fail =
      match st.input () with
      | Nil -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Cons (((last_pos, t) as v), input) when f t ->
          succ { input; last_pos } v
      | Cons ((pos, t), _) ->
          fail { pos; expected; actual = Token.string_of_token t }
    in
    { run }

  let satisfy_map ~expected f =
    let run st succ fail =
      match st.input () with
      | Nil -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Cons ((pos, t), input) -> begin
          match f t with
          | Some v -> succ { input; last_pos = pos } (pos, v)
          | None -> fail { pos; expected; actual = Token.string_of_token t }
        end
    in
    { run }

  let sep_by1 sep inner =
    fix @@ fun rest ->
    let* p, v = inner |> pos in
    let* vs = optional_or (sep *> rest) ~default:[] in
    return_at p (v :: vs)

  let sep_by sep inner =
    optional (sep_by1 sep inner) >>| Option.value ~default:[]

  let skip_while f =
    let rec skip_while' st =
      match st.input () with
      | Cons ((last_pos, t), input) when f t -> skip_while' { input; last_pos }
      | _ -> st
    in
    let run st succ _ = succ (skip_while' st) (st.last_pos, ()) in
    { run }

  let take_while f =
    let rec take_while' st =
      match st.input () with
      | Cons ((last_pos, t), input) when f t ->
          let st', (_, v) = take_while' { input; last_pos } in
          (st', (last_pos, t :: v))
      | _ -> (st, (st.last_pos, []))
    in
    let run st succ _ =
      let st', v = take_while' st in
      succ st' v
    in
    { run }

  let take_while1 ~expected f =
    let rec take_while1' st =
      match st.input () with
      | Cons ((last_pos, t), input) when f t ->
          let st', (_, v) = take_while1' { input; last_pos } in
          (st', (last_pos, t :: v))
      | _ -> (st, (st.last_pos, []))
    in
    let run st succ fail =
      match take_while1' st with
      | _, (_, []) -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | st', v -> succ st' v
    in
    { run }

  let token t = satisfy ~expected:[ Token.string_of_token t ] (( = ) t)

  let token_not t =
    satisfy ~expected:[ "not " ^ Token.string_of_token t ] (( <> ) t)

  let parse pos0_arg input r =
    let fail pe = Error pe in
    let succ _ (_, v) = Ok v in
    let r' = r <* eof in
    r'.run { input; last_pos = Pos.pos0 pos0_arg } succ fail
end

module CharToken : Token with type t = char = struct
  type t = char

  let string_of_token c = "'" ^ Char.escaped c ^ "'"
end

type string_pos = { row : int; col : int }

module StringPos : Pos with type t = string_pos with type pos0_arg = unit =
struct
  type t = string_pos
  type pos0_arg = unit

  let pos0 () = { row = 1; col = 1 }

  let compare p1 p2 =
    let compare_row = compare p1.row p2.row in
    if compare_row <> 0 then compare_row else compare p1.col p2.col

  let string_of_pos { row; col } = string_of_int row ^ ":" ^ string_of_int col
end

type file_pos = { path : string; row : int; col : int }

module FilePos : Pos with type t = file_pos with type pos0_arg = string = struct
  type t = file_pos
  type pos0_arg = string

  let pos0 path = { path; row = 1; col = 1 }

  let compare p1 p2 =
    let compare_path = String.compare p1.path p2.path in
    if compare_path <> 0 then compare_path
    else
      let compare_row = compare p1.row p2.row in
      if compare_row <> 0 then compare_row else compare p1.col p2.col

  let string_of_pos { path; row; col } =
    path ^ ":" ^ string_of_int row ^ ":" ^ string_of_int col
end

module type CharCombinators = sig
  include Combinators with type token = CharToken.t

  val string : string -> string t
end

module MakeChar (Pos : Pos) :
  CharCombinators with type pos = Pos.t with type pos0_arg = Pos.pos0_arg =
struct
  include Make (CharToken) (Pos)

  let string s =
    let string_tail s = String.sub s 1 (String.length s - 1) in
    let rec strip_prefix s ({ input; last_pos } : state) =
      match (s, input ()) with
      | "", _ -> Ok (last_pos, { input; last_pos })
      | _, Cons ((last_pos, c), input) when c = s.[0] -> begin
          match strip_prefix (string_tail s) { input; last_pos } with
          | Ok (_, st) -> Ok (last_pos, st)
          | Error (cs, p, eof) -> Error (c :: cs, p, eof)
        end
      | _, Cons ((last_pos, c), _) -> Error ([ c ], last_pos, false)
      | _, Nil -> Error ([], last_pos, true)
    in
    let run st succ fail =
      match strip_prefix s st with
      | Ok (p, st') -> succ st' (p, s)
      | Error (cs, pos, eof) ->
          let actual = String.of_seq (List.to_seq cs) in
          fail
            {
              pos;
              expected = [ "\"" ^ String.escaped s ^ "\"" ];
              actual =
                ("\"" ^ String.escaped actual ^ "\""
                ^ if eof then " EOF" else "");
            }
    in
    { run }
end

module StringCombinators = struct
  include MakeChar (StringPos)

  let input_of_string s : (pos * char) Seq.t =
    let bs = Bytes.unsafe_of_string s in
    let rec aux ((p : pos), i) () =
      if i = Bytes.length bs then Seq.Nil
      else
        let b = Bytes.get bs i in
        let p' =
          if b = '\n' then { row = p.row + 1; col = 1 }
          else { p with col = p.col + 1 }
        in
        Seq.Cons ((p, b), aux (p', i + 1))
    in
    aux (StringPos.pos0 (), 0)

  let parse_string s r = parse () (input_of_string s) r
end

module FileCombinators = struct
  include MakeChar (FilePos)

  let input_of_string p s =
    let bs = Bytes.unsafe_of_string s in
    let rec aux (p, i) () =
      if i = Bytes.length bs then Seq.Nil
      else
        let b = Bytes.get bs i in
        let p' =
          if b = '\n' then { p with row = p.row + 1; col = 1 }
          else { p with col = p.col + 1 }
        in
        Seq.Cons ((p, b), aux (p', i + 1))
    in
    aux (FilePos.pos0 p, 0)

  let parse_file p ?preprocess r =
    let f = open_in_bin p in
    let s = really_input_string f (in_channel_length f) in
    close_in f;
    let s = match preprocess with Some f -> f s | None -> s in
    parse p (input_of_string p s) r
end
