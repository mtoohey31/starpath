module type TokenType = sig
  type t

  val string_of_token : t -> string
end

module type CombinatorsType = sig
  type token
  type 'p parse_error = { pos : 'p; expected : string list; actual : string }

  val string_of_parse_error :
    string_of_pos:('p -> string) -> 'p parse_error -> string

  type ('p, 'a) t

  val parse :
    pos0:'p ->
    compare_pos:('p -> 'p -> int) ->
    ('p * token) Seq.t ->
    ('p, 'a) t ->
    ('a, 'p parse_error) result

  val ( >>| ) : ('p, 'a) t -> ('a -> 'b) -> ('p, 'b) t
  val ( >>= ) : ('p, 'a) t -> ('a -> ('p, 'b) t) -> ('p, 'b) t
  val ( <|> ) : ('p, 'a) t -> ('p, 'a) t -> ('p, 'a) t
  val ( <* ) : ('p, 'a) t -> ('p, 'b) t -> ('p, 'a) t
  val ( *> ) : ('p, 'a) t -> ('p, 'b) t -> ('p, 'b) t
  val ( @> ) : ('p, 'a) t -> ('p, 'b) t -> ('p, 'b) t
  val ( let+ ) : ('p, 'a) t -> ('a -> 'b) -> ('p, 'b) t
  val ( let* ) : ('p, 'a) t -> ('a -> ('p, 'b) t) -> ('p, 'b) t
  val eof : (_, unit) t
  val fail : 'p parse_error -> ('p, 'a) t
  val fix : (('p, 'a) t -> ('p, 'a) t) -> ('p, 'a) t
  val optional : ('p, 'a) t -> ('p, 'a option) t
  val optional_or : ('p, 'a) t -> default:'a -> ('p, 'a) t
  val optional_or_else : ('p, 'a) t -> default_f:(unit -> 'a) -> ('p, 'a) t
  val peek : (_, token option) t
  val pos : ('p, 'a) t -> ('p, 'p * 'a) t
  val repeat : ('p, 'a) t -> ('p, 'a list) t
  val repeat1 : ('p, 'a) t -> ('p, 'a list) t
  val return : 'a -> (_, 'a) t
  val return_at : 'p -> 'a -> ('p, 'a) t
  val satisfy : expected:string list -> (token -> bool) -> (_, token) t
  val satisfy_map : expected:string list -> (token -> 'a option) -> (_, 'a) t
  val sep_by1 : ('p, _) t -> ('p, 'a) t -> ('p, 'a list) t
  val sep_by : ('p, _) t -> ('p, 'a) t -> ('p, 'a list) t
  val skip_while : (token -> bool) -> ('p, unit) t

  val take_while1 :
    expected:string list -> (token -> bool) -> ('p, token list) t

  val take_while : (token -> bool) -> ('p, token list) t
  val token_not : token -> ('p, token) t
  val token : token -> ('p, token) t
end

module Make (Token : TokenType) = struct
  type token = Token.t
  type 'p parse_error = { pos : 'p; expected : string list; actual : string }

  let string_of_parse_error ~string_of_pos { pos; expected; actual } =
    Printf.sprintf "%s: expected %s, found %s" (string_of_pos pos)
      (String.concat " | " expected)
      actual

  type 'p state = { input : ('p * Token.t) Seq.t; last_pos : 'p }
  type ('p, 'a) with_state = 'p state -> 'a

  type ('p, 'a, 'b) success =
    ('p, 'p * 'a -> ('b, 'p parse_error) result) with_state

  type ('p, 'a) failure = 'p parse_error -> ('a, 'p parse_error) result

  type ('p, 'a) t = {
    run :
      'b.
      ( 'p,
        ('p, 'a, 'b) success ->
        ('p, 'b) failure ->
        ('p -> 'p -> int) ->
        ('b, 'p parse_error) result )
      with_state;
  }

  let ( >>| ) r f =
    let run st succ =
      let succ' st' (p, v) = succ st' (p, f v) in
      r.run st succ'
    in
    { run }

  let ( >>= ) r f =
    let run st succ fail pcmp =
      let succ' st' (_, v) = (f v).run st' succ fail pcmp in
      r.run st succ' fail pcmp
    in
    { run }

  let ( <|> ) r1 r2 =
    let run st succ fail pcmp =
      let fail' pe =
        let fail'' pe' =
          let compare = pcmp pe.pos pe'.pos in
          if compare < 0 then fail pe'
          else if compare = 0 && pe.actual = pe'.actual then
            let expected =
              List.sort_uniq String.compare (pe.expected @ pe'.expected)
            in
            fail { pe with expected }
          else fail pe
        in
        r2.run st succ fail'' pcmp
      in
      r1.run st succ fail' pcmp
    in
    { run }

  let ( <* ) r1 r2 =
    let run st succ fail pcmp =
      let succ' st' pv =
        let succ'' st'' _ = succ st'' pv in
        r2.run st' succ'' fail pcmp
      in
      r1.run st succ' fail pcmp
    in
    { run }

  let ( *> ) r1 r2 =
    let run st succ fail pcmp =
      let succ' st' _ = r2.run st' succ fail pcmp in
      r1.run st succ' fail pcmp
    in
    { run }

  let ( @> ) r1 r2 =
    let run st succ fail pcmp =
      let succ' st' (p, _) =
        let succ'' st'' (_, v) = succ st'' (p, v) in
        r2.run st' succ'' fail pcmp
      in
      r1.run st succ' fail pcmp
    in
    { run }

  let ( let+ ) = ( >>| )
  let ( let* ) = ( >>= )
  let uncons = Seq.uncons

  let eof =
    let run st succ fail _ =
      match uncons st.input with
      | None -> succ st (st.last_pos, ())
      | Some ((pos, t), _) ->
          fail { pos; expected = [ "EOF" ]; actual = Token.string_of_token t }
    in
    { run }

  let fail pe =
    let run _ _ fail _ = fail pe in
    { run }

  let fix f =
    let rec p = lazy (f r)
    and r = { run = (fun st succ fail -> (Lazy.force p).run st succ fail) } in
    r

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
    let run st succ _ _ =
      let v =
        match uncons st.input with
        | Some ((p, v), _) -> (p, Some v)
        | None -> (st.last_pos, None)
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
    let run st succ _ _ = succ st (st.last_pos, v) in
    { run }

  let return_at p v =
    let run st succ _ _ = succ st (p, v) in
    { run }

  let repeat r =
    fix (fun r' -> r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> return [])

  let repeat1 r =
    fix (fun r' ->
        r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> (r >>| fun x -> [ x ]))

  let satisfy ~expected f =
    let run st succ fail _ =
      match uncons st.input with
      | None -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Some (((last_pos, t) as v), input) when f t ->
          succ { input; last_pos } v
      | Some ((pos, t), _) ->
          fail { pos; expected; actual = Token.string_of_token t }
    in
    { run }

  let satisfy_map ~expected f =
    let run st succ fail _ =
      match uncons st.input with
      | None -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Some ((pos, t), input) -> begin
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
      match uncons st.input with
      | Some ((last_pos, t), input) when f t -> skip_while' { input; last_pos }
      | _ -> st
    in
    let run st succ _ _ = succ (skip_while' st) (st.last_pos, ()) in
    { run }

  let take_while f =
    let rec take_while' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
          let st', (_, v) = take_while' { input; last_pos } in
          (st', (last_pos, t :: v))
      | _ -> (st, (st.last_pos, []))
    in
    let run st succ _ _ =
      let st', v = take_while' st in
      succ st' v
    in
    { run }

  let take_while1 ~expected f =
    let rec take_while1' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
          let st', (_, v) = take_while1' { input; last_pos } in
          (st', (last_pos, t :: v))
      | _ -> (st, (st.last_pos, []))
    in
    let run st succ fail _ =
      match take_while1' st with
      | _, (_, []) -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | st', v -> succ st' v
    in
    { run }

  let token t = satisfy ~expected:[ Token.string_of_token t ] (( = ) t)

  let token_not t =
    satisfy ~expected:[ "not " ^ Token.string_of_token t ] (( <> ) t)

  let parse ~pos0 ~compare_pos input r =
    let fail pe = Error pe in
    let succ _ (_, v) = Ok v in
    let r' = r <* eof in
    r'.run { input; last_pos = pos0 } succ fail compare_pos
end

module CharToken = struct
  type t = Char.t

  let string_of_token c = "'" ^ Char.escaped c ^ "'"
end

type string_pos = { row : int; col : int }

let string_pos0 = { row = 1; col = 1 }

let compare_string_pos p1 p2 =
  let compare_row = compare p1.row p2.row in
  if compare_row <> 0 then compare_row else compare p1.col p2.col

let string_of_string_pos { row; col } =
  string_of_int row ^ ":" ^ string_of_int col

type file_pos = { path : string; row : int; col : int }

let file_pos0 path = { path; row = 1; col = 1 }

let compare_file_pos p1 p2 =
  let compare_path = String.compare p1.path p2.path in
  if compare_path <> 0 then compare_path
  else
    let compare_row = compare p1.row p2.row in
    if compare_row <> 0 then compare_row else compare p1.col p2.col

let string_of_file_pos { path; row; col } =
  path ^ ":" ^ string_of_int row ^ ":" ^ string_of_int col

module StringCombinators = struct
  include Make (CharToken)

  let string s : ('p, string) t =
    let string_tail s = String.sub s 1 (String.length s - 1) in
    let rec strip_prefix s { input; last_pos } =
      match (s, uncons input) with
      | "", _ -> Ok (last_pos, { input; last_pos })
      | _, Some ((last_pos, c), input) when c = s.[0] -> begin
          match strip_prefix (string_tail s) { input; last_pos } with
          | Ok (_, st) -> Ok (last_pos, st)
          | Error (cs, p, eof) -> Error (c :: cs, p, eof)
        end
      | _, Some ((last_pos, c), _) -> Error ([ c ], last_pos, false)
      | _, None -> Error ([], last_pos, true)
    in
    let run st succ fail _ =
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

  let advance_string_pos (p : string_pos) b =
    if b = '\n' then { row = p.row + 1; col = 1 }
    else { p with col = p.col + 1 }

  let advance_file_pos (p : file_pos) b =
    if b = '\n' then { p with row = p.row + 1; col = 1 }
    else { p with col = p.col + 1 }

  let input_of_string ~pos0 advance_pos s =
    let bs = Bytes.unsafe_of_string s in
    let rec aux (p, i) () =
      if i = Bytes.length bs then Seq.Nil
      else
        let b = Bytes.get bs i in
        let p' = advance_pos p b in
        Seq.Cons ((p, b), aux (p', i + 1))
    in
    aux (pos0, 0)

  let parse_string s r =
    parse ~pos0:string_pos0 ~compare_pos:compare_string_pos
      (input_of_string ~pos0:string_pos0 advance_string_pos s)
      r

  let parse_file p r =
    let f = open_in_bin p in
    let s = really_input_string f (in_channel_length f) in
    close_in f;
    let pos0 = file_pos0 p in
    parse ~pos0 ~compare_pos:compare_file_pos
      (input_of_string ~pos0 advance_file_pos s)
      r
end
