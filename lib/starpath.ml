module type TokenType = sig
  type t

  val string_of_token : t -> string

  type pos

  val compare_pos : pos -> pos -> int
  val string_of_pos : pos -> string
  val pos0 : pos
end

module type CombinatorsType = sig
  type token
  type pos
  type parse_error = { pos : pos; expected : string list; actual : string }

  val string_of_parse_error : parse_error -> string

  type 'a t

  val parse : (pos * token) Seq.t -> 'a t -> ('a, parse_error) result
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
  val optional : 'a t -> 'a option t
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

module Make (Token : TokenType) = struct
  type token = Token.t
  type pos = Token.pos
  type parse_error = { pos : pos; expected : string list; actual : string }

  let string_of_parse_error { pos; expected; actual } =
    Printf.sprintf "%s: expected %s, found %s" (Token.string_of_pos pos)
      (String.concat " | " expected)
      actual

  type state = { input : (Token.pos * Token.t) Seq.t; last_pos : Token.pos }
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
      let succ' st' (_, v) = (f v).run st' succ fail in
      r.run st succ' fail
    in
    { run }

  let ( <|> ) r1 r2 =
    let run st succ fail =
      let fail' pe =
        let fail'' pe' =
          let compare = Token.compare_pos pe.pos pe'.pos in
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
  let uncons = Seq.uncons

  let eof =
    let run st succ fail =
      match uncons st.input with
      | None -> succ st (st.last_pos, ())
      | Some ((pos, t), _) ->
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

  let optional r =
    let run st succ _ =
      let succ' st' (p, v) = succ st' (p, Some v) in
      let fail' _ = succ st (st.last_pos, None) in
      r.run st succ' fail'
    in
    { run }

  let peek =
    let run st succ _ =
      let v =
        match uncons st.input with
        | Some ((pos, v), _) -> (pos, Some v)
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
    let run st succ _ = succ st (st.last_pos, v) in
    { run }

  let return_at pos v =
    let run st succ _ = succ st (pos, v) in
    { run }

  let repeat r =
    fix (fun r' -> r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> return [])

  let repeat1 r =
    fix (fun r' ->
        r >>= (fun x -> r' >>| fun xs -> x :: xs) <|> (r >>| fun x -> [ x ]))

  let satisfy ~expected f =
    let run st succ fail =
      match uncons st.input with
      | None -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Some (((last_pos, t) as v), input) when f t ->
          succ { input; last_pos } v
      | Some ((pos, t), _) ->
          fail { pos; expected; actual = Token.string_of_token t }
    in
    { run }

  let satisfy_map ~expected f =
    let run st succ fail =
      match uncons st.input with
      | None -> fail { pos = st.last_pos; expected; actual = "EOF" }
      | Some ((p, t), input) -> begin
          match f t with
          | Some v -> succ { input; last_pos = p } (p, v)
          | None -> fail { pos = p; expected; actual = Token.string_of_token t }
        end
    in
    { run }

  let sep_by1 sep inner =
    fix @@ fun rest ->
    let* pos, v = inner |> pos in
    let* vs = optional (sep *> rest) >>| Option.value ~default:[] in
    return_at pos (v :: vs)

  let sep_by sep inner =
    optional (sep_by1 sep inner) >>| Option.value ~default:[]

  let skip_while f =
    let rec skip_while' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t -> skip_while' { input; last_pos }
      | _ -> st
    in
    let run st succ _ = succ (skip_while' st) (st.last_pos, ()) in
    { run }

  let take_while f =
    let rec take_while' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
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
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
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

  let parse input r =
    let fail pe = Error pe in
    let succ _ (_, v) = Ok v in
    let r' = r <* eof in
    r'.run { input; last_pos = Token.pos0 } succ fail
end

type char_pos = { row : int; col : int }

module CharToken = struct
  type t = Char.t

  let string_of_token c = "'" ^ Char.escaped c ^ "'"

  type pos = char_pos

  let compare_pos p1 p2 =
    let compare_row = compare p1.row p2.row in
    if compare_row <> 0 then compare_row else compare p1.col p2.col

  let string_of_pos { row; col } = string_of_int row ^ ":" ^ string_of_int col
  let pos0 = { row = 1; col = 1 }
end

module StringCombinators = struct
  include Make (CharToken)

  let string s =
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
    let run st succ fail =
      match strip_prefix s st with
      | Ok (pos, st') -> succ st' (pos, s)
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

  let input_of_string s =
    let bs = Bytes.unsafe_of_string s in
    let rec aux (p, i) () =
      if i = Bytes.length bs then Seq.Nil
      else
        let b = Bytes.get bs i in
        let p' =
          if b = '\n' then { row = p.row + 1; col = 0 }
          else { p with col = p.col + 1 }
        in
        Seq.Cons ((p', b), aux (p', i + 1))
    in
    aux ({ row = 1; col = 0 }, 0)

  let parse_string s r = parse (input_of_string s) r
end
