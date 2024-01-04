module type TokenType = sig
  type t

  val string_of_token : t -> string

  type pos

  val string_of_pos : pos -> string
  val pos0 : pos
end

module type CombinatorsType = sig
  type token
  type pos
  type parse_error = { pos : pos; expected : string; actual : string }

  val string_of_parse_error : parse_error -> string

  type 'a t

  val parse : (pos * token) Seq.t -> 'a t -> ('a, parse_error) result
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>@ ) : 'a t -> ('a -> pos -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>& ) : 'a t -> ('a -> pos -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( @> ) : 'a t -> 'b t -> 'b t
  val ( let| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let@ ) : 'a t -> ('a * pos -> 'b) -> 'b t
  val ( let= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let& ) : 'a t -> ('a * pos -> 'b t) -> 'b t
  val eof : unit t
  val fail : parse_error -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val optional : 'a t -> 'a option t
  val peek : (token * pos) option t
  val peek_pos : pos option t
  val peek_token : token option t
  val return : 'a -> 'a t
  val return_at : pos -> 'a -> 'a t
  val satisfy : expected:string -> (token -> bool) -> token t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val skip_while : (token -> bool) -> unit t
  val take_while1 : expected:string -> (token -> bool) -> token list t
  val take_while : (token -> bool) -> token list t
  val token_not : token -> token t
  val token : token -> token t
end

module Make (Token : TokenType) = struct
  type token = Token.t
  type pos = Token.pos
  type parse_error = { pos : pos; expected : string; actual : string }

  let string_of_parse_error { pos; expected; actual } =
    Printf.sprintf "%s: expected %s, found %s" (Token.string_of_pos pos)
      expected actual

  type state = { input : (Token.pos * Token.t) Seq.t; last_pos : Token.pos }
  type 'a with_state = state -> 'a
  type ('a, 'b) success = (pos * 'a -> ('b, parse_error) result) with_state
  type 'a failure = (parse_error -> ('a, parse_error) result) with_state

  type 'a t = {
    run :
      'b.
      (('a, 'b) success -> 'b failure -> ('b, parse_error) result) with_state;
  }

  let ( >>| ) r f =
    let run st succ =
      let succ' st' (pos, v) = succ st' (pos, f v) in
      r.run st succ'
    in
    { run }

  let ( >>@ ) r f =
    let run st succ =
      let succ' st' (pos, v) = succ st' (pos, f v pos) in
      r.run st succ'
    in
    { run }

  let ( >>= ) r f =
    let run st succ fail =
      let succ' st' (_, v) = (f v).run st' succ fail in
      r.run st succ' fail
    in
    { run }

  let ( >>& ) r f =
    let run st succ fail =
      let succ' st' (pos, v) = (f v pos).run st' succ fail in
      r.run st succ' fail
    in
    { run }

  let ( <|> ) r1 r2 =
    let run st succ fail =
      let fail' _ _ = r2.run st succ fail in
      r1.run st succ fail'
    in
    { run }

  let ( <* ) r1 r2 =
    let run st succ fail =
      let succ' st' v =
        let succ'' st'' _ = succ st'' v in
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
      let succ' st' (pos, _) =
        let succ'' st'' (_, v) = succ st'' (pos, v) in
        r2.run st' succ'' fail
      in
      r1.run st succ' fail
    in
    { run }

  let ( let| ) = ( >>| )
  let ( let@ ) r f = r >>@ fun v pos -> f (v, pos)
  let ( let= ) = ( >>= )
  let ( let& ) r f = r >>& fun v pos -> f (v, pos)
  let uncons = Seq.uncons

  let eof =
    let run st succ fail =
      match uncons st.input with
      | None -> succ st (st.last_pos, ())
      | Some ((pos, t), _) ->
          fail st { pos; expected = "EOF"; actual = Token.string_of_token t }
    in
    { run }

  let fail pe =
    let run st _ fail = fail st pe in
    { run }

  let fix f =
    let rec p = lazy (f r)
    and r = { run = (fun st succ fail -> (Lazy.force p).run st succ fail) } in
    r

  let optional r =
    let run st succ _ =
      let succ' st' (p, v) = succ st' (p, Some v) in
      let fail' _ _ = succ st (st.last_pos, None) in
      r.run st succ' fail'
    in
    { run }

  let peek =
    let run st succ _ =
      let v =
        match uncons st.input with
        | Some ((pos, v), _) -> (pos, Some (v, pos))
        | None -> (st.last_pos, None)
      in
      succ st v
    in
    { run }

  let peek_pos =
    let run st succ _ =
      let v =
        match uncons st.input with
        | Some ((pos, _), _) -> (pos, Some pos)
        | None -> (st.last_pos, None)
      in
      succ st v
    in
    { run }

  let peek_token =
    let run st succ _ =
      let v =
        match uncons st.input with
        | Some ((pos, v), _) -> (pos, Some v)
        | None -> (st.last_pos, None)
      in
      succ st v
    in
    { run }

  let return v =
    let run st succ _ = succ st (st.last_pos, v) in
    { run }

  let return_at pos v =
    let run st succ _ = succ st (pos, v) in
    { run }

  let satisfy ~expected f =
    let run st succ fail =
      match uncons st.input with
      | None -> fail st { pos = st.last_pos; expected; actual = "EOF" }
      | Some (((last_pos, t) as v), input) when f t ->
          succ { input; last_pos } v
      | Some ((pos, t), _) ->
          fail st { pos; expected; actual = Token.string_of_token t }
    in
    { run }

  let sep_by1 sep inner =
    fix @@ fun rest ->
    let& v, pos = inner in
    let= vs = optional (sep *> rest) >>| Option.value ~default:[] in
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
      | _, (_, []) -> fail st { pos = st.last_pos; expected; actual = "EOF" }
      | st', v -> succ st' v
    in
    { run }

  let token t = satisfy ~expected:(Token.string_of_token t) (( = ) t)

  let token_not t =
    satisfy ~expected:("not " ^ Token.string_of_token t) (( <> ) t)

  let parse input r =
    let fail _ pe = Error pe in
    let succ _ (_, v) = Ok v in
    let r' = r <* eof in
    r'.run { input; last_pos = Token.pos0 } succ fail
end

type char_pos = { row : int; col : int }

module CharToken = struct
  type t = Char.t

  let string_of_token c = "'" ^ Char.escaped c ^ "'"

  type pos = char_pos

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
          fail st
            {
              pos;
              expected = "\"" ^ String.escaped s ^ "\"";
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
          if b = '\n' then { row = p.row + 1; col = 1 }
          else { p with col = p.col + 1 }
        in
        Seq.Cons ((p', b), aux (p', i + 1))
    in
    aux ({ row = 1; col = 0 }, 0)

  let parse_string s r = parse (input_of_string s) r
end
