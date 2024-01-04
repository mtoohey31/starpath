module type TokenType = sig
  type t
  type pos

  val pos0 : pos
end

module type CombinatorsType = sig
  type token
  type pos
  type 'a t

  val parse : (pos * token) Seq.t -> 'a t -> ('a, string) result
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val eof : unit t
  val fail : string -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val optional : 'a t -> 'a option t
  val peek_token : token option t
  val return : 'a -> 'a t
  val satisfy : (token -> bool) -> token t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val skip_while : (token -> bool) -> unit t
  val take_while1 : (token -> bool) -> token list t
  val take_while : (token -> bool) -> token list t
  val token_not : token -> token t
  val token : token -> token t
end

module Make (Token : TokenType) = struct
  type token = Token.t
  type pos = Token.pos
  type state = { input : (Token.pos * Token.t) Seq.t; last_pos : Token.pos }
  type 'rest with_state = state -> 'rest

  type ('result, 'result') success =
    ('result -> ('result', string) result) with_state

  (* TODO: Include pos, maybe include branching tree? *)
  type 'result failure = (string -> ('result, string) result) with_state

  type 'result t = {
    run :
      'result'.
      (('result, 'result') success ->
      'result' failure ->
      ('result', string) result)
      with_state;
  }

  let ( >>= ) r f =
    let run st succ fail =
      let succ' st' v = (f v).run st' succ fail in
      r.run st succ' fail
    in
    { run }

  let ( let* ) = ( >>= )

  let ( <|> ) r1 r2 =
    let run st succ fail =
      let fail' _ _ = r2.run st succ fail in
      r1.run st succ fail'
    in
    { run }

  let ( >>| ) r f =
    let run st succ =
      let succ' st' v = succ st' (f v) in
      r.run st succ'
    in
    { run }

  let ( let+ ) = ( >>| )

  let ( *> ) r1 r2 =
    let run st succ fail =
      let succ' st' _ = r2.run st' succ fail in
      r1.run st succ' fail
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

  let uncons = Seq.uncons

  let eof =
    let run st succ fail =
      match uncons st.input with
      | None -> succ st ()
      | _ -> fail st "expected: eof"
    in
    { run }

  let fail msg =
    let run st _ fail = fail st msg in
    { run }

  let fix f =
    let rec p = lazy (f r)
    and r = { run = (fun st succ fail -> (Lazy.force p).run st succ fail) } in
    r

  let optional r =
    let run st succ _ =
      let succ' st' v = succ st' (Some v) in
      let fail' _ _ = succ st None in
      r.run st succ' fail'
    in
    { run }

  let peek_token =
    let run st succ _ =
      let v = Option.map (fun ((_, v), _) -> v) (uncons st.input) in
      succ st v
    in
    { run }

  let return v =
    let run st succ _ = succ st v in
    { run }

  let satisfy f =
    let run st succ fail =
      match uncons st.input with
      | None -> fail st "eof"
      | Some ((last_pos, t), input) when f t -> succ { input; last_pos } t
      | _ -> fail st "unexpected"
    in
    { run }

  let sep_by1 sep inner =
    fix @@ fun rest ->
    let* v = inner in
    let* vs = optional (sep *> rest) >>| Option.value ~default:[] in
    return (v :: vs)

  let sep_by sep inner =
    optional (sep_by1 sep inner) >>| Option.value ~default:[]

  let skip_while f =
    let rec skip_while' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t -> skip_while' { input; last_pos }
      | _ -> st
    in
    let run st succ _ = succ (skip_while' st) () in
    { run }

  let take_while f =
    let rec take_while' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
          let st', v = take_while' { input; last_pos } in
          (st', t :: v)
      | _ -> (st, [])
    in
    let run st succ _ =
      let st', v = take_while' st in
      succ st' v
    in
    { run }

  let take_while1 f =
    let rec take_while1' st =
      match uncons st.input with
      | Some ((last_pos, t), input) when f t ->
          let st', v = take_while1' { input; last_pos } in
          (st', t :: v)
      | _ -> (st, [])
    in
    let run st succ fail =
      match take_while1' st with
      | _, [] -> fail st "none taken"
      | st', v -> succ st' v
    in
    { run }

  let token t = satisfy (( = ) t)
  let token_not t = satisfy (( <> ) t)

  let parse input r =
    let fail _ msg = Error msg in
    let succ _ v = Ok v in
    let r' = r <* eof in
    r'.run { input; last_pos = Token.pos0 } succ fail
end

type pos' = { row : int; col : int }

module CharToken = struct
  type t = Char.t
  type pos = pos'

  let pos0 = { row = 1; col = 1 }
end

module StringCombinators = struct
  include Make (CharToken)

  let string s =
    let string_tail s = String.sub s 1 (String.length s - 1) in
    let rec strip_prefix s { input; last_pos } =
      match (s, uncons input) with
      | "", _ -> Some { input; last_pos }
      | _, Some ((last_pos, c), input) when c = s.[0] ->
          strip_prefix (string_tail s) { input; last_pos }
      | _ -> None
    in
    let run st succ fail =
      match strip_prefix s st with
      | Some st' -> succ st' s
      | None -> fail st ("expected: " ^ s)
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
    aux (CharToken.pos0, 0)

  let parse_string s r = parse (input_of_string s) r
end
