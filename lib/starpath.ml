module type TokenType = sig
  type t
  type pos

  val pos0 : pos
  val advance : pos -> t -> pos
end

module type CombinatorsType = sig
  type token
  type 'a t

  val satisfy : (token -> bool) -> token t
  val token : token -> token t
  val eof : unit t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val parse : token list -> 'a t -> ('a, string) result
end

module Make (Token : TokenType) = struct
  type token = Token.t
  type state = { input : Token.t list; pos : Token.pos }
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

  let satisfy f =
    let run ({ input; pos } as st) succ fail =
      match input with
      | [] -> fail st "eof"
      | head :: tail when f head ->
          let pos = Token.advance pos head in
          succ { input = tail; pos } head
      | _ -> fail st "unexpected"
    in
    { run }

  let token t = satisfy (( = ) t)

  let eof =
    let run ({ input; _ } as st) succ fail =
      match input with [] -> succ st () | _ -> fail st "expected: eof"
    in
    { run }

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

  let parse input r =
    let fail _ msg = Error msg in
    let succ _ v = Ok v in
    let r' = r <* eof in
    r'.run { pos = Token.pos0; input } succ fail
end

module CharToken = struct
  type t = Char.t
  type pos = { row : int; col : int }

  let pos0 = { row = 1; col = 1 }

  let advance p c =
    if c = '\n' then { row = p.row + 1; col = 1 }
    else { p with col = p.col + 1 }
end

module StringCombinators = struct
  include Make (CharToken)

  let explode s =
    let rec exp a b = if a < 0 then b else exp (a - 1) (s.[a] :: b) in
    exp (String.length s - 1) []

  let string s =
    let string_tail s = String.sub s 1 (String.length s - 1) in
    let rec starts_with s cs =
      match (s, cs) with
      | "", _ -> true
      | _, c :: cs when c = s.[0] -> starts_with (string_tail s) cs
      | _ -> false
    in
    let rec drop n xs =
      match (n, xs) with
      | _ when n < 0 ->
          raise (Invalid_argument (string_of_int n)) [@coverage off]
      | 0, _ -> xs
      | _, _ :: xs -> drop (n - 1) xs
      | _, [] -> raise (Invalid_argument "list too small") [@coverage off]
    in
    let run st succ fail =
      if starts_with s st.input then
        let pos = Seq.fold_left CharToken.advance st.pos (String.to_seq s) in
        let input = drop (String.length s) st.input in
        let st' = { pos; input } in
        succ st' s
      else fail st ("expected: " ^ s)
    in
    { run }

  let parse_string s r = parse (explode s) r
end
