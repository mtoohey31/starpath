open Starpath.StringCombinators
open OUnit2

let assert_ok expected s r = assert_equal (Ok expected) (parse_string s r)
let assert_err expected s r = assert_equal (Error expected) (parse_string s r)
let () = assert_ok 'a' "a" (token 'a')
let () = assert_err "eof" "" (token 'a')
let () = assert_err "unexpected" "a" (token_not 'a')
let () = assert_ok '?' "?" (satisfy (fun _ -> true))
let () = assert_err "expected: eof" "ab" (token 'a')
let () = assert_ok "foo" "foo" (string "foo")
let () = assert_err "expected: bar" "foo" (string "bar")

let () =
  let r = string "foo" *> token '\n' *> string "bar" in
  assert_ok "bar" "foo\nbar" r

let () =
  let r =
    string "foo" >>| String.length
    <|> (string "bazzes" >>| String.length)
    <* string "baz"
  in
  assert_ok 6 "bazzesbaz" r

let () = assert_err "test msg" "" (fail "test msg")

let () =
  let r =
    fix (fun term ->
        token '.' *> return 1
        <|> (token '[' *> (sep_by (token ',') term >>| List.fold_left ( + ) 0)
            <* token ']'))
  in
  assert_ok 4 "[.,[[[.,.],.],[[]]]]" r

let () =
  let keyword s =
    let* t = string s *> peek_token in
    match t with Some ' ' | None -> return () | _ -> fail "not keyword"
  in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let id = take_while1 is_alpha in
  let white = ( == ) ' ' in
  let r =
    skip_while white *> (keyword "let" *> return false <|> id *> return true)
  in
  assert_ok false "let" r;
  assert_ok true "   letter" r;
  assert_err "none taken" "" id

let () =
  let string = token '"' *> take_while (( <> ) '"') <* token '"' in
  assert_ok [] {|""|} string;
  assert_ok [ 'f'; 'o'; 'o' ] {|"foo"|} string
