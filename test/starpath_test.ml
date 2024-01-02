open Starpath.StringCombinators
open OUnit2

let assert_parses expected s r = assert_equal expected (parse_string s r)
let () = assert_parses (Ok 'a') "a" (token 'a')
let () = assert_parses (Error "eof") "" (token 'a')
let () = assert_parses (Error "unexpected") "?" (token 'a')
let () = assert_parses (Ok '?') "?" (satisfy (fun _ -> true))
let () = assert_parses (Error "expected: eof") "ab" (token 'a')
let () = assert_parses (Ok "foo") "foo" (string "foo")
let () = assert_parses (Error "expected: bar") "foo" (string "bar")

let () =
  let r = string "foo" *> token '\n' *> string "bar" in
  assert_parses (Ok "bar") "foo\nbar" r

let () =
  let r =
    string "foo" >>| String.length
    <|> (string "bazzes" >>| String.length)
    <* string "baz"
  in
  assert_parses (Ok 6) "bazzesbaz" r
