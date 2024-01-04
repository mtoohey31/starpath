open Starpath.StringCombinators
open OUnit2

let assert_ok expected s r = assert_equal (Ok expected) (parse_string s r)

let assert_err expected s r =
  match parse_string s r with
  | Ok _ -> assert_failure "unexpectedly ok"
  | Error pe ->
      assert_equal ~printer:(fun s -> s) expected (string_of_parse_error pe)

let () = assert_ok 'a' "a" (token 'a')
let () = assert_err "1:1: expected 'a', found EOF" "" (token 'a')
let () = assert_err "1:1: expected not 'a', found 'a'" "a" (token_not 'a')
let () = assert_ok '?' "?" (satisfy ~expected:"foo" (fun _ -> true))
let () = assert_err "1:2: expected EOF, found 'b'" "ab" (token 'a')
let () = assert_ok "foo" "foo" (string "foo")

let () =
  assert_err {|1:6: expected "foobaz", found "foobar"|} "foobar"
    (string "foobaz")

let () =
  assert_err {|1:3: expected "foobaz", found "foo" EOF|} "foo" (string "foobaz")

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

let () =
  let pe = { pos = { row = 7; col = 23 }; actual = "act"; expected = "exp" } in
  assert_err (string_of_parse_error pe) "" (fail pe)

let () =
  let r =
    fix (fun term ->
        token '.'
        >>@ (fun _ { col; _ } -> col)
        <|> (token '[' *> (sep_by (token ',') term >>| List.fold_left ( + ) 0)
            <* token ']'))
  in
  assert_ok 30 "[.,[[[.,.],.],[[]]]]" r

let () =
  let r =
    let@ _, { col; _ } = token '[' @> string "hi" in
    col
  in
  assert_ok 1 "[hi" r

let () =
  let keyword s =
    let= t = string s *> peek_token in
    match t with
    | Some ' ' | None -> return ()
    | _ -> fail { pos = { row = 0; col = 0 }; expected = "foo"; actual = "bar" }
  in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let id = take_while1 ~expected:"[a-zA-Z]+" is_alpha in
  let white = ( == ) ' ' in
  let r =
    skip_while white *> (keyword "let" *> return false <|> id *> return true)
  in
  assert_ok false "let" r;
  assert_ok true "   letter" r;
  assert_err "1:1: expected [a-zA-Z]+, found EOF" "" id

let () =
  let string = token '"' *> take_while (( <> ) '"') <* token '"' in
  assert_ok [] {|""|} string;
  assert_ok [ 'f'; 'o'; 'o' ] {|"foo"|} string

let () =
  let r = string "hi" *> peek <* take_while (fun _ -> true) in
  assert_ok (Some (' ', ({ row = 1; col = 3 } : pos))) "hi there" r;
  assert_ok None "hi" r

let () =
  let r = string "hi" *> peek_pos <* take_while (fun _ -> true) in
  assert_ok (Some ({ row = 1; col = 3 } : pos)) "hi there" r;
  assert_ok None "hi" r
