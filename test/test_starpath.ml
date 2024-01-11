open Starpath
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
let () = assert_ok '?' "?" (satisfy ~expected:[ "foo" ] (fun _ -> true))
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
  let pe =
    { pos = { row = 7; col = 23 }; actual = "act"; expected = [ "exp" ] }
  in
  assert_err (string_of_parse_error pe) "" (fail pe)

let () =
  let r =
    fix (fun term ->
        token '.' |> pos
        >>| (fun (({ col; _ } : string_pos), _) -> col)
        <|> (token '[' *> (sep_by (token ',') term >>| List.fold_left ( + ) 0)
            <* token ']'))
  in
  assert_ok 30 "[.,[[[.,.],.],[[]]]]" r

let () =
  let r =
    let+ ({ col; _ } : string_pos), _ = token '[' @> string "hi" |> pos in
    col
  in
  assert_ok 1 "[hi" r

let () =
  let keyword s =
    let* t = string s *> peek in
    match t with
    | Some ' ' | None -> return ()
    | _ ->
        fail
          { pos = { row = 0; col = 0 }; expected = [ "foo" ]; actual = "bar" }
  in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let id = take_while1 ~expected:[ "[a-zA-Z]+" ] is_alpha in
  let white = ( == ) ' ' in
  let r =
    skip_while white *> (keyword "let" *> return false <|> id *> return true)
    <* skip_while white
  in
  assert_ok false "let" r;
  assert_ok false "let   " r;
  assert_ok true "   letter" r;
  assert_err "1:1: expected [a-zA-Z]+, found EOF" "" id

let () =
  let string = token '"' *> take_while (( <> ) '"') <* token '"' in
  assert_ok [] {|""|} string;
  assert_ok [ 'f'; 'o'; 'o' ] {|"foo"|} string

let () =
  let r =
    string "foo"
    <|> string "bar" *> string "foo" *> token '\n' *> string "baz"
    <|> string "baz" <|> string "quux"
  in
  assert_err {|2:1: expected "baz", found "q"|} "barfoo\nquux" r;
  assert_err {|1:1: expected "bar" | "baz" | "foo" | "quux", found "o"|} "other"
    r

let () =
  let r1 = repeat (token 'a') in
  let r2 = repeat1 (token 'a') in
  assert_ok [] "" r1;
  assert_ok [ 'a'; 'a'; 'a' ] "aaa" r1;
  assert_err "1:1: expected 'a', found EOF" "" r2;
  assert_ok [ 'a'; 'a'; 'a' ] "aaa" r2

let () =
  let r =
    satisfy_map ~expected:[ "<NUM>" ] (function
      | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
      | _ -> None)
  in
  assert_ok 5 "5" r;
  assert_err "1:1: expected <NUM>, found EOF" "" r;
  assert_err "1:1: expected <NUM>, found 'q'" "q" r

let () =
  let r =
    token '['
    @> optional_or_else (token 'x' <|> token ' ') ~default_f:(fun () -> ' ')
    <* token ']'
  in
  assert_ok ' ' "[ ]" r;
  assert_ok ' ' "[]" r;
  assert_ok 'x' "[x]" r

let () =
  assert_equal
    (FilePos.compare
       { path = "a.zt"; row = 1; col = 1 }
       { path = "b.zt"; row = 1; col = 1 })
    (String.compare "a.zt" "b.zt");
  assert_equal
    (FilePos.compare
       { path = "test.zt"; row = 2; col = 1 }
       { path = "test.zt"; row = 1; col = 1 })
    (compare 2 1)

let () =
  assert_equal "test.zt:2:7"
    (FilePos.string_of_pos { path = "test.zt"; row = 2; col = 7 })

let () =
  let module Parser (Combinators : Starpath.CharCombinators) = struct
    open Combinators

    let r =
      token 'a' *> (token 'o' <|> token 'p' <|> token 'b')
      <* token 'c' <* token '\n'
  end in
  let () =
    let open Parser (Starpath.StringCombinators) in
    let open Starpath.StringCombinators in
    assert_equal (parse_string "abc\n" r) (Ok 'b')
  in
  let () =
    let open Parser (Starpath.FileCombinators) in
    let open Starpath.FileCombinators in
    assert_equal (parse_file "testdata/test1.txt" r) (Ok 'b')
  in
  ()
