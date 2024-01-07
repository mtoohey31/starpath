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

module Make (Token : TokenType) : CombinatorsType with type token = Token.t
module CharToken : TokenType with type t = char

type string_pos = { row : int; col : int }

val string_pos0 : string_pos
val compare_string_pos : string_pos -> string_pos -> int
val string_of_string_pos : string_pos -> string

type file_pos = { path : string; row : int; col : int }

val file_pos0 : string -> file_pos
val compare_file_pos : file_pos -> file_pos -> int
val string_of_file_pos : file_pos -> string

module StringCombinators : sig
  include module type of Make (CharToken)

  val string : string -> ('p, string) t

  val parse_string :
    string -> (string_pos, 'a) t -> ('a, string_pos parse_error) result

  val parse_file :
    string -> (file_pos, 'a) t -> ('a, file_pos parse_error) result
end
