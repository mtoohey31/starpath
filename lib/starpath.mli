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
  type parse_error = { pos : pos; expected : string; actual : string }

  val string_of_parse_error : parse_error -> string

  type 'res t

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

module Make (Token : TokenType) :
  CombinatorsType with type token = Token.t with type pos = Token.pos

type char_pos = { row : int; col : int }

module CharToken : TokenType with type t = char with type pos = char_pos

module StringCombinators : sig
  include module type of Make (CharToken)

  val string : string -> string t
  val parse_string : string -> 'a t -> ('a, parse_error) result
end
