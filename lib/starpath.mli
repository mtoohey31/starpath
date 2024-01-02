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

module Make (Token : TokenType) : CombinatorsType with type token = Token.t
module CharToken : TokenType with type t = char

module StringCombinators : sig
  include module type of Make (CharToken)

  val string : string -> string t
  val parse_string : string -> 'a t -> ('a, string) result
end
