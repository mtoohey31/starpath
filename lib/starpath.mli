module type TokenType = sig
  type t
  type pos

  val pos0 : pos
  val advance : pos -> t -> pos
end

module type CombinatorsType = sig
  type token
  type 'a t

  val parse : token list -> 'a t -> ('a, string) result
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

module Make (Token : TokenType) : CombinatorsType with type token = Token.t
module CharToken : TokenType with type t = char

module StringCombinators : sig
  include module type of Make (CharToken)

  val string : string -> string t
  val parse_string : string -> 'a t -> ('a, string) result
end
