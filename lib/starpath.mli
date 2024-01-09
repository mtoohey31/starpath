module type Token = sig
  type t

  val string_of_token : t -> string
end

module type Pos = sig
  type t
  type pos0_arg

  val pos0 : pos0_arg -> t
  val compare : t -> t -> int
  val string_of_pos : t -> string
end

module type Combinators = sig
  type token
  type pos
  type pos0_arg
  type parse_error = { pos : pos; expected : string list; actual : string }

  val string_of_parse_error : parse_error -> string

  type 'a t

  val parse :
    pos0_arg -> (pos * token) Seq.t -> 'a t -> ('a, parse_error) result

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( @> ) : 'a t -> 'b t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val eof : unit t
  val fail : parse_error -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val optional : 'a t -> 'a option t
  val optional_or : 'a t -> default:'a -> 'a t
  val optional_or_else : 'a t -> default_f:(unit -> 'a) -> 'a t
  val peek : token option t
  val pos : 'a t -> (pos * 'a) t
  val repeat : 'a t -> 'a list t
  val repeat1 : 'a t -> 'a list t
  val return : 'a -> 'a t
  val return_at : pos -> 'a -> 'a t
  val satisfy : expected:string list -> (token -> bool) -> token t
  val satisfy_map : expected:string list -> (token -> 'a option) -> 'a t
  val sep_by1 : _ t -> 'a t -> 'a list t
  val sep_by : _ t -> 'a t -> 'a list t
  val skip_while : (token -> bool) -> unit t
  val take_while1 : expected:string list -> (token -> bool) -> token list t
  val take_while : (token -> bool) -> token list t
  val token_not : token -> token t
  val token : token -> token t
end

module Make (Token : Token) (Pos : Pos) :
  Combinators
    with type token = Token.t
    with type pos = Pos.t
    with type pos0_arg = Pos.pos0_arg

module CharToken : Token with type t = char

type string_pos = { row : int; col : int }

module StringPos : Pos with type t = string_pos with type pos0_arg = unit

type file_pos = { path : string; row : int; col : int }

module FilePos : Pos with type t = file_pos with type pos0_arg = string

module type CharCombinators = sig
  include Combinators with type token = CharToken.t

  val string : string -> string t
end

module MakeChar (Pos : Pos) :
  CharCombinators with type pos = Pos.t with type pos0_arg = Pos.pos0_arg

module StringCombinators : sig
  include
    CharCombinators
      with type pos = StringPos.t
      with type pos0_arg = StringPos.pos0_arg

  val parse_string : string -> 'a t -> ('a, parse_error) result
end

module FileCombinators : sig
  include
    CharCombinators
      with type pos = FilePos.t
      with type pos0_arg = FilePos.pos0_arg

  val parse_file : string -> 'a t -> ('a, parse_error) result
end
