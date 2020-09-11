(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

{
  open Tigerparser.Parser  
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

let digits=['0'-'9']+
let stringLit=[\"\""][a-z0-9A-Z \| "\\\\""\"" \| ' ' \| \n \| \t \| \^c \| \ddd \| "\\\\" \| \f___f\]* ["\""]
let id =[a-z A-Z]+[a-z A-Z _]*
let comment = [/*]stringLit[*/]

(* add more named regexps here *)

(* an entrypoint with a few starting regexps *)
rule token = parse
  [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
| eof                 { EOF }
| ','                 { COMMA }
| ';'                 { SEMICOLON }
| ":="                { ASSIGN }
| "array"             { ARRAY }
| "if"                { IF }
| digits as i         { INT (int_of_string i) }
| stringLit as s      { STRING (String.sub s 1 (String.length s - 2)) }
| id as a             { ID a }
| comment as c        { }

(* add your regexps here *)
| '='                 { EQ }
| ':'                 { COLON }
| '{'                 { LBRACE }
| '}'                 { RBRACE }
| '['                 { LBRACK }
| ']'                 { RBRACK }
| '('                 { LPAREN }
| ')'                 { RPAREN }
| '.'                 { DOT }
| '/'                 { SLASH }
| '+'                 { PLUS }
| '-'                 { MINUS }
| '*'                 { TIMES }
| '<'                 { LT }
| '>'                 { GT }
| "<>"                { NEQ }
| ">="                { GE }
| "<="                { LE }
| '&'                 { AND }
| '|'                 { OR }
| "while"             { WHILE }
| "for"               { FOR }
| "to"                { TO }
| "break"             { BREAK }
| "let"               { LET }
| "in"                { IN }
| "end"               { END }
| "function"          { FUNCTION }
| "var"               { VAR }
| "type"              { TYPE }
| "then"              { THEN }
| "else"              { ELSE }
| "do"                { DO }
| "of"                { OF }
| "nil"               { NIL }


(* default error handling *)
| _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }
