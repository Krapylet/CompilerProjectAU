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

let digits = ['0'-'9']+
let stringLit = ['"'][ 'a'-'z' 'A'-'Z' '0'-'9' '_' '\n' ' ' '\t' '?' ':' ';' ',' '.' '=' '!' '[' ']' '(' ')' '{' '}' '^' '\\']*['"']
let id = ['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* add more named regexps here *)




(* an entrypoint with a few starting regexps *)
rule token = parse
  [' ' '\t']     { token lexbuf }     (* skip blanks *)
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| eof                 { EOF }
| ','                 { COMMA }
| ';'                 { SEMICOLON }
| ":="                { ASSIGN }
| "array"             { ARRAY }
| "if"                { IF }
| '='                 { EQ }
| ':'                 { COLON }
| '{'                 { LBRACE }
| '}'                 { RBRACE }
| '['                 { LBRACK }
| ']'                 { RBRACK }
| '('                 { LPAREN }
| ')'                 { RPAREN }
| '.'                 { DOT }
| '/'                 { DIVIDE }
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
| '\"'                { let pos = lexbuf.lex_start_p in
                        let s = stringToken "" lexbuf in
                          lexbuf.lex_start_p <- pos;
                          STRING s} 
| "\\^"               { asciiCode lexbuf}
| "/*"                { comment 0 lexbuf}  

| id as a             { ID (a) }
| digits as i         { if ((int_of_string i) <= Int.max_int) then (INT (int_of_string i)) else (error lexbuf ("Invalid integer"))  }

(*
| stringLit as s      { STRING (match String.length s with | 0 | 1 | 2 -> "" | l -> String.sub s 1 (l-2))}
 *)

(* default error handling *)
| _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }

and comment commentLevel = parse
| "/*"                { comment (commentLevel+1) lexbuf}
| "*/"                { (if commentLevel = 0 then token else comment (commentLevel-1)) lexbuf}
| '\n'                { Lexing.new_line lexbuf; comment commentLevel lexbuf }
| _                   { comment (commentLevel) lexbuf}

and asciiCode = parse
| _ as c              {INT (Char.code c)}

and stringToken accumulator = parse
| '\"'                {accumulator}
| '\\'                {escapeSequence lexbuf}
|_ as c               {stringToken (accumulator ^ (String.make 1 c)) lexbuf}

and escapeSequence = parse
| 'n'                 {stringToken "\n" lexbuf}
