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
let id = ['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']*
let numberLetter = ['0'-'9']+['A'-'Z' 'a'-'z']

(* add more named regexps here *)




(* an entrypoint with a few starting regexps *)
rule token = parse
| [' ' '\t']     { token lexbuf }     (* skip blanks *)
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
| '^'                 { CARET }
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
| "/*"                { comment 0 lexbuf}  
| id as a             { ID (a) }
| digits as i         { 
                        try INT (int_of_string i)
                        with
                        | _ -> error lexbuf ("Invalid integer") 
                        }
| numberLetter as i   { error lexbuf ("Cannot have letters after a number '" ^ i ^ "'") }

(* default error handling *)
| _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }

and comment commentLevel = parse
| "/*"                { comment (commentLevel+1) lexbuf}
| "*/"                { (if commentLevel = 0 then token else comment (commentLevel-1)) lexbuf}
| '\n'                { Lexing.new_line lexbuf; comment commentLevel lexbuf }
| _                   { comment (commentLevel) lexbuf}

and formFeed = parse
| "\\"                { stringToken "" lexbuf}
| '\n'                { Lexing.new_line lexbuf; formFeed lexbuf }
| '\t'                { formFeed lexbuf }
| ' '                 { formFeed lexbuf }
| eof                 {  error lexbuf ("End of file") }
| _                   {  error lexbuf ("FormFeed never ended") }

and asciiCode = parse
| _ as c              {INT (Char.code c)}

and stringToken accumulator = parse
| '\"'                {accumulator}
| '\\'                {accumulator ^ escapeSequence lexbuf}
| [' '-'~'] as c      {stringToken (accumulator ^ (String.make 1 c)) lexbuf}
| eof                 {  error lexbuf ("End of file") }
| _ as c              {error lexbuf ("Illegal character " ^ (String.make 1 c)) }

and escapeSequence = parse
| 'n'                 { stringToken "\n" lexbuf }
| 't'                 { stringToken "\t" lexbuf }
| '\"'                { stringToken "\"" lexbuf }
| ' '                 { formFeed lexbuf }
| '\t'                { formFeed lexbuf }
| '\n'                { Lexing.new_line lexbuf; formFeed lexbuf }
| '^'                 { charCode lexbuf}
| ['0'-'9'] as d      { let s = (String.make 1 d) ^ (asciiSign 2 lexbuf) in
                        if int_of_string s > 255 then error lexbuf ("Argument out of bounds")
                        else stringToken ( String.make 1 (Char.chr (int_of_string s))) lexbuf 
                        }
| '\\'                { stringToken "\\" lexbuf }
| eof                 { error lexbuf ("End of file") }
| _ as e              { error lexbuf ("Invalid character in escape sequence \\'" ^ (String.make 1 e) ^ "'") }

and charCode = parse
| ['@'-'_'] as c      { stringToken (String.make 1 (Char.chr (Char.code c - 64))) lexbuf }
| ['a'-'z'] as c      { stringToken (String.make 1 (Char.chr (Char.code c - 64 - 32))) lexbuf }
| '?' as c            { stringToken (String.make 1 (Char.chr (Char.code c + 64))) lexbuf }
| eof                 { error lexbuf ("End of file.") }
| _ as e              { error lexbuf ("Illigal character after ^" ^ (String.make 1 e)) }

and asciiSign counter = parse
| ['0'-'9'] as d      { if counter > 1 then (String.make 1 d) ^ asciiSign (counter - 1) lexbuf else String.make 1 d}
| _ as e              { error lexbuf ("Invalid character in \\ddd'" ^ (String.make 1 e) ^ "'") }

