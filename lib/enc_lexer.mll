{
  open Enc_parser
  exception SyntaxError of string

  let annon_struct_inner = ref 0
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']

let num = digit+
let tag = alpha (alpha | digit)+

rule token = parse
  | '@' { ID }
  | '#' { CLASS }
  | ':' { SEL }
  | 'v' { VOID }
  | '*' { STRING }
  | 'c' { BOOL }
  | 'C' { UCHAR }
  | 'i' { INT }
  | 'I' { UINT }
  | 's' { SHORT }
  | 'S' { USHORT }
  | 'l' { LONG }
  | 'L' { ULONG }
  | 'q' { LLONG }
  | 'Q' { ULLONG }
  | 'f' { FLOAT }
  | 'd' { DOUBLE }
  | 'D' { LDOUBLE }
  | 'b' { BITFIELD }
  | 'B' { BOOL }
  | '^' { POINTER }
  | "@?" { BLOCK }
  | '?' { UNKNOWN }
  | '{' { L_BRACE }
  | '}' { R_BRACE }
  | '[' { L_BRACKET }
  | ']' { R_BRACKET }
  | '(' { L_PAREN }
  | ')' { R_PAREN }
  (* | '=' { EQUAL } *)
  | "{?}" { VOID }
  | "{?=" { anon_struct (Buffer.create 17) lexbuf }
  | "(?=" _* ')' { VOID }
  | '{' (tag as n) '='? '}' | '(' (tag as n) '='? ')' { TYPE n }
  | 'j' | 'A' | 'r' | 'n' | 'N' | 'o' | 'O' | 'R' | 'V' | '+'
    { MODIFIER (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "^{__va_list_tag=" _* '}' { TYPE "va_list_tag" }
  | num as n { NUM (int_of_string n) }
  | (tag as n) '=' { TAG n }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected: " ^ Lexing.lexeme lexbuf)) }

and anon_struct buf = shortest
  | '}' { if Int.equal !annon_struct_inner 0 then VOID else (decr annon_struct_inner; anon_struct buf lexbuf) }
  | '{' { incr annon_struct_inner; anon_struct buf lexbuf }
  | [^ '}']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); anon_struct buf lexbuf }
  | eof { raise (SyntaxError "Unterminated struct") }
  | _ { raise (SyntaxError ("Illegal struct char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = shortest
  | '"' { FIELD (Buffer.contents buf) }
  | [^ '"']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof { raise (SyntaxError "Unterminated string") }
  | _ { raise (SyntaxError ("Illegal string char: " ^ Lexing.lexeme lexbuf)) }
