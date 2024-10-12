%token <string> TAG
%token <string> FIELD
%token <int> NUM
%token <string> MODIFIER
%token <string> TYPE

%token ID CLASS SEL VOID STRING POINTER BLOCK
%token BOOL UCHAR INT UINT SHORT USHORT LONG ULONG LLONG ULLONG
%token FLOAT DOUBLE LDOUBLE
%token L_BRACE R_BRACE L_BRACKET R_BRACKET L_PAREN R_PAREN
%token UNKNOWN EOF BITFIELD
// %token EQUAL

%start <Objc_type.t option> nonmeth
%start <Objc_type.t option> meth
%%

nonmeth:
  | x = typ { Some x }
  | EOF { None };

typ:
  | ID { `Id }
  | CLASS { `Class }
  | SEL { `Sel }
  | VOID { `Void }
  | STRING { `String }
  | BOOL { `Bool }
  | UCHAR { `Uchar }
  | INT { `Int }
  | UINT { `Uint }
  | SHORT { `Short }
  | USHORT { `Ushort }
  | LONG { `Long }
  | ULONG { `Ulong }
  | LLONG { `Llong }
  | ULLONG { `Ullong }
  | FLOAT { `Float }
  | DOUBLE { `Double }
  | LDOUBLE { `Ldouble }
  | BLOCK { `Block }
  | BITFIELD; n = NUM { `Bitfield n }
  | UNKNOWN { `Unknown }
  | x = MODIFIER; t = typ { `Modifier (x, t) }
  | x = TYPE { `Type x }
  | POINTER; x = typ { `Pointer x }
  | L_BRACKET; n = NUM; t = typ; R_BRACKET { `Array (n, t) }
  | L_BRACE; n = option(tag); t = struct_fields; R_BRACE { `Struct (n, t) }
  | L_PAREN; n = option(tag); t = struct_fields; R_PAREN { `Union (n, t) };

struct_fields:
  | (* empty *) { [] }
  | k = option(field); v = typ; s = struct_fields { (k, v) :: s };

tag: TAG { $1 };
field: FIELD { $1 };

meth:
  | x = meth_sig { Some x }
  | EOF { None };

meth_sig: x = meth_arg; xs = meth_arg+; EOF { `Method (xs, x) };
meth_arg: x = typ; NUM { x };
