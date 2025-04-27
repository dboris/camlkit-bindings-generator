type t =
  [ `Id
  | `Class
  | `Sel
  | `Void
  | `String
  | `Bool
  | `Uchar
  | `Int
  | `Uint
  | `Short
  | `Ushort
  | `Long
  | `Ulong
  | `Llong
  | `Ullong
  | `Float
  | `Double
  | `Ldouble
  | `Block
  | `Unknown
  | `Bitfield of int
  | `Pointer of t
  | `Type of string
  | `Modifier of string * t
  | `Struct of string option * (string option * t) list
  | `Union of string option * (string option * t) list
  | `Array of int * t
  | `Method of t list * t ]
