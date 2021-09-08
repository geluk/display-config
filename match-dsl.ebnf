

expression           = literal
                     | operation
                     | delimited expression;

delimited expression = "(", expression, ")";

(* operations *)
operation            = binary operation
                     | unary operation;

unary operation      = unary operator, expr;
binary operation     = expression, binary operator, expression;

(* operators *)
unary operator       = "not";
binary operator      = "and" | "or" | ">" | "<" | "=" | ">=" | "<=";

(* identifiers *)
identifier           = lowercase { lowercase | digit };

(* literals *)
literal              = number
                     | boolean
                     | string
                     | resolution;

number               = digit excluding zero, { digit };
boolean              = "true" | "false";
string               = '"', { text }, '"';
resolution           = number, "x", number, [ ":", number ];

(* definitions *)
text                 = digit | lowercase | uppercase
digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
digit                = "0" | digit excluding zero;

lowercase            = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i"
                     | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
                     | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z";

uppercase            = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
                     | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
                     | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";