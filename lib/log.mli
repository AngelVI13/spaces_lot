(* NOTE: here 'a refers to the arg types, then follows the formatter type and
   the last unit refers to the return value (don't know what the first unit
   stands for *)
val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
val info : ('a, Format.formatter, unit, unit) format4 -> 'a
val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ('a, Format.formatter, unit, unit) format4 -> 'a
