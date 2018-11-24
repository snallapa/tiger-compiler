signature FRAME =
sig type frame
  datatype access = InFrame of int | InReg of Temp.temp

  type register = string

  val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  val externalCall: string * Tree.exp list -> Tree.exp

  val getTemp: Temp.temp -> register

  val FP : Temp.temp
  val RV : Temp.temp
  val calldefs : Temp.temp list
  val argregs : Temp.temp list
  val precolored: Temp.temp list
  val wordSize : int
  val exp : access -> Tree.exp -> Tree.exp


  val string: Tree.label * string -> string
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> { prolog: string, body: Assem.instr list, epilog: string }
  datatype frag = PROC of { body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end
