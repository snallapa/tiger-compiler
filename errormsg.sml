signature ERRORMSG =
sig
    val anyErrors : bool ref
    val lineNum : int ref
    val linePos : int ref
    val error : int * string -> unit
    val errorLine: int -> int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
    val fileName: string ref
end

structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val lineNum = ref 1
  val linePos = ref 1
  val fileName = ref ""

  fun reset() = (anyErrors:=false;
		 lineNum:=1;
		 linePos:=1)

  exception Error

  fun error (pos, (msg:string)) = (
    anyErrors := true;
    print ((Int.toString (pos)) ^ " ");
    print msg;
    print "\n")

  fun errorLine pos line (msg:string) = (
    anyErrors := true;
    print ((Int.toString (line)) ^ ":" ^ (Int.toString (pos)) ^ " ");
    print msg;
    print "\n")

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

end  (* structure ErrorMsg *)
