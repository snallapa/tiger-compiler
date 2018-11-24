functor TigerLexFun(structure Tokens: Tiger_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val currentString = ref ""
val inString = ref false

fun eof() = (
  if (!commentDepth <> 0) then ErrorMsg.errorLine 0 (!lineNum) ("a program cannot end in a comment") else ();
  if (!inString) then ErrorMsg.errorLine 0 (!lineNum) ("a program cannot end in a string") else ();

  if !ErrorMsg.anyErrors then (raise Fail "Errors found in lexing, please fix before continuing") else ();

  Tokens.EOF(0,0))

val keywordHashtable : (string, pos * pos -> lexresult) HashTable.hash_table =
  HashTable.mkTable (HashString.hashString, op=) (17, Fail "Keyword not found")

val _ = HashTable.insert keywordHashtable ("type", Tokens.TYPE)
val _ = HashTable.insert keywordHashtable ("var", Tokens.VAR)
val _ = HashTable.insert keywordHashtable ("function", Tokens.FUNCTION)
val _ = HashTable.insert keywordHashtable ("break", Tokens.BREAK)
val _ = HashTable.insert keywordHashtable ("of", Tokens.OF)
val _ = HashTable.insert keywordHashtable ("end", Tokens.END)
val _ = HashTable.insert keywordHashtable ("in", Tokens.IN)
val _ = HashTable.insert keywordHashtable ("nil", Tokens.NIL)
val _ = HashTable.insert keywordHashtable ("let", Tokens.LET)
val _ = HashTable.insert keywordHashtable ("do", Tokens.DO)
val _ = HashTable.insert keywordHashtable ("to", Tokens.TO)
val _ = HashTable.insert keywordHashtable ("for", Tokens.FOR)
val _ = HashTable.insert keywordHashtable ("while", Tokens.WHILE)
val _ = HashTable.insert keywordHashtable ("else", Tokens.ELSE)
val _ = HashTable.insert keywordHashtable ("then", Tokens.THEN)
val _ = HashTable.insert keywordHashtable ("if", Tokens.IF)
val _ =  HashTable.insert keywordHashtable ("array", Tokens.ARRAY)



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum + 1; linePos := yypos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; commentDepth := !commentDepth + 1; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (!commentDepth) = 1 then YYBEGIN INITIAL else YYBEGIN COMMENT;
                              commentDepth := !commentDepth - 1;
                              continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; inString := true; currentString := ""; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (currentString := !currentString ^ "\n"; continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (currentString := !currentString ^ "\t"; continue()))
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (currentString := !currentString ^
                              (String.str (case (Char.fromString yytext) of
                                                  SOME char => char
                                                  | NONE => ErrorMsg.impossible "invalid three digit string escape"));
                              continue())
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (currentString := !currentString ^
                              (String.str (case (Char.fromString yytext) of
                                            SOME char => char
                                            | NONE => ErrorMsg.impossible "invalid three digit string escape"));
                              continue())
      end
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (currentString := !currentString ^ "\""; continue()))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (currentString := !currentString ^ "\\"; continue()))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("illegal character in string at " ^ yytext); continue())
      end
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("unfinished string"); continue()))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; inString := false; Tokens.STRING(!currentString, yypos - (!linePos), !lineNum)))
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (currentString := !currentString ^ yytext; continue())
      end
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT((foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext)), yypos - (!linePos), !lineNum))
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(yypos - (!linePos), !lineNum)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos - (!linePos), !lineNum)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos - (!linePos), !lineNum)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(yypos - (!linePos), !lineNum)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos - (!linePos), !lineNum)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(yypos - (!linePos), !lineNum)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos - (!linePos), !lineNum)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos - (!linePos), !lineNum)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos - (!linePos), !lineNum)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos - (!linePos), !lineNum)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos - (!linePos), !lineNum)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos - (!linePos), !lineNum)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos - (!linePos), !lineNum)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos - (!linePos), !lineNum)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos - (!linePos), !lineNum)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos - (!linePos), !lineNum)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK(yypos - (!linePos), !lineNum)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK(yypos - (!linePos), !lineNum)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos - (!linePos), !lineNum)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos - (!linePos), !lineNum)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos - (!linePos), !lineNum)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos - (!linePos), !lineNum)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos - (!linePos), !lineNum)))
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case (HashTable.find keywordHashtable yytext) of
                                      SOME token => (token (yypos - (!linePos), !lineNum))
                                      | NONE => (Tokens.ID (yytext, yypos - (!linePos), !lineNum)))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("illegal character " ^ yytext); continue())
      end
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction41(strm, yyNO_MATCH)
                      else yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction41(strm, yyNO_MATCH)
                  else yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp = #"`"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction41(strm, yyNO_MATCH)
                      else yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction41(strm, yyNO_MATCH)
                  else yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp = #"`"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ51(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ52(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ54(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ53(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ56(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"0"
              then yyAction17(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ56(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ56(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"0"
              then yyAction17(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ56(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ25(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ37(strm', lastMatch)
            else if inp < #"."
              then if inp = #"#"
                  then yyQ27(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"\r"
                      then yyQ28(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ22(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ28(strm', lastMatch)
                              else yyQ27(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ27(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ28(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #")"
                  then yyQ32(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"'"
                      then yyQ27(strm', lastMatch)
                    else if inp < #"'"
                      then if inp = #"&"
                          then yyQ30(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = #","
                  then yyQ35(strm', lastMatch)
                else if inp < #","
                  then if inp = #"*"
                      then yyQ33(strm', lastMatch)
                      else yyQ34(strm', lastMatch)
                  else yyQ36(strm', lastMatch)
            else if inp = #"["
              then yyQ46(strm', lastMatch)
            else if inp < #"["
              then if inp = #"<"
                  then yyQ42(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ40(strm', lastMatch)
                    else if inp < #":"
                      then if inp = #"/"
                          then yyQ38(strm', lastMatch)
                          else yyQ39(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = #"?"
                  then yyQ27(strm', lastMatch)
                else if inp < #"?"
                  then if inp = #"="
                      then yyQ43(strm', lastMatch)
                      else yyQ44(strm', lastMatch)
                else if inp <= #"@"
                  then yyQ27(strm', lastMatch)
                  else yyQ45(strm', lastMatch)
            else if inp = #"{"
              then yyQ48(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"^"
                  then yyQ27(strm', lastMatch)
                else if inp < #"^"
                  then if inp = #"\\"
                      then yyQ27(strm', lastMatch)
                      else yyQ47(strm', lastMatch)
                else if inp <= #"`"
                  then yyQ27(strm', lastMatch)
                  else yyQ45(strm', lastMatch)
            else if inp = #"}"
              then yyQ50(strm', lastMatch)
            else if inp = #"|"
              then yyQ49(strm', lastMatch)
              else yyQ27(strm', lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ25(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ26(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ23(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ22(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = #"/"
              then yyQ24(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ17(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ15(strm', lastMatch)
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ15(strm', lastMatch)
                else if inp < #"\t"
                  then yystuck(lastMatch)
                else if inp <= #"\n"
                  then yyQ15(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"f"
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ15(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ15(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ15(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ16(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ18(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then yyAction12(strm, yyNO_MATCH)
            else if inp <= #"Z"
              then yyQ18(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ20(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ20(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ19(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"0"
              then yyAction12(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ19(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"#"
                  then yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\v"
                      then yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\n"
                          then yyAction15(strm, yyNO_MATCH)
                          else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ8(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = #":"
                  then yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #":"
                  then if inp <= #"/"
                      then yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyQ9(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ10(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"n"
              then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ12(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"f"
                  then if inp = #"^"
                      then yyQ11(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"t"
              then yyQ14(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyQ7(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ5(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ4(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"\\"
              then yyQ6(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
