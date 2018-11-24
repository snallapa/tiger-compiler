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

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s STRING COMMENT;
%%
<INITIAL,COMMENT>\n       => (lineNum := !lineNum + 1; linePos := yypos; continue());
<INITIAL,COMMENT>"/*"     => (YYBEGIN COMMENT; commentDepth := !commentDepth + 1; continue());
<COMMENT>"*/"             => (if (!commentDepth) = 1 then YYBEGIN INITIAL else YYBEGIN COMMENT;
                              commentDepth := !commentDepth - 1;
                              continue());
<COMMENT>.                => (continue());

<INITIAL>"\""             => (YYBEGIN STRING; inString := true; currentString := ""; continue());
<STRING>"\\n"             => (currentString := !currentString ^ "\n"; continue());
<STRING>"\\t"             => (currentString := !currentString ^ "\t"; continue());
<STRING>"\\"[0-9]{3}      => (currentString := !currentString ^
                              (String.str (case (Char.fromString yytext) of
                                                  SOME char => char
                                                  | NONE => ErrorMsg.impossible "invalid three digit string escape"));
                              continue());
<STRING>"\\\^"[A-Z]       => (currentString := !currentString ^
                              (String.str (case (Char.fromString yytext) of
                                            SOME char => char
                                            | NONE => ErrorMsg.impossible "invalid three digit string escape"));
                              continue());
<STRING>"\\\""            => (currentString := !currentString ^ "\""; continue());
<STRING>"\\\\"            => (currentString := !currentString ^ "\\"; continue());

<STRING>"\\f"[ \n\t]*"f\\"    => (continue());
<STRING>\\.               => (ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("illegal character in string at " ^ yytext); continue());
<STRING>\n                => (YYBEGIN INITIAL; ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("unfinished string"); continue());
<STRING>\"                => (YYBEGIN INITIAL; inString := false; Tokens.STRING(!currentString, yypos - (!linePos), !lineNum));
<STRING>.                 => (currentString := !currentString ^ yytext; continue());

<INITIAL>[\ \t\r]         => (continue());
<INITIAL>[0-9]+           => (Tokens.INT((foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext)), yypos - (!linePos), !lineNum));
<INITIAL>":="             => (Tokens.ASSIGN(yypos - (!linePos), !lineNum));
<INITIAL>"|"              => (Tokens.OR(yypos - (!linePos), !lineNum));
<INITIAL>"&"              => (Tokens.AND(yypos - (!linePos), !lineNum));
<INITIAL>">="             => (Tokens.GE(yypos - (!linePos), !lineNum));
<INITIAL>">"              => (Tokens.GT(yypos - (!linePos), !lineNum));
<INITIAL>"<="             => (Tokens.LE(yypos - (!linePos), !lineNum));
<INITIAL>"<"              => (Tokens.LT(yypos - (!linePos), !lineNum));
<INITIAL>"<>"             => (Tokens.NEQ(yypos - (!linePos), !lineNum));
<INITIAL>"="              => (Tokens.EQ(yypos - (!linePos), !lineNum));
<INITIAL>"/"              => (Tokens.DIVIDE(yypos - (!linePos), !lineNum));
<INITIAL>"*"              => (Tokens.TIMES(yypos - (!linePos), !lineNum));
<INITIAL>"-"              => (Tokens.MINUS(yypos - (!linePos), !lineNum));
<INITIAL>"+"              => (Tokens.PLUS(yypos - (!linePos), !lineNum));
<INITIAL>"."              => (Tokens.DOT(yypos - (!linePos), !lineNum));
<INITIAL>"{"              => (Tokens.LBRACE(yypos - (!linePos), !lineNum));
<INITIAL>"}"              => (Tokens.RBRACE(yypos - (!linePos), !lineNum));
<INITIAL>"["              => (Tokens.LBRACK(yypos - (!linePos), !lineNum));
<INITIAL>"]"              => (Tokens.RBRACK(yypos - (!linePos), !lineNum));
<INITIAL>"("              => (Tokens.LPAREN(yypos - (!linePos), !lineNum));
<INITIAL>")"              => (Tokens.RPAREN(yypos - (!linePos), !lineNum));
<INITIAL>";"              => (Tokens.SEMICOLON(yypos - (!linePos), !lineNum));
<INITIAL>":"              => (Tokens.COLON(yypos - (!linePos), !lineNum));
<INITIAL>","              => (Tokens.COMMA(yypos - (!linePos), !lineNum));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]*   => (case (HashTable.find keywordHashtable yytext) of
                                      SOME token => (token (yypos - (!linePos), !lineNum))
                                      | NONE => (Tokens.ID (yytext, yypos - (!linePos), !lineNum)));

<INITIAL>.                => (ErrorMsg.errorLine (yypos - (!linePos)) (!lineNum) ("illegal character " ^ yytext); continue());
