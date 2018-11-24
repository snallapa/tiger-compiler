functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open Symbol

fun combineDecs (decs) = (
  case decs
    of nil                    => nil
     | Absyn.TypeDec(first) :: nil           => Absyn.TypeDec(first) :: nil
     | Absyn.TypeDec(list1) :: Absyn.TypeDec(list2) :: nil => Absyn.TypeDec(list2 @ list1) :: nil
     | Absyn.TypeDec(list1) :: Absyn.TypeDec(list2):: rest => combineDecs(Absyn.TypeDec(list2 @ list1) :: rest)
     | Absyn.FunctionDec(first) :: nil           => Absyn.FunctionDec(first) :: nil
     | Absyn.FunctionDec(list1) :: Absyn.FunctionDec(list2) :: nil => Absyn.FunctionDec(list2 @ list1) :: nil
     | Absyn.FunctionDec(list1) :: Absyn.FunctionDec(list2):: rest => combineDecs(Absyn.FunctionDec(list2 @ list1) :: rest)
     | first :: nil => first :: nil
     | first :: rest => first :: combineDecs(rest)
)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\009\000\039\000\016\000\012\000\029\000\011\000\032\000\010\000\
\\033\000\009\000\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\033\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\078\000\000\000\
\\001\000\002\000\079\000\000\000\
\\001\000\002\000\080\000\000\000\
\\001\000\002\000\104\000\012\000\103\000\028\000\102\000\000\000\
\\001\000\002\000\106\000\000\000\
\\001\000\002\000\122\000\000\000\
\\001\000\002\000\125\000\000\000\
\\001\000\002\000\128\000\000\000\
\\001\000\002\000\132\000\000\000\
\\001\000\002\000\133\000\000\000\
\\001\000\006\000\093\000\027\000\092\000\000\000\
\\001\000\006\000\119\000\000\000\
\\001\000\006\000\127\000\019\000\126\000\000\000\
\\001\000\006\000\130\000\000\000\
\\001\000\007\000\070\000\009\000\069\000\000\000\
\\001\000\007\000\070\000\038\000\095\000\000\000\
\\001\000\008\000\094\000\000\000\
\\001\000\009\000\090\000\000\000\
\\001\000\009\000\118\000\000\000\
\\001\000\011\000\077\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\011\000\088\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\013\000\086\000\000\000\
\\001\000\013\000\123\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\030\000\068\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\034\000\096\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\067\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\120\000\000\000\
\\001\000\019\000\087\000\000\000\
\\001\000\019\000\091\000\000\000\
\\001\000\019\000\134\000\000\000\
\\001\000\027\000\066\000\000\000\
\\001\000\027\000\116\000\000\000\
\\001\000\037\000\065\000\042\000\064\000\043\000\063\000\044\000\062\000\000\000\
\\001\000\039\000\114\000\000\000\
\\137\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\138\000\010\000\019\000\014\000\018\000\027\000\017\000\000\000\
\\139\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\146\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\147\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\148\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\149\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\150\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\000\000\
\\151\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\000\000\
\\152\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\000\000\
\\153\000\017\000\029\000\018\000\028\000\000\000\
\\154\000\017\000\029\000\018\000\028\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\160\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\161\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\031\000\097\000\000\000\
\\162\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\163\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\002\000\109\000\000\000\
\\177\000\005\000\117\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\181\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\182\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\183\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\184\000\000\000\
\\185\000\005\000\112\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\186\000\000\000\
\\187\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\188\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\189\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\190\000\005\000\089\000\000\000\
\\191\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\192\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\193\000\008\000\042\000\010\000\041\000\012\000\040\000\000\000\
\\194\000\039\000\099\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\"
val actionRowNumbers =
"\002\000\061\000\041\000\040\000\
\\069\000\067\000\070\000\003\000\
\\002\000\002\000\002\000\001\000\
\\043\000\044\000\096\000\002\000\
\\004\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\038\000\036\000\
\\031\000\029\000\060\000\020\000\
\\090\000\045\000\005\000\002\000\
\\092\000\042\000\098\000\025\000\
\\055\000\054\000\053\000\051\000\
\\052\000\050\000\049\000\048\000\
\\059\000\058\000\057\000\056\000\
\\074\000\073\000\072\000\071\000\
\\006\000\007\000\008\000\002\000\
\\002\000\002\000\002\000\046\000\
\\002\000\027\000\033\000\026\000\
\\093\000\023\000\094\000\099\000\
\\034\000\016\000\022\000\021\000\
\\030\000\065\000\064\000\091\000\
\\087\000\002\000\097\000\002\000\
\\047\000\009\000\002\000\010\000\
\\079\000\068\000\002\000\002\000\
\\088\000\002\000\095\000\075\000\
\\039\000\079\000\076\000\083\000\
\\037\000\080\000\024\000\017\000\
\\032\000\063\000\005\000\062\000\
\\011\000\028\000\002\000\012\000\
\\018\000\013\000\002\000\089\000\
\\078\000\077\000\084\000\019\000\
\\002\000\014\000\081\000\066\000\
\\015\000\085\000\035\000\082\000\
\\002\000\086\000\000\000"
val gotoT =
"\
\\001\000\134\000\010\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\030\000\000\000\
\\000\000\
\\010\000\032\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\033\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\034\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\036\000\011\000\035\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\041\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\010\000\043\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\044\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\045\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\046\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\047\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\048\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\049\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\050\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\051\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\052\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\053\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\054\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\055\000\012\000\002\000\013\000\001\000\000\000\
\\002\000\059\000\004\000\058\000\005\000\057\000\006\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\069\000\000\000\
\\010\000\071\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\074\000\012\000\002\000\013\000\001\000\015\000\073\000\
\\016\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\036\000\011\000\079\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\080\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\081\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\082\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\010\000\083\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\096\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\010\000\098\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\007\000\099\000\000\000\
\\010\000\103\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\008\000\106\000\009\000\105\000\000\000\
\\000\000\
\\010\000\108\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\109\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\010\000\111\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\113\000\009\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\122\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\127\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\129\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\133\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 135
val numrules = 60
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | STRING of  (string)
 | INT of  (int) | ID of  (string)
 | nonempty_exparglist of  (Absyn.exp list)
 | exparglist of  (Absyn.exp list)
 | recordinst of  ( ( symbol * Absyn.exp * pos )  list)
 | record of  (Absyn.exp) | lvalue of  (Absyn.var)
 | explist of  ( ( Absyn.exp * Absyn.pos )  list)
 | exp of  (Absyn.exp) | nonempty_tyfields of  (Absyn.field list)
 | tyfields of  (Absyn.field list) | ty of  (Absyn.ty)
 | fundec of  (Absyn.dec) | vardec of  (Absyn.dec)
 | tydec of  (Absyn.dec) | decs of  (Absyn.dec list)
 | dec of  (Absyn.dec) | program of  (Absyn.exp)
end
type svalue = MlyValue.svalue
type result = Absyn.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "LOOP"
  | (T 45) => "UMINUS"
  | (T 46) => "ARRAYCREATE"
  | (T 47) => "DANGLINGELSE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41)
 $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.program (exp)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.lvalue lvalue, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (Absyn.VarExp(lvalue))
 in ( LrTable.NT 9, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lvalue lvalue, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (
Absyn.AssignExp{var=lvalue, exp=exp, pos=ASSIGNleft})
 in ( LrTable.NT 9, ( result, lvalue1left, exp1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.STRING STRING, (STRINGleft as STRING1left), 
STRING1right)) :: rest671)) => let val  result = MlyValue.exp (
Absyn.StringExp(STRING, STRINGleft))
 in ( LrTable.NT 9, ( result, STRING1left, STRING1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (Absyn.IntExp(INT))
 in ( LrTable.NT 9, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.exp (Absyn.SeqExp(nil))
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.explist 
explist, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.exp (Absyn.SeqExp(rev explist))
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exparglist 
exparglist, _, _)) :: _ :: ( _, ( MlyValue.ID ID, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.exp (
Absyn.CallExp{func=Symbol.symbol(ID), args=exparglist, pos=IDleft})
 in ( LrTable.NT 9, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, EQleft
, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.EqOp, right=exp2, pos=EQleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.NeqOp, right=exp2, pos=NEQleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.LtOp, right=exp2, pos=LTleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.GtOp, right=exp2, pos=GTleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.LeOp, right=exp2, pos=LEleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.GeOp, right=exp2, pos=GEleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ANDleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
Absyn.IfExp{test=exp1, then'=exp2, else'=SOME(Absyn.IntExp(0)), pos=ANDleft}
)
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ORleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (
Absyn.IfExp{test=exp1, then'=Absyn.IntExp(1), else'=SOME(exp2), pos=ORleft}
)
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.PlusOp, right=exp2, pos=PLUSleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.MinusOp, right=exp2, pos=MINUSleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.TimesOp, right=exp2, pos=TIMESleft})
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)
) => let val  result = MlyValue.exp (
Absyn.OpExp{left=exp1, oper=Absyn.DivideOp, right=exp2, pos=DIVIDEleft}
)
 in ( LrTable.NT 9, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (
Absyn.OpExp{left=Absyn.IntExp(0), oper=Absyn.MinusOp, right=exp1, pos=MINUSleft}
)
 in ( LrTable.NT 9, ( result, MINUS1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.record record, record1left, record1right))
 :: rest671)) => let val  result = MlyValue.exp (record)
 in ( LrTable.NT 9, ( result, record1left, record1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (
Absyn.ArrayExp{typ=Symbol.symbol(ID), size=exp1, init=exp2, pos=IDleft}
)
 in ( LrTable.NT 9, ( result, ID1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (
Absyn.IfExp{test=exp1, then'=exp2, else'=SOME(exp3), pos=IFleft})
 in ( LrTable.NT 9, ( result, IF1left, exp3right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (
Absyn.IfExp{test=exp1, then'=exp2, else'=NONE, pos=IFleft})
 in ( LrTable.NT 9, ( result, IF1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (
Absyn.WhileExp{test=exp1, body=exp2, pos=WHILEleft})
 in ( LrTable.NT 9, ( result, WHILE1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, (FORleft as FOR1left)
, _)) :: rest671)) => let val  result = MlyValue.exp (
Absyn.ForExp{var=Symbol.symbol(ID), escape=ref false, lo=exp1, hi=exp2, body=exp3, pos=FORleft}
)
 in ( LrTable.NT 9, ( result, FOR1left, exp3right), rest671)
end
|  ( 27, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (Absyn.BreakExp(BREAKleft)
)
 in ( LrTable.NT 9, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 28, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.explist explist
, _, _)) :: _ :: ( _, ( MlyValue.decs decs, _, _)) :: ( _, ( _, (
LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (
Absyn.LetExp{decs=(rev (combineDecs decs)), body=Absyn.SeqExp(rev explist), pos=LETleft}
)
 in ( LrTable.NT 9, ( result, LET1left, END1right), rest671)
end
|  ( 29, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (Absyn.NilExp)
 in ( LrTable.NT 9, ( result, NIL1left, NIL1right), rest671)
end
|  ( 30, ( rest671)) => let val  result = MlyValue.decs (nil)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 31, ( ( _, ( MlyValue.dec dec, _, dec1right)) :: ( _, ( 
MlyValue.decs decs, decs1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (dec :: decs)
 in ( LrTable.NT 2, ( result, decs1left, dec1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.tydec tydec, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.dec (tydec)
 in ( LrTable.NT 1, ( result, tydec1left, tydec1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.vardec vardec, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (vardec)
 in ( LrTable.NT 1, ( result, vardec1left, vardec1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.fundec fundec, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.dec (fundec)
 in ( LrTable.NT 1, ( result, fundec1left, fundec1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ty ty, _, ty1right)) :: ( _, ( _, EQleft, _
)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, TYPE1left, _)) :: 
rest671)) => let val  result = MlyValue.tydec (
Absyn.TypeDec({name=Symbol.symbol(ID), ty=ty, pos=EQleft} :: nil))
 in ( LrTable.NT 3, ( result, TYPE1left, ty1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ID ID, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (
Absyn.NameTy(Symbol.symbol(ID), IDleft))
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (Absyn.RecordTy(tyfields))
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID, IDleft, ID1right)) :: _ :: ( _, ( _,
 ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ty (
Absyn.ArrayTy(Symbol.symbol(ID), IDleft))
 in ( LrTable.NT 6, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 39, ( rest671)) => let val  result = MlyValue.tyfields (nil)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.nonempty_tyfields nonempty_tyfields, 
nonempty_tyfields1left, nonempty_tyfields1right)) :: rest671)) => let
 val  result = MlyValue.tyfields (rev nonempty_tyfields)
 in ( LrTable.NT 7, ( result, nonempty_tyfields1left, 
nonempty_tyfields1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.nonempty_tyfields (
{name=Symbol.symbol(ID1), escape=ref false, typ=Symbol.symbol(ID2), pos=ID1left} :: nil
)
 in ( LrTable.NT 8, ( result, ID1left, ID2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: _ :: ( _, ( 
MlyValue.nonempty_tyfields nonempty_tyfields, nonempty_tyfields1left,
 _)) :: rest671)) => let val  result = MlyValue.nonempty_tyfields (
{name=Symbol.symbol(ID1), escape=ref false, typ=Symbol.symbol(ID2), pos=ID1left} :: nonempty_tyfields
)
 in ( LrTable.NT 8, ( result, nonempty_tyfields1left, ID2right), 
rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left
, _)) :: rest671)) => let val  result = MlyValue.vardec (
Absyn.VarDec{name=Symbol.symbol(ID), escape=ref false, typ=NONE, init=exp, pos=ASSIGNleft}
)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, (
 MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.vardec (
Absyn.VarDec{name=Symbol.symbol(ID1), escape=ref false, typ=SOME((Symbol.symbol(ID2), ID2left)), init=exp, pos=ASSIGNleft}
)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: ( _, ( _, EQleft
, _)) :: _ :: ( _, ( MlyValue.tyfields tyfields, _, _)) :: _ :: ( _, (
 MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671))
 => let val  result = MlyValue.fundec (
Absyn.FunctionDec({name=Symbol.symbol(ID), params=tyfields, result=NONE, body=exp, pos=EQleft} :: nil)
)
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: ( _, ( _, EQleft
, _)) :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( 
MlyValue.tyfields tyfields, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _,
 _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result =
 MlyValue.fundec (
Absyn.FunctionDec({name=Symbol.symbol(ID1), params=tyfields, result=SOME(Symbol.symbol(ID2), ID2left), body=exp, pos=EQleft} :: nil)
)
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 47, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordinst 
recordinst, _, _)) :: _ :: ( _, ( MlyValue.ID ID, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.record (
Absyn.RecordExp({typ=Symbol.symbol(ID), fields=recordinst, pos=IDleft})
)
 in ( LrTable.NT 12, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.recordinst ((Symbol.symbol(ID), exp, IDleft) :: nil)
 in ( LrTable.NT 13, ( result, ID1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.recordinst recordinst, _, recordinst1right)
) :: _ :: ( _, ( MlyValue.exp exp, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID, (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.recordinst ((Symbol.symbol(ID), exp, IDleft) :: recordinst)
 in ( LrTable.NT 13, ( result, ID1left, recordinst1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.exp exp, (expleft as exp1left), exp1right))
 :: rest671)) => let val  result = MlyValue.explist (
(exp, expleft) :: nil)
 in ( LrTable.NT 10, ( result, exp1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.exp exp, expleft, exp1right)) :: _ :: ( _, 
( MlyValue.explist explist, explist1left, _)) :: rest671)) => let val 
 result = MlyValue.explist ((exp, expleft) :: explist)
 in ( LrTable.NT 10, ( result, explist1left, exp1right), rest671)
end
|  ( 52, ( rest671)) => let val  result = MlyValue.exparglist (nil)
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 53, ( ( _, ( MlyValue.nonempty_exparglist nonempty_exparglist, 
nonempty_exparglist1left, nonempty_exparglist1right)) :: rest671)) =>
 let val  result = MlyValue.exparglist (rev nonempty_exparglist)
 in ( LrTable.NT 14, ( result, nonempty_exparglist1left, 
nonempty_exparglist1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.nonempty_exparglist (exp :: nil)
 in ( LrTable.NT 15, ( result, exp1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp, _, exp1right)) :: _ :: ( _, ( 
MlyValue.nonempty_exparglist nonempty_exparglist, 
nonempty_exparglist1left, _)) :: rest671)) => let val  result = 
MlyValue.nonempty_exparglist (exp :: nonempty_exparglist)
 in ( LrTable.NT 15, ( result, nonempty_exparglist1left, exp1right), 
rest671)
end
|  ( 56, ( ( _, ( MlyValue.ID ID, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (
Absyn.SimpleVar(Symbol.symbol(ID), IDleft))
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp, _,
 _)) :: ( _, ( _, _, LBRACKright)) :: ( _, ( MlyValue.ID ID, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.lvalue (
Absyn.SubscriptVar(Absyn.SimpleVar(Symbol.symbol(ID), IDleft), exp, LBRACKright)
)
 in ( LrTable.NT 11, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID, IDleft, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.lvalue (
Absyn.FieldVar(lvalue, Symbol.symbol(ID), IDleft))
 in ( LrTable.NT 11, ( result, lvalue1left, ID1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp, _,
 _)) :: ( _, ( _, _, LBRACKright)) :: ( _, ( MlyValue.lvalue lvalue, 
lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue (
Absyn.SubscriptVar(lvalue, exp, LBRACKright))
 in ( LrTable.NT 11, ( result, lvalue1left, RBRACK1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING i,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun LOOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAYCREATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun DANGLINGELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
end
end
