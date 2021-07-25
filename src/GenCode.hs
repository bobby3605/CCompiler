module GenCode where
import Parsers

returnCodeGen :: Expression -> String
returnCodeGen expr =
    matchCodeGen expr
  ++genIns "ret" Nothing ""

addCodeGen :: Expression -> Expression -> String
addCodeGen expr1 expr2 =
  matchCodeGen expr1
  ++genIns "push" Nothing "%rax"
  ++matchCodeGen expr2
  ++genIns "pop" Nothing "%rcx"
  ++genIns "add" (Just "%rcx") "%rax"

mulCodeGen :: Expression -> Expression -> String
mulCodeGen expr1 expr2 =
  matchCodeGen expr1
  ++genIns "push" Nothing "%rax"
  ++matchCodeGen expr2
  ++genIns "pop" Nothing "%rcx"
  --imul is signed multiplication
  ++genIns "imul" (Just "%rcx") "%rax"

subCodeGen :: Expression -> Expression -> String
subCodeGen expr1 expr2 =
  -- e1, e2 swapped here because sub is src, dst, dst - src, and the result is stored in dst
  matchCodeGen expr2
  ++genIns "push" Nothing "%rax"
  ++matchCodeGen expr1
  ++genIns "pop" Nothing "%rcx"
  ++genIns "sub" (Just "%rcx") "%rax"

divCodeGen :: Expression -> Expression -> String
divCodeGen expr1 expr2 =
  -- divides e1 by e2, stores quotient into rax
  matchCodeGen expr2
  ++genIns "push" Nothing "%rax"
  ++matchCodeGen expr1
  ++genIns "pop" Nothing "%rcx"
  ++genIns "div" Nothing "%rcx"

negationCodeGen :: Expression -> String
negationCodeGen expr =
  matchCodeGen expr
  ++genIns "neg" Nothing "%rax"

bitwiseCompCodeGen :: Expression -> String
bitwiseCompCodeGen expr =
  matchCodeGen expr
  ++genIns "not" Nothing "%rax"

moduloCodeGen :: Expression -> Expression -> String
moduloCodeGen expr1 expr2 =
  matchCodeGen expr2
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr1
  ++genIns "cqo" Nothing ""
  ++genIns "idiv" Nothing "%rcx"
  ++genIns "mov" (Just "%rdx") "%rax"

logicalNegationCodeGen :: Expression -> String
logicalNegationCodeGen expr =
  matchCodeGen expr
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "sete" Nothing "%al"

andCodeGen :: Expression -> Expression -> String
andCodeGen expr1 expr2 =
    matchCodeGen expr1
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "jne" Nothing "_andClause"
  ++genIns "jmp" Nothing "_andClauseEnd"
  ++"_andClause:"++"\n"
  ++matchCodeGen expr2
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "setne" Nothing "%al"
  ++"_andClauseEnd:"++"\n"

orCodeGen :: Expression -> Expression -> String
orCodeGen expr1 expr2 =
  matchCodeGen expr1
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "je" Nothing "_orClause"
  ++genIns "mov" (Just "$1") "%rax"
  ++genIns "jmp" Nothing "_orClauseEnd"
  ++"_orClause:"++"\n"
  ++matchCodeGen expr2
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "setne" Nothing "%al"
  ++"_orClauseEnd:"++"\n"

equalityChecksCodeGen :: Expression -> Expression -> String -> String
equalityChecksCodeGen expr1 expr2 ins =
    matchCodeGen expr1
  ++genIns "push" Nothing "%rax"
  ++matchCodeGen expr2
  ++genIns "pop" Nothing "%rcx"
  ++genIns "cmp" (Just "%eax") "%ecx"
  ++genIns "mov" (Just "$0") "%eax"
  ++genIns ins Nothing "%al"

equalCodeGen :: Expression -> Expression -> String
equalCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "sete"

notEqualCodeGen :: Expression -> Expression -> String
notEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setne"

lessThanCodeGen :: Expression -> Expression -> String
lessThanCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setl"

lessThanOrEqualCodeGen :: Expression -> Expression -> String
lessThanOrEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setle"

greaterThanCodeGen :: Expression -> Expression -> String
greaterThanCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setg"

greaterThanOrEqualCodeGen :: Expression -> Expression -> String
greaterThanOrEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setge"

bitwiseAndCodeGen :: Expression -> Expression -> String
bitwiseAndCodeGen expr1 expr2=
  matchCodeGen expr1
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr2
  ++genIns "and" (Just "%rcx") "%rax"

bitwiseOrCodeGen :: Expression -> Expression -> String
bitwiseOrCodeGen expr1 expr2=
  matchCodeGen expr1
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr2
  ++genIns "or" (Just "%rcx") "%rax"

bitwiseXorCodeGen :: Expression -> Expression -> String
bitwiseXorCodeGen expr1 expr2=
  matchCodeGen expr1
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr2
  ++genIns "xor" (Just "%rcx") "%rax"

bitwiseShiftLeftCodeGen :: Expression -> Expression -> String
bitwiseShiftLeftCodeGen expr1 expr2=
  matchCodeGen expr1
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr2
  ++genIns "shl" (Just "%rcx") "%rax"

bitwiseShiftRightCodeGen :: Expression -> Expression -> String
bitwiseShiftRightCodeGen expr1 expr2=
  matchCodeGen expr1
  ++genIns "mov" (Just "%rax") "%rcx"
  ++matchCodeGen expr2
  ++genIns "shr" (Just "%rcx") "%rax"

{--
matchCodeGen :: Expression -> String
matchCodeGen expr =
  case matchCodeGen expr of
    Right s -> genIns "mov" (Just ("$"++s)) "%rax"
    Left s -> s
-}

tokenIntCodeGen :: Integer -> String
tokenIntCodeGen int = genIns "mov" (Just $ "$"++show int) "%rax"

genIns :: String -> Maybe String -> String -> String
genIns ins (Just src) dst = beginspaces++ins++"    "++src++", "++dst++"\n"
genIns ins Nothing dst = beginspaces++ins++"    "++dst++"\n"

beginspaces :: String
beginspaces = "         "

matchCodeGen :: Expression -> String
matchCodeGen (Uop Return expr) = returnCodeGen expr
matchCodeGen (Token (Integer a)) = tokenIntCodeGen a
matchCodeGen (Token (Double a)) = show a -- unimplemented
matchCodeGen (Token (Float a)) = show a -- unimplemented
matchCodeGen (Token (String a)) = show a -- unimplemented
matchCodeGen (Token (Char a)) = show a -- unimplemented
matchCodeGen (Bop Add e1 e2) = addCodeGen e1 e2
matchCodeGen (Bop Sub e1 e2) = subCodeGen e1 e2
matchCodeGen (Bop Mul e1 e2) = mulCodeGen e1 e2
matchCodeGen (Bop Div e1 e2) = divCodeGen e1 e2
matchCodeGen (Uop Negation expr) = negationCodeGen expr
matchCodeGen (Uop LogicalNegation expr) = logicalNegationCodeGen expr
matchCodeGen (Uop BitwiseComp expr) = bitwiseCompCodeGen expr
matchCodeGen (Bop Modulo expr1 expr2) = moduloCodeGen expr1 expr2
matchCodeGen (Bop BitwiseAnd expr1 expr2) = bitwiseAndCodeGen expr1 expr2
matchCodeGen (Bop BitwiseOr expr1 expr2) = bitwiseOrCodeGen expr1 expr2
matchCodeGen (Bop BitwiseXor expr1 expr2) = bitwiseXorCodeGen expr1 expr2
matchCodeGen (Bop BitwiseShiftLeft expr1 expr2) = bitwiseShiftLeftCodeGen expr1 expr2
matchCodeGen (Bop BitwiseShiftRight expr1 expr2) = bitwiseShiftRightCodeGen expr1 expr2
matchCodeGen (Bop And expr expr2) = andCodeGen expr expr2
matchCodeGen (Bop Or expr expr2) = orCodeGen expr expr2
matchCodeGen (Bop Equal expr expr2) = equalCodeGen expr expr2
matchCodeGen (Bop NotEqual expr expr2) = notEqualCodeGen expr expr2
matchCodeGen (Bop LessThan expr expr2) = lessThanCodeGen expr expr2
matchCodeGen (Bop LessThanOrEqual expr expr2) = lessThanOrEqualCodeGen expr expr2
matchCodeGen (Bop GreaterThan expr expr2) = greaterThanCodeGen expr expr2
matchCodeGen (Bop GreaterThanOrEqual expr expr2) = greaterThanOrEqualCodeGen expr expr2

generate :: Program -> String
generate = concatMap functionGenerator

functionGenerator :: Function -> String
functionGenerator (Function returnType name arguments body) =
    ".globl "++name++"\n"
  ++name++":"++"\n"
  ++concatMap matchCodeGen body
