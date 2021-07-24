module GenCode where
import Parsers

returnCodeGen :: Expression -> String
returnCodeGen expr =
    insMovHelper expr
  ++genIns "ret" Nothing ""

addCodeGen :: Expression -> Expression -> String
addCodeGen expr1 expr2 =
  insMovHelper expr1
  ++genIns "push" Nothing "%rax"
  ++insMovHelper expr2
  ++genIns "pop" Nothing "%rcx"
  ++genIns "add" (Just "%rcx") "%rax"

mulCodeGen :: Expression -> Expression -> String
mulCodeGen expr1 expr2 =
  insMovHelper expr1
  ++genIns "push" Nothing "%rax"
  ++insMovHelper expr2
  ++genIns "pop" Nothing "%rcx"
  --imul is signed multiplication
  ++genIns "imul" (Just "%rcx") "%rax"

subCodeGen :: Expression -> Expression -> String
subCodeGen expr1 expr2 =
  -- e1, e2 swapped here because sub is src, dst, dst - src, and the result is stored in dst
  insMovHelper expr2
  ++genIns "push" Nothing "%rax"
  ++insMovHelper expr1
  ++genIns "pop" Nothing "%rcx"
  ++genIns "sub" (Just "%rcx") "%rax"

divCodeGen :: Expression -> Expression -> String
divCodeGen expr1 expr2 =
  -- divides e1 by e2, stores quotient into rax
  insMovHelper expr2
  ++genIns "push" Nothing "%rax"
  ++insMovHelper expr1
  ++genIns "pop" Nothing "%rcx"
  ++genIns "div" Nothing "%rcx"

negationCodeGen :: Expression -> String
negationCodeGen expr =
  insMovHelper expr
  ++genIns "neg" Nothing "%rax"

bitwiseCompCodeGen :: Expression -> String
bitwiseCompCodeGen expr =
  insMovHelper expr
  ++genIns "not" Nothing "%rax"

logicalNegationCodeGen :: Expression -> String
logicalNegationCodeGen expr =
  insMovHelper expr
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "sete" Nothing "%al"

andCodeGen :: Expression -> Expression -> String
andCodeGen expr1 expr2 =
    insMovHelper expr1
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "jne" Nothing "_andClause"
  ++genIns "jmp" Nothing "_andClauseEnd"
  ++"_andClause:"++"\n"
  ++insMovHelper expr2
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "setne" Nothing "%al"
  ++"_andClauseEnd:"++"\n"

orCodeGen :: Expression -> Expression -> String
orCodeGen expr1 expr2 =
  insMovHelper expr1
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "je" Nothing "_orClause"
  ++genIns "mov" (Just "$1") "%rax"
  ++genIns "jmp" Nothing "_orClauseEnd"
  ++"_orClause:"++"\n"
  ++insMovHelper expr2
  ++genIns "cmp" (Just "$0") "%rax"
  ++genIns "mov" (Just "$0") "%rax"
  ++genIns "setne" Nothing "%al"
  ++"_orClauseEnd:"++"\n"

equalityChecksCodeGen :: Expression -> Expression -> String -> String
equalityChecksCodeGen expr1 expr2 ins =
    insMovHelper expr1
  ++genIns "push" Nothing "%rax"
  ++insMovHelper expr2
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

insMovHelper :: Expression -> String
insMovHelper expr =
  case matchCodeGen expr of
    Right s -> genIns "mov" (Just ("$"++s)) "%rax"
    Left s -> s

genIns :: String -> Maybe String -> String -> String
genIns ins (Just src) dst = beginspaces++ins++"    "++src++", "++dst++"\n"
genIns ins Nothing dst = beginspaces++ins++"    "++dst++"\n"

beginspaces :: String
beginspaces = "         "

matchCodeGen :: Expression -> Either String String
matchCodeGen (Uop Return expr) = Right $ returnCodeGen expr
matchCodeGen (Token (Integer a)) = Right $ show a
matchCodeGen (Token (Double a)) = Right $ show a -- unimplemented
matchCodeGen (Token (Float a)) = Right $ show a -- unimplemented
matchCodeGen (Token (String a)) = Right $ show a -- unimplemented
matchCodeGen (Token (Char a)) = Right $ show a -- unimplemented
matchCodeGen (Bop Add e1 e2) = Left $ addCodeGen e1 e2
matchCodeGen (Bop Sub e1 e2) = Left $ subCodeGen e1 e2
matchCodeGen (Bop Mul e1 e2) = Left $ mulCodeGen e1 e2
matchCodeGen (Bop Div e1 e2) = Left $ divCodeGen e1 e2
matchCodeGen (Uop Negation expr) = Left $ negationCodeGen expr
matchCodeGen (Uop LogicalNegation expr) = Left $ logicalNegationCodeGen expr
matchCodeGen (Uop BitwiseComp expr) = Left $ bitwiseCompCodeGen expr
matchCodeGen (Bop And expr expr2) = Left $ andCodeGen expr expr2
matchCodeGen (Bop Or expr expr2) = Left $ orCodeGen expr expr2
matchCodeGen (Bop Equal expr expr2) = Left $ equalCodeGen expr expr2
matchCodeGen (Bop NotEqual expr expr2) = Left $ notEqualCodeGen expr expr2
matchCodeGen (Bop LessThan expr expr2) = Left $ lessThanCodeGen expr expr2
matchCodeGen (Bop LessThanOrEqual expr expr2) = Left $ lessThanOrEqualCodeGen expr expr2
matchCodeGen (Bop GreaterThan expr expr2) = Left $ greaterThanCodeGen expr expr2
matchCodeGen (Bop GreaterThanOrEqual expr expr2) = Left $ greaterThanOrEqualCodeGen expr expr2

generate :: Program -> String
generate = concatMap functionGenerator

functionGenerator :: Function -> String
functionGenerator (Function returnType name arguments body) =
    ".globl "++name++"\n"
  ++name++":"++"\n"
  ++concatMap helper body
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> s
          Left e -> error "functionGenerator received Left " ++ show e
