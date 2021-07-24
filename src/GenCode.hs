module GenCode where
import Parsers

returnCodeGen :: Expression -> String
returnCodeGen expr = case matchCodeGen expr of
  -- A right string means it is a simple token
  Right s -> genmov ("$"++s) "rax"++beginspaces++"ret"++"\n"
  -- Left means it is an expression like Add
  -- it handles eax on its own
  Left s -> s++beginspaces++"ret"++"\n"

addCodeGen :: Expression -> Expression -> String
addCodeGen e1 e2 =
  helper e1
  ++beginspaces++"push %rax"++"\n"
  ++helper e2
  ++beginspaces++"pop %rcx"++"\n"
  ++beginspaces++"add %rcx, %rax"++"\n"
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
          Left s -> s

mulCodeGen :: Expression -> Expression -> String
mulCodeGen e1 e2 =
  helper e1
  ++beginspaces++"push %rax"++"\n"
  ++helper e2
  ++beginspaces++"pop %rcx"++"\n"
  -- imul is signed multiplication
  ++beginspaces++"imul %rcx, %rax"++"\n"
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
          Left s -> s

subCodeGen :: Expression -> Expression -> String
subCodeGen e1 e2 =
  -- e1, e2 swapped here because sub is src, dst, dst - src, and the result is stored in dst
  helper e2
  ++beginspaces++"push %rax"++"\n"
  ++helper e1
  ++beginspaces++"pop %rcx"++"\n"
  ++beginspaces++"sub %rcx, %rax"++"\n"
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
          Left s -> s

divCodeGen :: Expression -> Expression -> String
divCodeGen e1 e2 =
  -- divides e1 by e2, stores quotient into rax
  helper e2
  ++beginspaces++"push %rax"++"\n"
  ++helper e1
  ++beginspaces++"pop %rcx"++"\n"
  ++beginspaces++"div %rcx"++"\n"
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
          Left s -> s

negationCodeGen :: Expression -> String
negationCodeGen expr = helper expr
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"++
                      beginspaces++"neg"++"     "++"%rax"++"\n"
          Left s -> s
                    ++beginspaces++"neg"++"     "++"%rax"++"\n"

bitwiseCompCodeGen :: Expression -> String
bitwiseCompCodeGen expr = helper expr
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"++
                      beginspaces++"not"++"     "++"%rax"++"\n"
          Left s -> s
                    ++beginspaces++"not"++"     "++"%rax"++"\n"

logicalNegationCodeGen :: Expression -> String
logicalNegationCodeGen expr = helper expr
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
                     ++beginspaces++"cmp"++"    "++"$0"++", "++"%rax"++"\n"
                     ++beginspaces++"mov"++"    "++"$0"++", "++"%rax"++"\n"
                     ++beginspaces++"sete"++"    "++"%al"++"\n"
          Left s -> s
                    ++beginspaces++"cmp"++"    "++"$0"++", "++"%rax"++"\n"
                     ++beginspaces++"mov"++"    "++"$0"++", "++"%rax"++"\n"
                     ++beginspaces++"sete"++"    "++"%al"++"\n"

andCodeGen :: Expression -> Expression -> String
andCodeGen expr1 expr2 =
  helper1 expr1
  ++"_andClause:"++"\n"
  ++helper2 expr2
  ++"_andClauseEnd:"++"\n"
  where helper1 :: Expression -> String
        helper1 expr = case matchCodeGen expr of
          Right s -> genIns "mov" (Just ("$"++s)) "%rax"
                   ++genIns "cmp" (Just "$0") "%rax"
                   ++genIns "jne" Nothing "_andClause"
                   ++genIns "jmp" Nothing "_andClauseEnd"
          Left s -> s
                   ++genIns "cmp" (Just "$0") "%rax"
                   ++genIns "jne" Nothing "_andClause"
                   ++genIns "jmp" Nothing "_andClauseEnd"
        helper2 :: Expression -> String
        helper2 expr = case matchCodeGen expr of
          Right s -> genIns "mov" (Just ("$"++s)) "%rax"
                   ++genIns "cmp" (Just "$0") "%rax"
                   ++genIns "mov" (Just "$0") "%rax"
                   ++genIns "setne" Nothing "%al"
          Left s -> s
                   ++genIns "cmp" (Just "$0") "%rax"
                   ++genIns "mov" (Just "$0") "%rax"
                   ++genIns "setne" Nothing "%al"

genIns :: String -> Maybe String -> String -> String
genIns ins (Just src) dst = beginspaces++ins++"    "++src++", "++dst++"\n"
genIns ins Nothing dst = beginspaces++ins++"    "++dst++"\n"

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
{--
matchCodeGen (Bop Or expr expr2) = Left $ orCodeGen expr expr2
matchCodeGen (Bop Equal expr expr2) = Left $ equalCodeGen expr expr2
matchCodeGen (Bop NotEqual expr expr2) = Left $ notEqualCodeGen expr expr2
matchCodeGen (Bop LessThan expr expr2) = Left $ lessThanCodeGen expr expr2
matchCodeGen (Bop LessThanOrEqual expr expr2) = Left $ lessThanOrEqualCodeGen expr expr2
matchCodeGen (Bop GreaterThan expr expr2) = Left $ greaterThanCodeGen expr expr2
matchCodeGen (Bop GreaterThanOrEqual expr expr2) = Left $ greaterThanOrEqualCodeGen expr expr2
--}

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

beginspaces :: String
beginspaces = "         "

genmov :: String -> String -> String
genmov val reg = beginspaces++"mov"++"    "++val++", "++"%"++reg++"\n"
