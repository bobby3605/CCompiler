module GenCode where
import Parsers
import Data.Map
import Control.Monad.State.Lazy

returnCodeGen :: Expression -> State VariableMap String
returnCodeGen expr = do
  codeGenStateHelper expr (genIns "mov" (Just "%rbp") "%rsp"
                             ++genIns "pop" Nothing "%rbp"
                             ++genIns "ret" Nothing "")


addCodeGen :: Expression -> Expression -> State VariableMap String
addCodeGen expr1 expr2 = genCodeHelper
  [(expr1, genIns "push" Nothing "%rax")
  ,(expr2, genIns "pop" Nothing "%rcx"
           ++genIns "add" (Just "%rcx") "%rax")]

mulCodeGen :: Expression -> Expression -> State VariableMap String
mulCodeGen expr1 expr2 = genCodeHelper
  [(expr1, genIns "push" Nothing "%rax")
  ,(expr2, genIns "pop" Nothing "%rcx"
           --imul is signed multiplication
           ++genIns "imul" (Just "%rcx") "%rax")]

subCodeGen :: Expression -> Expression -> State VariableMap String
subCodeGen expr1 expr2 = genCodeHelper
  -- e1, e2 swapped here because sub is src, dst, dst - src, and the result is stored in dst
  [(expr2, genIns "push" Nothing "%rax")
  ,(expr1, genIns "pop" Nothing "%rcx"
           ++genIns "sub" (Just "%rcx") "%rax")]

divCodeGen :: Expression -> Expression -> State VariableMap String
divCodeGen expr1 expr2 = genCodeHelper
  -- divides e1 by e2, stores quotient into rax
  [(expr2, genIns "push" Nothing "%rax")
  ,(expr1, genIns "pop" Nothing "%rcx"
           ++genIns "div" Nothing "%rcx")]

negationCodeGen :: Expression -> State VariableMap String
negationCodeGen expr = do
  codeGenStateHelper expr $ genIns "neg" Nothing "%rax"

bitwiseCompCodeGen :: Expression -> State VariableMap String
bitwiseCompCodeGen expr = do
  codeGenStateHelper expr $ genIns "not" Nothing "%rax"

moduloCodeGen :: Expression -> Expression -> State VariableMap String
moduloCodeGen expr1 expr2 = genCodeHelper
  [(expr2, genIns "mov" (Just "%rax") "%rcx")
  ,(expr1, genIns "cqo" Nothing ""
           ++genIns "idiv" Nothing "%rcx"
           ++genIns "mov" (Just "%rdx") "%rax")]

logicalNegationCodeGen :: Expression -> State VariableMap String
logicalNegationCodeGen expr = do
  codeGenStateHelper expr (genIns "cmp" (Just "$0") "%rax"
                           ++genIns "mov" (Just "$0") "%rax"
                           ++genIns "sete" Nothing "%al")

andCodeGen :: Expression -> Expression -> State VariableMap String
andCodeGen expr1 expr2 = genCodeHelper
  [(expr1, genIns "cmp" (Just "$0") "%rax"
           ++genIns "jne" Nothing "_andClause"
           ++genIns "jmp" Nothing "_andClauseEnd"
           ++"_andClause:"++"\n")
  ,(expr2, genIns "cmp" (Just "$0") "%rax"
           ++genIns "mov" (Just "$0") "%rax"
           ++genIns "setne" Nothing "%al"
           ++"_andClauseEnd:"++"\n")]

orCodeGen :: Expression -> Expression -> State VariableMap String
orCodeGen expr1 expr2 = genCodeHelper
  [(expr1, genIns "cmp" (Just "$0") "%rax"
           ++genIns "je" Nothing "_orClause"
           ++genIns "mov" (Just "$1") "%rax"
           ++genIns "jmp" Nothing "_orClauseEnd"
           ++"_orClause:"++"\n")
  ,(expr2, genIns "cmp" (Just "$0") "%rax"
           ++genIns "mov" (Just "$0") "%rax"
           ++genIns "setne" Nothing "%al"
           ++"_orClauseEnd:"++"\n")]

equalityChecksCodeGen :: Expression -> Expression -> String -> State VariableMap String
equalityChecksCodeGen expr1 expr2 ins = genCodeHelper
    [(expr1, genIns "push" Nothing "%rax")
    ,(expr2, genIns "pop" Nothing "%rcx"
             ++genIns "cmp" (Just "%eax") "%ecx"
             ++genIns "mov" (Just "$0") "%eax"
             ++genIns ins Nothing "%al")]

equalCodeGen :: Expression -> Expression -> State VariableMap String
equalCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "sete"

notEqualCodeGen :: Expression -> Expression -> State VariableMap String
notEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setne"

lessThanCodeGen :: Expression -> Expression -> State VariableMap String
lessThanCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setl"

lessThanOrEqualCodeGen :: Expression -> Expression -> State VariableMap String
lessThanOrEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setle"

greaterThanCodeGen :: Expression -> Expression -> State VariableMap String
greaterThanCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setg"

greaterThanOrEqualCodeGen :: Expression -> Expression -> State VariableMap String
greaterThanOrEqualCodeGen expr1 expr2 = equalityChecksCodeGen expr1 expr2 "setge"

bitwiseBopCodeGen :: Expression  -> Expression -> String -> State VariableMap String
bitwiseBopCodeGen expr1 expr2 ins = genCodeHelper
  [(expr1, genIns "mov" (Just "%rax") "%rcx")
  ,(expr2, genIns ins (Just "%rcx") "%rax")]

bitwiseAndCodeGen :: Expression -> Expression -> State VariableMap String
bitwiseAndCodeGen expr1 expr2 = bitwiseBopCodeGen expr1 expr2 "and"

bitwiseOrCodeGen :: Expression -> Expression -> State VariableMap String
bitwiseOrCodeGen expr1 expr2 = bitwiseBopCodeGen expr1 expr2 "or"

bitwiseXorCodeGen :: Expression -> Expression -> State VariableMap String
bitwiseXorCodeGen expr1 expr2 = bitwiseBopCodeGen expr1 expr2 "xor"

bitwiseShiftLeftCodeGen :: Expression -> Expression -> State VariableMap String
bitwiseShiftLeftCodeGen expr1 expr2 = bitwiseBopCodeGen expr1 expr2 "shl"

bitwiseShiftRightCodeGen :: Expression -> Expression -> State VariableMap String
bitwiseShiftRightCodeGen expr1 expr2 = bitwiseBopCodeGen expr1 expr2 "shr"

tokenIntCodeGen :: Integer -> State VariableMap String
tokenIntCodeGen int = return $ genIns "mov" (Just $ "$"++show int) "%rax"

-- int a = 1;a=2;
-- int a;a=2;
-- a = 1; this should fail because not declared

-- needs to add to storage
declareVariableCodeGen :: String -> String -> Maybe Expression -> State VariableMap String
declareVariableCodeGen varName varType maybeValue = do
  inputMap <- get
  case maybeValue of
    Just expr -> do
      let (outputString,outputMap) = runState (matchCodeGen expr) inputMap in
        do
          put $ addVariableToStorage varName outputMap
          return $ outputString++genIns "push" Nothing "%rax"
    Nothing -> do
      put $ addVariableToStorage varName inputMap
      return $ genIns "push" Nothing "$0"

assignVariableCodeGen :: String -> Expression -> State VariableMap String
assignVariableCodeGen varName varValue = do
  inputMap <- get
  case getVariableLocation varName inputMap of
    Just varLocation -> codeGenStateHelper varValue $ genIns "mov" (Just "%rax") (show varLocation++"%rbp")
    Nothing -> return $ error "Variable not found: " ++ varName

getVariableCodeGen :: String -> State VariableMap String
getVariableCodeGen varName = do
  inputMap <- get
  case getVariableLocation varName inputMap of
      Just varLocation -> return $ genIns "mov" (Just (show varLocation++"(%rbp)")) "%rax"
      Nothing -> return $ error "Variable not found: " ++ varName

genIns :: String -> Maybe String -> String -> String
genIns ins (Just src) dst = beginspaces++ins++"    "++src++", "++dst++"\n"
genIns ins Nothing dst = beginspaces++ins++"    "++dst++"\n"

beginspaces :: String
beginspaces = "         "

matchCodeGen :: Expression -> State VariableMap String
matchCodeGen (Uop Return expr) = stateRunner (returnCodeGen expr) Nothing
matchCodeGen (Token (Integer a))  = stateRunner (tokenIntCodeGen a) Nothing
matchCodeGen (Token (Double a))  = return (show a) -- unimplemented
matchCodeGen (Token (Float a))  = return (show a)-- unimplemented
matchCodeGen (Token (String a))  = return (show a) -- unimplemented
matchCodeGen (Token (Char a))  = return (show a) -- unimplemented
matchCodeGen (Bop Add e1 e2)  = stateRunner (addCodeGen e1 e2) Nothing
matchCodeGen (Bop Sub e1 e2)  = stateRunner (subCodeGen e1 e2) Nothing
matchCodeGen (Bop Mul e1 e2)  = stateRunner (mulCodeGen e1 e2) Nothing
matchCodeGen (Bop Div e1 e2)  = stateRunner (divCodeGen e1 e2) Nothing
matchCodeGen (Uop Negation expr)  = stateRunner (negationCodeGen expr) Nothing
matchCodeGen (Uop LogicalNegation expr)  = stateRunner (logicalNegationCodeGen expr) Nothing
matchCodeGen (Uop BitwiseComp expr)  = stateRunner (bitwiseCompCodeGen expr) Nothing
matchCodeGen (Bop Modulo expr1 expr2)  = stateRunner (moduloCodeGen expr1 expr2) Nothing
matchCodeGen (Bop BitwiseAnd expr1 expr2)  = stateRunner (bitwiseAndCodeGen expr1 expr2) Nothing
matchCodeGen (Bop BitwiseOr expr1 expr2)  = stateRunner (bitwiseOrCodeGen expr1 expr2) Nothing
matchCodeGen (Bop BitwiseXor expr1 expr2)  = stateRunner (bitwiseXorCodeGen expr1 expr2) Nothing
matchCodeGen (Bop BitwiseShiftLeft expr1 expr2)  = stateRunner (bitwiseShiftLeftCodeGen expr1 expr2) Nothing
matchCodeGen (Bop BitwiseShiftRight expr1 expr2)  = stateRunner (bitwiseShiftRightCodeGen expr1 expr2) Nothing
matchCodeGen (Bop And expr expr2)  = stateRunner (andCodeGen expr expr2) Nothing
matchCodeGen (Bop Or expr expr2)  = stateRunner (orCodeGen expr expr2) Nothing
matchCodeGen (Bop Equal expr expr2)  = stateRunner (equalCodeGen expr expr2) Nothing
matchCodeGen (Bop NotEqual expr expr2)  = stateRunner (notEqualCodeGen expr expr2) Nothing
matchCodeGen (Bop LessThan expr expr2)  = stateRunner (lessThanCodeGen expr expr2) Nothing
matchCodeGen (Bop LessThanOrEqual expr expr2)  = stateRunner (lessThanOrEqualCodeGen expr expr2) Nothing
matchCodeGen (Bop GreaterThan expr expr2)  = stateRunner (greaterThanCodeGen expr expr2) Nothing
matchCodeGen (Bop GreaterThanOrEqual expr expr2)  = stateRunner (greaterThanOrEqualCodeGen expr expr2) Nothing
matchCodeGen (DeclareVariable varName varType Nothing)  = stateRunner (declareVariableCodeGen varName varType Nothing) Nothing
matchCodeGen (DeclareVariable varName varType varValue)  = stateRunner (declareVariableCodeGen varName varType varValue) Nothing
matchCodeGen (AssignVariable varName varValue)  = stateRunner (assignVariableCodeGen varName varValue) Nothing
matchCodeGen (Token (Variable varName varType))  = stateRunner (getVariableCodeGen varName) Nothing

data VariableMap = VariableMap (Map String Integer) Integer

blankVariableMap :: VariableMap
blankVariableMap = VariableMap empty 0

addVariableToStorage :: String -> VariableMap -> VariableMap
addVariableToStorage variableName (VariableMap inputMap stackPointer) = do
  if notMember variableName inputMap then
    VariableMap (insert variableName (stackPointer-8) inputMap) (stackPointer-8)
  else
    VariableMap inputMap stackPointer

getVariableLocation :: String -> VariableMap -> Maybe Integer
getVariableLocation variableName (VariableMap inputMap _) = Data.Map.lookup variableName inputMap

codeGenStateHelper :: Expression -> String -> State VariableMap String
codeGenStateHelper expr string = stateRunner (matchCodeGen expr) (Just string)

stateRunner :: State VariableMap String -> Maybe String -> State VariableMap String
stateRunner inputFunc appendString = do
  inputMap <- get
  let (outputString,outputMap) = runState inputFunc inputMap in
    do
      put outputMap
      case appendString of
        Just string -> return $ outputString++string
        Nothing -> return outputString

-- Feeds state and result from one StateT into the next
genCodeHelper :: [(Expression,String)] -> State VariableMap String
genCodeHelper l = helper l ""
  where helper :: [(Expression,String)] -> String -> State VariableMap String
        helper ((expr,inputString):xs) acc =
          do
            inputMap <- get
            let (outputString,outputMap) = runState (codeGenStateHelper expr inputString) inputMap in
              helper xs (acc++outputString)
        helper [] acc = return acc

generate :: Program -> String
generate = concatMap functionGenerator

functionGenerator :: Function -> String
functionGenerator (Function returnType name arguments body) = do
  ".globl "++name++"\n"
   ++name++":"++"\n"
   ++genIns "push" Nothing "%rbp"
   ++genIns "mov" (Just "%rsp") "%rbp"
   ++helper body blankVariableMap
   where helper :: [Expression] -> VariableMap -> String
         helper (expr:exprs) varMap =
           let (outputString,outputMap) = runState (matchCodeGen expr) varMap in
             outputString++helper exprs outputMap
         helper [] _ = ""
