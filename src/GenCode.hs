module GenCode where
import Parsers

generate :: Program -> String
generate = concatMap functionGenerator

integerGetter :: [Expression] -> Integer
integerGetter l = helper (head l)
  where helper (Return (Integer a)) = a
        helper _ = 0

functionGenerator :: Function -> String
functionGenerator (Function returnType name arguments body) =
    ".globl "++name++"\n"
  ++name++":"++"\n"
  ++"         "++"movl   "++"$"++show (integerGetter body)++", "++"%eax"++"\n"
  ++"         "++"ret"++"\n"
