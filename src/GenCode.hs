module GenCode where
import Parsers

generate :: Program -> String
generate _ = "Not working"--map functionWriter

functionWriter :: Function -> String
functionWriter (Function returnType name arguments body) =
    ".globl _"++name++"\n"
  ++"_"++name++":" 
  
  
