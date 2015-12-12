module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement|
    Skip|
    Begin [Statement]|
    While Expr.T Statement|
    Read String|
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifState = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

beginEnd = accept "begin" -#  iter parse #- require "end" >-> buildBeg
buildBeg ss = Begin ss

whileDo = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readState = accept "read" -# word #- require ";" >-> buildRead
buildRead s = Read s

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
    
exec (Assignment s val:stmts) dict input = exec stmts (Dictionary.insert (s, Expr.value val dict) dict) input

exec (Skip:stmts) dict input = exec stmts dict input

exec (Begin s:stmts) dict input = exec (s++stmts) dict input

exec (While cond s:stmts) dict input = 
    if Expr.value cond dict > 0
    then exec (s:While cond s:stmts) dict input  
    else exec stmts dict input
    
exec (Read s:stmts) dict (i:input) = exec stmts (Dictionary.insert (s, i) dict) input

exec (Write e:stmts) dict input = Expr.value e dict : exec stmts dict input

instance Parse Statement where
  parse = assignment ! ifState ! skip ! beginEnd ! whileDo ! readState ! write
  
  toString (Assignment s e) = s ++ " := " ++ Expr.toString e ++ ";\n"
  toString (If e s1 s2) = "if " ++ Expr.toString e ++ " then \n" ++ toString s1 ++ "else \n" ++ toString s2
  toString Skip = "skip; \n"
  toString (Begin ss) = "begin \n" ++ concat (map toString ss ) ++ "end \n"
  toString (While e s) = "while " ++ Expr.toString e ++ " do \n" ++ toString s
  toString (Read s) = "read " ++ s ++ ";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"
