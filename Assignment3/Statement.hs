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
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! ifState ! skip ! beginEnd ! whileDo ! readState ! write
  toString = error "Statement.toString not implemented"
