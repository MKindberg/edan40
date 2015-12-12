module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program []) = []
  toString (Program (s:ss)) = Statement.toString s ++ toString (Program ss)  
    
exec (Program p) = Statement.exec p Dictionary.empty
