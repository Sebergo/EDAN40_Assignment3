module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show -- to be defined
instance Parse T where
  parse = error "not implemented"
  toString (Program xs) = foldr ((++).Statement.toString) [] xs 
  fromString cs = (Program (inner cs))
 

exec (Program p) ints = Statement.exec p Dictionary.empty ints
  
  
inner xs =
    case parse xs of
           Just(s, []) -> [s]
           Just(s, xs2) -> s:(inner xs2)
           Nothing -> error "Nothing"
