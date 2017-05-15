module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin Statement |
    If Expr.T Statement Statement |
    If2 |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

getSkip = accept "skip" # require ";" >-> buildSkip
buildSkip v = Skip

getRead = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

getWrite = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v = Write v

--getBegin = accept "begin" -# parse #- require "end" >-> buildBegin
--buildBegin s

getIf = accept "read" -# Parse Statement #- require ";" >-> buildIf
buildIf v = If2 v

--getIf = accept "if" -# Expr.parse #- require "then" -# parse #- require "else" -# parse >-> buildIf
--buildIf (e,s1,s2) = If e s1 s2  

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = getSkip ! assignment ! getRead ! getWrite
  toString = error "Statement.toString not implemented"
