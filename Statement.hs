module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement.T] |
    If Expr.T Statement.T Statement.T |
    While Expr.T Statement.T |
    Read String |
    Write Expr.T |
    Comment String
    
    deriving Show

getComment = accept "--" -# iter (char ? ((/=) '\n')) #- require "\n" >-> buildComment
buildComment s = Comment s

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

getSkip = accept "skip" # require ";" >-> buildSkip
buildSkip v = Skip

getRead = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

getWrite = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v = Write v

getWhile = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s


parseStatements = (parse #> (\ s -> parseStatements >-> ((:) s))) ! return []

getBegin = accept "begin" -# parseStatements #- require "end" >-> buildBegin
buildBegin s = Begin s

getIf = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e,s1),s2) = If e s1 s2  

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = getSkip ! assignment ! getRead ! getWrite ! getIf ! getWhile ! getBegin ! getComment
  toString = error "Statement.toString not implemented"
