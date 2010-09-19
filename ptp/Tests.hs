
import Test.HUnit
import Parser
import Declarations

testParser = TestCase $ do
	exprTest "  x   " (ExprVar "x")
	exprTest "  10  " (ExprInt 10)
	exprTest "(x,20,(30, y ) ) " 
		(ExprTuple [ ExprVar "x", ExprInt 20, ExprTuple [ ExprInt 30, ExprVar "y" ]])
	exprTest  " ( )" (ExprTuple [])
	exprTest "x+y" (ExprCall "+" [ ExprVar "x", ExprVar "y"])
	exprTest "  x  +  10  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  +  10 )  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  +  (2,2) )  " (ExprCall "+" [ ExprVar "x", ExprTuple [ExprInt 2, ExprInt 2]])
	exprTest "a(b(c(), ()) + (133))" 
		(ExprCall "a" [ExprCall "+" [ExprCall "b" [ ExprCall "c" [], ExprTuple []], ExprInt 133]])
	exprTest "2 + 3 * 5" (ExprCall "+" [ ExprInt 2, ExprCall "*" [ ExprInt 3, ExprInt 5]])
	where exprTest str result = 
		assertEqual str (parseExpr str) result

tests = TestList [ testParser ]

main = runTestTT tests
