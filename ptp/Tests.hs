
import Test.HUnit
import Parser
import Declarations
import ProjectTools

testExprParser = TestCase $ do
	exprTest "  x   " (ExprVar "x")
	exprTest "  (2,  4,   4)   " (ExprTuple [ ExprInt 2, ExprInt 4, ExprInt 4])
	exprTest "  10  " (ExprInt 10)
	exprTest "  #aa  " (ExprParam "aa")
	exprTest "(x,20,(30, y ) ) " 
		(ExprTuple [ ExprVar "x", ExprInt 20, ExprTuple [ ExprInt 30, ExprVar "y" ]])
	exprTest  " ( )" (ExprTuple [])
	exprTest "x+y" (ExprCall "+" [ ExprVar "x", ExprVar "y"])
	exprTest "  x  +  10  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  + /* comment */ 10 )  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  +  (2,2) )  " (ExprCall "+" [ ExprVar "x", ExprTuple [ExprInt 2, ExprInt 2]])
	exprTest "a(b(c(), ()) + (133))" 
		(ExprCall "a" [ExprCall "+" [ExprCall "b" [ ExprCall "c" [], ExprTuple []], ExprInt 133]])
	exprTest "2 + 3 * 5" (ExprCall "+" [ ExprInt 2, ExprCall "*" [ ExprInt 3, ExprInt 5]])
	where exprTest str result = 
		assertEqual str (parseExpr str) result

testEdgeInscriptionParser = TestCase $ do
	exprTest "~name" (EdgePacking "name" Nothing)
	exprTest "  ~name   " (EdgePacking "name" Nothing)
	exprTest " ~xx (5)  " (EdgePacking "xx" (Just (ExprInt 5)))
	exprTest " ~xx ( 5)  " (EdgePacking "xx" (Just (ExprInt 5)))
	exprTest "5 * v" (EdgeExpression (ExprCall "*" [ ExprInt 5, ExprVar "v" ]))
	exprTest "5 >= v" (EdgeExpression (ExprCall ">=" [ ExprInt 5, ExprVar "v" ]))
	exprTest "5 > v" (EdgeExpression (ExprCall ">" [ ExprInt 5, ExprVar "v" ]))
	exprTest "x * 3 != 5 && y + 2 == 1" (EdgeExpression 
		(ExprCall "&&" [ ExprCall "!=" [ ExprCall "*" [ ExprVar "x", ExprInt 3 ], ExprInt 5],
						ExprCall "==" [ ExprCall "+" [ ExprVar "y", ExprInt 2 ], ExprInt 1]]))
	where exprTest str result = 
		assertEqual str (parseEdgeInscription str) result

testEdgeOrdering = TestCase $ do
	assertEqual "ordering" (orderEdgesByDependancy edges1) edges2
	where 
		e x = Edge { edgePlaceId = 0, edgeInscription = parseEdgeInscription x, edgeTarget = Nothing }
		edges1 = [ e "(x + 1, y + 1, z + 1)", e "~x", e "(x, z + 1)", e "(y, z)", e "y + 2", e "~ff(y)" ]
		edges2 = [ e "(y, z)", e "(x, z + 1)", e "y + 2", e "(x + 1, y + 1, z + 1)", e "~x", e "~ff(y)" ] 

testGuardParser = TestCase $ do
	exprTest "" ExprTrue
	exprTest "   " ExprTrue
	exprTest " x  " $ ExprVar "x"
	exprTest "   m + 5 > 2 + n" $ ExprCall ">" [ ExprCall "+" [ ExprVar "m", ExprInt 5 ], ExprCall "+" [ ExprInt 2, ExprVar "n" ]]
	where exprTest str result = 
		assertEqual str (parseGuard str) result



tests = TestList [ testExprParser, testEdgeInscriptionParser, testEdgeOrdering, testGuardParser ]

main = runTestTT tests
