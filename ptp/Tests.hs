
import Test.HUnit
import Parser
import Declarations
import ProjectTools
import Codegen

import qualified Data.Set as Set

testExprParser = TestCase $ do
	exprTest "  x   " (ExprVar "x")
	exprTest "  (2,  4,   4)   " (ExprTuple [ ExprInt 2, ExprInt 4, ExprInt 4])
	exprTest "  10  " (ExprInt 10)
	exprTest "  #aa  " (ExprParam "aa")
	exprTest "(x,20,(30, y ) ) " 
		(ExprTuple [ ExprVar "x", ExprInt 20, ExprTuple [ ExprInt 30, ExprVar "y" ]])
	exprTest  " ( )" (ExprTuple [])
	exprTest "x+y" (ExprCall "+" [ ExprVar "x", ExprVar "y"])
	exprTest "(x - y)" (ExprCall "-" [ ExprVar "x", ExprVar "y"])
	exprTest "  x  +  10  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  + /* comment */ 10 )  " (ExprCall "+" [ ExprVar "x", ExprInt 10])
	exprTest " (  x  +  (2,2) )  " (ExprCall "+" [ ExprVar "x", ExprTuple [ExprInt 2, ExprInt 2]])
	exprTest "a(b(c(), ()) + (133))" 
		(ExprCall "a" [ExprCall "+" [ExprCall "b" [ ExprCall "c" [], ExprTuple []], ExprInt 133]])
	exprTest "2 + 3 * 5" (ExprCall "+" [ ExprInt 2, ExprCall "*" [ ExprInt 3, ExprInt 5]])
	where exprTest str result = 
		assertEqual str (parseExpr "" str) result

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
		assertEqual str (parseEdgeInscription "" str) result

testEdgeOrdering = TestCase $ do
	assertEqual "ordering" (orderEdgesByDependancy edges1) edges2
	where 
		e x = Edge { edgePlaceId = 0, edgeInscription = parseEdgeInscription "" x, edgeTarget = Nothing }
		edges1 = [ e "(x + 1, y + 1, z + 1)", e "~x", e "(x, z + 1)", e "(y, z)", e "y + 2", e "~ff(y)" ]
		edges2 = [ e "(y, z)", e "(x, z + 1)", e "y + 2", e "(x + 1, y + 1, z + 1)", e "~x", e "~ff(y)" ] 

testGuardParser = TestCase $ do
	exprTest "" ExprTrue
	exprTest "   " ExprTrue
	exprTest " x  " $ ExprVar "x"
	exprTest "   m + 5 > 2 + n" $ ExprCall ">" [ ExprCall "+" [ ExprVar "m", ExprInt 5 ], ExprCall "+" [ ExprInt 2, ExprVar "n" ]]
	where exprTest str result = 
		assertEqual str (parseGuard "" str) result


testTypeParser = TestCase $ do
	exprTest " Int " TInt
	exprTest "String" TString
	exprTest "(Int, (Int,   Int, (String, Int  )), Int,  String)" $ TTuple [ TInt, TTuple [ TInt, TInt, TTuple [ TString, TInt ]], TInt, TString ]
	where exprTest str result = 
		assertEqual str (parseType standardTypes "" str) result

testParametersParser = TestCase $ do
	test "Int x, String b" [("x", TInt), ("b", TString)]
	test "  " []
	test "(Int,String) zzz  " [ ("zzz", TTuple [ TInt, TString ]) ]
	where test str result =
		assertEqual str (parseParameters standardTypes "" str) result

testTypeDependancy = TestCase $ do
	test "1" [TInt] [TInt]
	test "2" [TTuple [TInt, TString], TInt, TString] [ TInt, TString, TTuple [ TInt, TString ] ]
	test "3" [TInt, TTuple [TInt, TString], TString] [ TInt, TString, TTuple [ TInt, TString ] ]
	test "4" [TTuple [ TTuple [TInt, TString], TString ], TString, TInt, TTuple [TInt, TString]] 
		[ TInt, TString, TTuple [ TInt, TString ], TTuple [ TTuple [ TInt, TString ], TString ] ]
	where test name x y =
		assertEqual name (orderTypeByDepedancy (Set.fromList x)) y

tests = TestList [
	testExprParser,
	testEdgeInscriptionParser,
	testEdgeOrdering,
	testGuardParser,
	testTypeParser,
	testParametersParser,
	testTypeDependancy ]

main = runTestTT tests
