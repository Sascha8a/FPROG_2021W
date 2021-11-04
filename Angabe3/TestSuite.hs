module TestSuite3 where

import Angabe3
import Test.Tasty as T
import Test.Tasty.HUnit as T ( testCase, (@?=) )
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.ExpectedFailure

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite3 Spec"
    [ show_matrix_Tests,
      matrixtyp_Tests,
      geichheit_Tests,
      expectFail geichheit_fail_Tests,
      addition_Tests,
      multiplikation_Tests,
      fromInteger_Tests
    ]

-- threeGees

show_matrix_Tests :: TestTree
show_matrix_Tests =
  testGroup
    "Tests für show"
    [ testCase "(M [[1,2],[3,4]])" $ do
        show  (M [[1,2],[3,4]]) @?= "([1,2] [3,4])",
      testCase "(M [[1,2],[3,4],[5,6]])" $ do
        show (M [[1,2],[3,4],[5,6]]) @?= "([1,2] [3,4] [5,6])",
      testCase "(M [[1,2,3],[4,5],[6]])" $ do
        show (M [[1,2,3],[4,5],[6]]) @?= "([1,2,3] [4,5] [6])",
      testCase "(M [[1,2,3],[],[6]])" $ do
        show (M [[1,2,3],[],[6]]) @?= "([1,2,3] [] [6])",
      testCase "(M [[],[],[]])" $ do
        show (M [[],[],[]]) @?= "([] [] [])",
      testCase "(M [])" $ do
        show (M []) @?= "()"
    ]

matrixtyp_Tests :: TestTree
matrixtyp_Tests =
  testGroup
    "Tests für matrixtyp"
    [ testCase "leere Matrix" $ do
        matrixtyp (M []) @?= KeineMatrix,
      testCase "leere Zeilen" $ do
        matrixtyp (M [[], [], []]) @?= KeineMatrix,
      testCase "leere Zeilen 2" $ do
        matrixtyp (M [[2], [2], [], [2]]) @?= KeineMatrix,
      testCase "ungleich viele Spalten" $ do
        matrixtyp (M [[1,2,3], [1,2,3,4], [1,2,3]]) @?= KeineMatrix,
      testCase "ungleich viele Spalten 2" $ do
        matrixtyp (M [[1,2,3], [1,2], [1,2,3]]) @?= KeineMatrix,
      testCase "1x1" $ do
        matrixtyp (M [[1]]) @?= Mat (1, 1),
      testCase "2x2" $ do
        matrixtyp (M [[2,2], [2,2]]) @?= Mat (2, 2),
      testCase "1x2" $ do
        matrixtyp (M [[1, 1]]) @?= Mat (1, 2),
      testCase "2x1" $ do
        matrixtyp (M [[1], [1]]) @?= Mat (2, 1)
    ]

geichheit_Tests :: TestTree
geichheit_Tests =
  testGroup
    "Tests für =="
    [ testCase "Zwei idente Matrizen" $ do
        (M [[1]]) == (M [[1]]) @?= True
    ]


geichheit_fail_Tests :: TestTree
geichheit_fail_Tests = testGroup
    "Exception Tests für =="
    [ testCase "Leere Matrix" $ do
        (M []) == (M []) @?= False,
      testCase "Leere Matrix 2" $ do
        (M [[1]]) == (M []) @?= False,
      testCase "Leere Matrix 3" $ do
        (M [[]]) == (M [[1]]) @?= False
    ]
  
addition_Tests :: TestTree
addition_Tests = testGroup
    "Add / Sub Tests"
    [ testCase "1+1=2" $ do
        M [[1]] + M [[1]] @?= M [[2]],
      testCase "(-1)+1=0" $ do
        M [[-1]] + M [[1]] @?= M [[0]],
      testCase "1+0=1" $ do
        M [[1]] + M [[0]] @?= M [[1]],
      testCase "2x3 + 2x3" $ do
        M [[1,2,3], [1,2,3], [1,2,3]] + M [[1,2,3], [1,2,3], [1,2,3]] @?= M [[2,4,6], [2,4,6], [2,4,6]],
      testCase "2x3 - 2x3" $ do
        M [[1,2,3], [1,2,3], [1,2,3]] - M [[1,2,3], [1,2,3], [1,2,3]] @?= M [[0,0,0], [0,0,0], [0,0,0]]
    ]

multiplikation_Tests :: TestTree
multiplikation_Tests = testGroup
    "Multiplikation Tests"
    [ testCase "1*1=1" $ do
        M [[1]] * M [[1]] @?= M [[1]],
      testCase "1*(-1)=(-1)" $ do
        M [[1]] * M [[-1]] @?= M [[-1]],
      testCase "2x3 * 3*2" $ do
        M [[1,1,1],[1,1,1]] * M [[1,1], [1,1], [1,1]] @?= M [[3, 3], [3, 3]],
      testCase "Undefined: 2x3 * 2x2" $ do
        show (M [[1,1,1],[1,1,1]] * M [[1,1], [1,1]]) @?= "()",
      testCase "Skalarprodukt -1" $ do
        M [[-1]] * M [[1,1], [1,1]] @?= M [[-1,-1], [-1,-1]]
    ]

fromInteger_Tests :: TestTree
fromInteger_Tests = testGroup
    "fromInteger Tests"
    [ testCase "1" $ do
        1 @?= M [[1]],
      testCase "-1" $ do
        (-1) @?= M [[-1]]
    ]