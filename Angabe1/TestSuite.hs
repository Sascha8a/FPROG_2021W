module TestSuite1 where

import Angabe1 hiding (repeat, sort, main)
import Data.List (sort)
import Test.Tasty (testGroup)
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main =
  defaultMainWithIngredients
    [consoleTestReporter]
    spec

spec :: TestTree
spec =
  testGroup
    "TestSuite1 Spec"
    [ ist_tzrTests,
      tzr_zeugeTests,
      tzr_zeugeSchwerereTests,
      tzr_zeugenTests,
      tzr_zeugenSchwerereTests,
      wieOftTests
    ]

ist_tzrTests :: TestTree
ist_tzrTests =
  testGroup
    "Tests für ist_tzr"
    [ testCase "Leere Teilzeichenreihe" $ do
        ist_tzr "Explosion" "" @?= True,
      testCase "Leere Zeichenreihe" $ do
        ist_tzr "" "Explosion" @?= False,
      testCase "Teilzeichenreihe kommt vor" $ do
        ist_tzr "Racecar" "acecar" @?= True,
      testCase "Teilzeichenreihe kommt nicht vor" $ do
        ist_tzr "Retsinakanister" "Kanister" @?= False,
      testCase "Teilzeichenreihe ist zu lange" $ do
        ist_tzr "Retsinakanister" "Retsinakanister2" @?= False,
      testCase "Teilzeichenreihe ist gleich Zeichenreihe" $ do
        ist_tzr "Racecar" "Racecar" @?= True
    ]

tzr_zeugeTests :: TestTree
tzr_zeugeTests =
  testGroup
    "Tests für tzr_zeuge"
    [ testCase "Kein Zeuge" $ do
        tzr_zeuge "Retsinakanister" "Kanister" @?= ("", "KanisterKanister", ""),
      testCase "Genau ein Zeuge, am Ende der Zeichenreihe" $ do
        tzr_zeuge "Retsinakanister" "kanister" @?= ("Retsina", "kanister", ""),
      testCase "Genau ein Zeuge am Anfang der Zeichenreihe" $ do
        tzr_zeuge "Retsinakanister" "Rets" @?= ("", "Rets", "inakanister"),
      testCase "Genau ein Zeuge in der Mitte der Zeichenreihe" $ do
        tzr_zeuge "Retsinakanister" "nak" @?= ("Retsi", "nak", "anister")
    ]

tzr_zeugeSchwerereTests :: TestTree
tzr_zeugeSchwerereTests =
  testGroup
    "Schwerere Tests für tzr_zeuge"
    [ testCase "Zeuge mit mehreren Lösungen, Teilzeichenreihe ist nicht Leer" $ do
        let zeuge = tzr_zeuge "Banana" "an"
        let alle_zeugen = [("B", "an", "ana"), ("Ban", "an", "a")]
        (zeuge `elem` alle_zeugen) @? "Keine bekannte Lösung",
      testCase "Zeuge mit mehreren Lösungen, Teilzeichenreihe ist Leer" $ do
        let zeuge = tzr_zeuge "all" ""
        let alle_zeugen = [("", "", "all"), ("a", "", "ll"), ("al", "", "l"), ("all", "", "")]
        (zeuge `elem` alle_zeugen) @? "Keine bekannte Lösung"
    ]

tzr_zeugenTests :: TestTree
tzr_zeugenTests =
  testGroup
    "Tests für tzr_zeugen"
    [ testCase "Keine Zeugen" $ do
        tzr_zeugen "Retsinakanister" "Kanister" @?= [],
      testCase "Genau ein Zeuge, am Ende der Zeichenreihe" $ do
        tzr_zeugen "Retsinakanister" "kanister" @?= [("Retsina", "kanister", "")],
      testCase "Genau ein Zeuge am Anfang der Zeichenreihe" $ do
        tzr_zeugen "Retsinakanister" "Rets" @?= [("", "Rets", "inakanister")],
      testCase "Genau ein Zeuge in der Mitte der Zeichenreihe" $ do
        tzr_zeugen "Retsinakanister" "nak" @?= [("Retsi", "nak", "anister")]
    ]

tzr_zeugenSchwerereTests :: TestTree
tzr_zeugenSchwerereTests =
  testGroup
    "Schwerere Tests für tzr_zeugen"
    [ testCase "Zeuge mit mehreren Lösungen, Teilzeichenreihe ist nicht Leer" $ do
        let zeugen = tzr_zeugen "Banana" "an"
        let alle_zeugen = [("B", "an", "ana"), ("Ban", "an", "a")]
        (sort zeugen) @?= alle_zeugen,
      testCase "Zeuge mit mehreren Lösungen, Teilzeichenreihe ist Leer" $ do
        let zeugen = tzr_zeugen "all" ""
        let alle_zeugen = [("", "", "all"), ("a", "", "ll"), ("al", "", "l"), ("all", "", "")]
        (sort zeugen) @?= alle_zeugen
    ]

wieOftTests :: TestTree
wieOftTests =
  testGroup
    "Tests für wieOft"
    [ testCase "Keine Zeugen" $ do
        wieOft "Retsinakanister" "Kanister" @?= 0,
      testCase "Genau ein Zeuge, am Ende der Zeichenreihe" $ do
        wieOft "Retsinakanister" "kanister" @?= 1,
      testCase "Genau ein Zeuge am Anfang der Zeichenreihe" $ do
        wieOft "Retsinakanister" "Rets" @?= 1,
      testCase "Zeuge mit mehreren Lösungen, Teilzeichenreihe ist nicht Leer" $ do
        let anzahl_zeugen = wieOft "Banana" "an"
        let alle_zeugen = [("B", "an", "ana"), ("Ban", "an", "a")]
        anzahl_zeugen @?= length alle_zeugen
    ]
