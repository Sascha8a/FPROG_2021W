module TestSuite2 where

import Angabe2 hiding
  ( einzulassen_Tests,
    einzulassende_Tests,
    einzulassende_abzuweisende_Tests,
    main,
    mickey,
    personen,
    show_Tests,
    spec,
    testCase,
    testGroup,
    xmas20,
  )
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite2 Spec"
    [ einzulassen_Tests,
      einzulassende_Tests,
      einzulassende_abzuweisende_Tests,
      show_Tests
    ]

-- threeGees
mickey :: DreiG_Status -> Person
mickey = P (Vorname "Mickey") (Nachname "Mouse")

-- timez
xmas20 :: Kontrollzeitpunkt
xmas20 = (D XXIV Dez 20, U (Schlag, Sechs, NM))

einzulassen_Tests :: TestTree
einzulassen_Tests =
  testGroup
    "Tests für einzulassen"
    [ testCase "genesen 2G" $ do
        einzulassen (mickey Genesen, ZweiG, xmas20) @?= Einlassen,
      testCase "genesen 3G" $ do
        einzulassen (mickey Genesen, DreiG, xmas20) @?= Einlassen,
      testCase "geimpft J&J 2G" $ do
        einzulassen (mickey (Geimpft (JundJ, Einmal)), ZweiG, xmas20) @?= Einlassen,
      testCase "geimpft BT-1 2.5G" $ do
        einzulassen (mickey (Geimpft (BioNTec, Einmal)), ZweieinhalbG, xmas20) @?= Abweisen,
      testCase "geimpft BT-2 2.5G" $ do
        einzulassen (mickey (Geimpft (BioNTec, Zweimal)), ZweieinhalbG, xmas20) @?= Einlassen,
      testCase "geimpft sputnik 2.5G" $ do
        einzulassen (mickey (Geimpft (Sputnik, Zweimal)), ZweieinhalbG, xmas20) @?= Abweisen,
      testCase "testestet 3G" $ do
        einzulassen (mickey (Getestet Antigen (D XXIII Dez 20) (U (Halb, Sieben, NM))), DreiG, xmas20) @?= Einlassen,
      testCase "test abgelaufen 3G" $ do
        einzulassen (mickey (Getestet Antigen (D XXII Dez 20) (U (Halb, Sechs, NM))), DreiG, xmas20) @?= Abweisen,
      testCase "antigentest 2.5G" $ do
        einzulassen (mickey (Getestet Antigen (D XXIII Dez 20) (U (Halb, Sieben, NM))), ZweieinhalbG, xmas20) @?= Abweisen,
      testCase "test pcr 2.5G" $ do
        einzulassen (mickey (Getestet PCR (D XXII Dez 20) (U (Halb, Sechs, NM))), DreiG, xmas20) @?= Einlassen,
      testCase "Datum ungültig" $ do
        einzulassen (mickey (Getestet PCR (D XXXI Nov 20) (U (Halb, Sechs, NM))), DreiG, xmas20) @?= Ungueltig
    ]

personen :: Einlassbegehrende
personen =
  [ P (Vorname "Donald") (Nachname "Duck") (Geimpft (BioNTec, Einmal)),
    P (Vorname "Dagobert") (Nachname "Duck") (Geimpft (Moderna, Zweimal)),
    P (Vorname "Minnie") (Nachname "Maus") Genesen,
    P (Vorname "Buzz") (Nachname "Lightyear") (Getestet PCR (D XXII Dez 20) (U (Halb, Sechs, NM))),
    P (Vorname "Sheriff") (Nachname "Woody") (Getestet Antigen (D XXIV Dez 20) (U (Schlag, Fuenf, VM)))
  ]

einzulassende_Tests :: TestTree
einzulassende_Tests =
  testGroup
    "Tests for einzulassende"
    [ testCase "3G" $ do
        einzulassende personen DreiG xmas20 @?= ["Dagobert Duck", "Minnie Maus", "Buzz Lightyear", "Sheriff Woody"],
      testCase "2.5G" $ do
        einzulassende personen ZweieinhalbG xmas20 @?= ["Dagobert Duck", "Minnie Maus", "Buzz Lightyear"],
      testCase "2G" $ do
        einzulassende personen ZweiG xmas20 @?= ["Dagobert Duck", "Minnie Maus"]
    ]

einzulassende_abzuweisende_Tests :: TestTree
einzulassende_abzuweisende_Tests =
  testGroup
    "Tests for einzulassende_abzuweisende"
    [ testCase "3G" $ do
        einzulassende_abzuweisende personen DreiG xmas20 @?= (["Dagobert Duck", "Minnie Maus", "Buzz Lightyear", "Sheriff Woody"], ["Donald Duck"]),
      testCase "2.5G" $ do
        einzulassende_abzuweisende personen ZweieinhalbG xmas20 @?= (["Dagobert Duck", "Minnie Maus", "Buzz Lightyear"], ["Donald Duck", "Sheriff Woody"]),
      testCase "2G" $ do
        einzulassende_abzuweisende personen ZweiG xmas20 @?= (["Dagobert Duck", "Minnie Maus"], ["Donald Duck", "Buzz Lightyear", "Sheriff Woody"])
    ]

show_Tests :: TestTree
show_Tests =
  testGroup
    "Tests für show Instanzen"
    [ testCase "11:15" $ do
        show (U (Viertel, Elf, VM)) @?= "10:15 Uhr",
      testCase "15:30" $ do
        show (U (Halb, Vier, NM)) @?= "15:30 Uhr",
      testCase "22:00" $ do
        show (U (Schlag, Zehn, NM)) @?= "22:00 Uhr",
      testCase "11.11." $ do
        show (D XI Nov 1002) @?= "11.11.1002",
      testCase "9.3." $ do
        show (D IX Mar 2002) @?= "9.3.2002",
      testCase "29.2.2000" $ do
        show (D XXIX Feb 2000) @?= "29.2.2000",
      testCase "29.2.2100" $ do
        show (D XXIX Feb 2100) @?= "Datum ungueltig"
    ]
