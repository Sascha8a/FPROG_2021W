module TestSuite4 where

import Angabe4
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

jan1 = D I Jan 2000
mustermann = GP "Max Mustermann" jan1

spec :: TestTree
spec =
  testGroup
    "TestSuite4 Spec"
    [ waup_Tests ]

waup_Tests :: TestTree
waup_Tests =
  testGroup
    "Tests für waup"
    [ testCase "zahlung, kein skonto" $ do
        waup (mustermann, Zahlung (C 100) KeinSkonto jan1)  @?= (mustermann, AP_Zahlung (C 100) jan1),
      testCase "zahlung, 3% skonto" $ do
        waup (mustermann, Zahlung (C 100) DreiProzent jan1)  @?= (mustermann, AP_Zahlung (C 97) jan1),
      testCase "zahlung, 5% skonto" $ do
        waup (mustermann, Zahlung (C 100) FuenfProzent jan1)  @?= (mustermann, AP_Zahlung (C 95) jan1),
      testCase "zahlung, 10% skonto" $ do
        waup (mustermann, Zahlung (C 100) ZehnProzent jan1)  @?= (mustermann, AP_Zahlung (C 90) jan1),
      testCase "zahlung, skonto rundung" $ do
        waup (mustermann, Zahlung (C 50) FuenfProzent jan1)  @?= (mustermann, AP_Zahlung (C 48) jan1),
      testCase "zahlung, skonto rundung 2" $ do
        waup (mustermann, Zahlung (C 60) DreiProzent jan1)  @?= (mustermann, AP_Zahlung (C 59) jan1),
      testCase "Geschäftsfall 31. April" $ do
        waup (mustermann, Zahlung (C 100) KeinSkonto (D XXXI Apr 2021))  @?= (mustermann, AP_Zahlung (C 100) (D I Mai 2021)),
      testCase "Kunde + Geschäftsfall 31. April" $ do
        waup (GP "Max Mustermann" (D XXXI Apr 2021), Zahlung (C 100) KeinSkonto (D XXXI Apr 2021))  @?= (GP "Max Mustermann" (D I Mai 2021), AP_Zahlung (C 100) (D I Mai 2021)),
      testCase "Gutschrift" $ do
        waup (mustermann, Gutschrift (C 100) jan1)  @?= (mustermann, P_Gutschrift (C 100) jan1),
      testCase "Gutschrift 31. April" $ do
        waup (mustermann, Gutschrift (C 100) (D XXXI Apr 2021))  @?= (mustermann, P_Gutschrift (C 100) (D I Mai 2021)),
      testCase "1999 kein Schaltjahr" $ do
        waup (mustermann, Gutschrift (C 100) (D XXIX Feb 1999))  @?= (mustermann, P_Gutschrift (C 100) (D I Mar 1999)),
      testCase "1996 Schaltjahr" $ do
        waup (mustermann, Gutschrift (C 100) (D XXIX Feb 1996))  @?= (mustermann, P_Gutschrift (C 100) (D XXIX Feb 1996)),
      testCase "1900 kein Schaltjahr" $ do
        waup (mustermann, Gutschrift (C 100) (D XXIX Feb 1900))  @?= (mustermann, P_Gutschrift (C 100) (D I Mar 1900)),
      testCase "2000 Schaltjahr" $ do
        waup (mustermann, Gutschrift (C 100) (D XXIX Feb 2000))  @?= (mustermann, P_Gutschrift (C 100) (D XXIX Feb 2000))
    ]
