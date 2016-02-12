module ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "Primitive parsers" $
       do describe "result" $
            it "succeeds without consuming any of the input string" $
            property $
            \x xs -> result x xs `shouldBe` ([(x,xs)] :: [(Char,String)])
          describe "zero" $
            it "always fails regardless of the input string" $
            property $ \xs -> (zero xs :: [(Char,String)]) `shouldSatisfy` null
          describe "item" $
            it "successfully consumes the first character" $
            property $
            \xs ->
              length xs >
              0 ==> item xs `shouldBe` ([(head xs,tail xs)] :: [(Char,String)])
     describe "Combinators" $
       do describe ">>=" $
            it "sequences two parsers" $
            (item `bind` \x -> item `bind` \y -> result (x,y)) "2+2" `shouldBe`
            [(('2','+'),"2")]
          describe "sat" $
            do describe "char" $
                 do it "consumes if the 1st char equals given then given char" $
                      char '+' "+1" `shouldBe` [('+',"1")]
                    it "else it does not consumes the input" $
                      char '+' "1" `shouldBe` ([] :: [(Char,String)])
               describe "digit" $
                 do it "consumes if the 1st char is digit" $
                      digit "1+1" `shouldBe` [('1',"+1")]
                    it "else it does not consumes the input" $
                      digit "+1" `shouldBe` []
               describe "lower" $
                 do it "consumes if the 1st char is a lower case letter" $
                      lower "abc" `shouldBe` [('a',"bc")]
                    it "else it does not consumes the input" $
                      lower "1" `shouldBe` []
               describe "upper" $
                 do it "consumes if the 1st char is an upper case letter" $
                      upper "Abc" `shouldBe` [('A',"bc")]
                    it "else it does not consumes the input" $
                      upper "bc" `shouldBe` []
          describe "plus" $
            do describe "letter" $
                 do it "consumes if the 1st char is a letter" $
                      do letter "Abc" `shouldBe` [('A',"bc")]
                         letter "abc" `shouldBe` [('a',"bc")]
                    it "else it does not consumes the input" $
                      letter "4bc" `shouldBe` []
               describe "alphanum" $
                 do it "consumes if the 1st char is an alphanumeric" $
                      do alphanum "Abc" `shouldBe` [('A',"bc")]
                         alphanum "abc" `shouldBe` [('a',"bc")]
                         alphanum "4bc" `shouldBe` [('4',"bc")]
                    it "else it does not consumes the input" $
                      alphanum "(bc" `shouldBe` []
               describe "word" $
                 do it "consumes the input if it finds a word" $
                      word "Yes!" `shouldBe`
                      [("Yes","!"),("Ye","s!"),("Y","es!"),("","Yes!")]
                    it "else it does not consumes the input" $
                      word "!" `shouldBe` [("","!")]
     describe "Combinators for repetition" $
       do describe "string" $
            it "parses specific string if it's present in input" $
            do string "Yes" "Yes!" `shouldBe` [("Yes","!")]
               string "No" "Yes!" `shouldBe` []
               string "" "Yes!" `shouldBe` [("","Yes!")]
          describe "ident" $
            it "parses identifier" $
            ident "index = 1" `shouldBe`
            [("index"," = 1")
            ,("inde","x = 1")
            ,("ind","ex = 1")
            ,("in","dex = 1")
            ,("i","ndex = 1")]
          describe "nat" $
            it "parses a natural number" $
            nat "123 + 1" `shouldBe` [(123," + 1"),(12,"3 + 1"),(1,"23 + 1")]
          describe "int" $
            it "parses an integer" $
            do int "123" `shouldBe` [(123,""),(12,"3"),(1,"23")]
               int "-123" `shouldBe` [(-123,""),(-12,"3"),(-1,"23")]
          describe "ints" $
            it "parses list of integers" $
            do ints "[1]" `shouldBe` [([1],"")]
               ints "[1,2,3]" `shouldBe` [([1,2,3],"")]
               ints "[-1,2,-3,0]" `shouldBe` [([-1,2,-3,0],"")]
          describe "expr" $
            it "evaluates an arithmetic expression" $
            do expr "(1+1)" `shouldBe` [(2,"")]
               expr "1-2+3-4" `shouldBe`
                 [(-2,""),(2,"-4"),(-1,"+3-4"),(1,"-2+3-4")]
               expr "1+2-(3+4)" `shouldBe`
                 [(-4,""),(3,"-(3+4)"),(1,"+2-(3+4)")]
               expr "1+3^2-(1+4)" `shouldBe`
                 [(5,""),(10,"-(1+4)"),(4,"^2-(1+4)"),(1,"+3^2-(1+4)")]
     describe "Handling lexical issues" $
       do describe "spaces" $
            it "consumes white spaces from the beginning of input" $
            spaces "  test" `shouldBe` [((),"test"),(()," test")]
          describe "comment" $
            do it "consumes single line comments" $
                 comment "-- xxx" `shouldBe`
                 [((),""),((),"x"),((),"xx"),((),"xxx"),(()," xxx")]
               it "consumes multiline comments" $
                 comment "{- x\n  x -} rest = 1" `shouldBe` [(()," rest = 1")]
          describe "junk" $
            it "discards spaces and comments" $
            junk "-- xxx \n {- xx -} x = 2" `shouldBe`
            [((),"x = 2")
            ,(()," x = 2")
            ,((),"{- xx -} x = 2")
            ,((),"\n {- xx -} x = 2")
            ,((),"-- xxx \n {- xx -} x = 2")]
          describe "tokens" $
            do it "parses a natural number" $
                 natural "1 -- test" `shouldBe`
                 [(1,""),(1,"-- test"),(1," -- test")]
               it "parses an integer" $
                 integer "-1 -- test" `shouldBe`
                 [(-1,""),(-1,"-- test"),(-1," -- test")]
               it "parses a symbol" $
                 symbol "var" "var x = 1" `shouldBe`
                 [("var","x = 1"),("var"," x = 1")]
               it "will not parse as identifier a reserved keyword" $
                 identifier ["var"]
                            "var x = 1" `shouldBe`
                 [("","x = 1"),(""," x = 1"),("va","r x = 1"),("v","ar x = 1")]
               it "will parse an identifier that is not a reserved keyword" $
                 identifier ["var"]
                            "x = 1" `shouldBe`
                 [("x","= 1"),("x"," = 1")]
          describe "parser for lambda expressions" $
            it "parses an expression" $
            lambdaExpr "\\x -> succ x" `shouldBe`
            [(Lam "x"
                  (App (Var "succ")
                       (Var "x"))
             ,"")]
