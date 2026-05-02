module ScannerSpec (spec) where
    import Test.Hspec
    import Scanner

    spec :: Spec
    spec = do
        describe "empty string and whitespace" $ do
            it "handles empty string" $  
                scan "" `shouldBe` []
            it "handles a space" $
                scan " " `shouldBe`[]
            it "handles a tab" $
                scan "  " `shouldBe` []
            it "handles a newline" $
                scan "\n" `shouldBe` []
        
        describe "keywords" $ do
            it "handles else" $ do
                scan "else" `shouldBe` [TElse]
            it "handles if" $ do
                scan "if" `shouldBe` [TIf]
        
        describe "ids" $ do
            it "tokenizes one id" $ do
                scan "main" `shouldBe` [TId "main"]
            it "tokenizes multiple ids" $ do
                scan "i love haskell" `shouldBe` [TId "i", TId "love", TId "haskell"]
            it "tokenizes two ids, with an unexpected char in the middle" $ do
                scan "test$ing" `shouldBe` [TId "test", Error "Unexpected character: $", TId "ing"]

        describe "nums" $ do
            it "scans one int" $ do
                scan "596" `shouldBe` [TNum 596]
            it "scans multiple ints" $ do
                scan "123 45" `shouldBe` [TNum 123, TNum 45]
        
        describe "removeFirstId tests" $ do
            it "removes the id supplied with no whitespace" $ do
                removeFirstId "test" "testing" `shouldBe` "ing"
            it "removes the id supplied with whitespace" $ do
                removeFirstId "test" "test ing" `shouldBe` " ing"

        describe "example" $ do
            it "assignment example" $ do
                scan "void main (void) /*Main function*/" `shouldBe`
                    [TVoid, TId "main", TLParen, TVoid, TRParen]
            it "ids keywords test" $ do
                scan "else ifint" `shouldBe` [TElse, TId "ifint"]
            it "keywords nums test" $ do
                scan "else if200" `shouldBe` [TElse, TId "if200"]

