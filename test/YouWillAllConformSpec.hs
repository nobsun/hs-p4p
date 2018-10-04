module YouWillAllConformSpec
  ( main
  , spec
  ) where
  
import Test.Hspec
import YouWillAllConform

main :: IO ()
main = hspec spec

-- テスト入力データ
cap1 :: [Cap]
cap1 = "FFBBBFBBBFFBF"
cap2 :: [Cap]
cap2 = "FFBBBFBBBFFFF"
range1 :: Range
range1 = (2, 4)
range2 :: Range
range2 = (11, 11)

-- テストコード
spec :: Spec
spec = do
  { describe "mkCmd"             $ do
    { context ("when provided with " ++ show range1) $ do
      { it "can make a command from a range containing more than one positions" $ do
        { mkCmd range1 `shouldBe` "2番目から4番目の人は帽子の向きを替えてください" }
      }
    ; context ("when provided with " ++ show range2) $ do
      { it "can make a command from a singleton range" $ do
        { mkCmd range2 `shouldBe` "11番目の人は帽子の向きを替えてください" }
      }
    }
  ; describe "pleaseConform"     $ do
    { it "can make commands people to conform their cap" $ do
      { pleaseConform cap1 `shouldBe` [ "2番目から4番目の人は帽子の向きを替えてください"
                                      , "6番目から8番目の人は帽子の向きを替えてください"
                                      , "11番目の人は帽子の向きを替えてください"
                                      ]
      ; pleaseConform cap2 `shouldBe` [ "2番目から4番目の人は帽子の向きを替えてください"
                                      , "6番目から8番目の人は帽子の向きを替えてください"
                                      ]
      }
    }
  } 
