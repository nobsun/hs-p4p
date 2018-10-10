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
rng1 :: Range
rng1 = (2, 4)
rng2 :: Range
rng2 = (11, 11)

-- テストコード
spec :: Spec
spec = do
  { mkCmdSpec
  ; pleaseConformSpec
  }

mkCmdSpec :: Spec
mkCmdSpec = describe "mkCmd" $ do
  { context ("when provided with " ++ show rng1) $ do
    { it "can make a command from a range containing more than one positions" $ do
      { mkCmd rng1 `shouldBe` "2番目から4番目の人は帽子の向きを替えてください" }
    }
  ; context ("when provided with " ++ show rng2) $ do
    { it "can make a command from a singleton range" $ do
      { mkCmd rng2 `shouldBe` "11番目の人は帽子の向きを替えてください" }
    }
  }

pleaseConformSpec :: Spec
pleaseConformSpec = describe "pleaseConform" $ do
  { it "can make commands people to conform their caps" $ do
    { pleaseConform cap1 `shouldBe` [ "2番目から4番目の人は帽子の向きを替えてください"
                                    , "6番目から8番目の人は帽子の向きを替えてください"
                                    , "11番目の人は帽子の向きを替えてください"
                                    ]
    ; pleaseConform cap2 `shouldBe` [ "2番目から4番目の人は帽子の向きを替えてください"
                                    , "6番目から8番目の人は帽子の向きを替えてください"
                                    ]
    }
  }
