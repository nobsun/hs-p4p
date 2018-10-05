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
grp1 :: [[Cap]]
grp1 = ["FF","BBB","F","BBB","FF","B","F"]
grp2 :: [[Cap]]
grp2 = ["FF","BBB","F","BBB","FFFF"]
rngs1 :: [Range]
rngs1 = [(0,1),(2,4),(5,5),(6,8),(9,10),(11,11),(12,12)]
rngs2 :: [Range]
rngs2 = [(0,1),(2,4),(5,5),(6,8),(9,12)]
rng1 :: Range
rng1 = (2, 4)
rng2 :: Range
rng2 = (11, 11)

-- テストコード
spec :: Spec
spec = do
  { mkCmdSpec
  ; pickupRangesSpec
  ; makeRangesSpec
  ; groupSpec
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

pickupRangesSpec :: Spec
pickupRangesSpec = describe "pickupRanges" $ do
  { it "can pick up ranges every 2nd" $ do
    { pickupRanges rngs1 `shouldBe` [(2,4),(6,8),(11,11)]
    ; pickupRanges rngs2 `shouldBe` [(2,4),(6,8)]
    }
  }

makeRangesSpec :: Spec
makeRangesSpec = describe "makeRanges" $ do
  { it "can convert from groups to ranges" $ do
    { makeRanges grp1 `shouldBe` [(0,1),(2,4),(5,5),(6,8),(9,10),(11,11),(12,12)]
    ; makeRanges grp2 `shouldBe` [(0,1),(2,4),(5,5),(6,8),(9,12)]
    }
  }

groupSpec :: Spec
groupSpec = describe "group" $ do
  { it "can group cap sequence" $ do
    { group cap1 `shouldBe` ["FF","BBB","F","BBB","FF","B","F"]
    ; group cap2 `shouldBe` ["FF","BBB","F","BBB","FFFF"]
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
