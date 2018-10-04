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

-- テストコード

spec :: Spec
spec = do
 { describe "pleaseConform"     $ do
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
