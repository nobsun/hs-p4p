module BestTimeToPartySpec
  ( main
  , spec
  ) where
  
import Test.Hspec
import BestTimeToParty

main :: IO ()
main = hspec spec

-- テスト入力データ
sched :: [Schedule]
sched = [(6,8),(6,12),(6,7),(7,8)
        ,(7,10),(8,9),(8,10),(9,12)
        ,(9,10),(10,11),(10,12),(11,12)
        ]

timing :: Timing
timing = (9, 5)

-- テストコード
spec :: Spec
spec = do
  { makeMessageSpec
  ; bestTimeToPartySpec
  }

makeMessageSpec :: Spec
makeMessageSpec = describe "makeMessage" $ do
  { it "can make a message from a pair of timing and a number of celebrities" $ do
    { makeMessage timing `shouldBe` "Best time to attend the party is at 9 o'clock : 5 celebrities will be attending!"
    }
  }

bestTimeToPartySpec :: Spec
bestTimeToPartySpec = describe "bestTimeToParty" $ do
  { it "can tell us the best time to party from a list of schedules" $ do
    { bestTimeToParty sched `shouldBe` "Best time to attend the party is at 9 o'clock : 5 celebrities will be attending!"
    }
  }
