import           Test.Disorder (disorderMain)

import qualified Test.IO.Entwine.Guard
import qualified Test.IO.Entwine.Loop
import qualified Test.IO.Entwine.Snooze


main :: IO ()
main =
  disorderMain [
      Test.IO.Entwine.Guard.tests
    , Test.IO.Entwine.Loop.tests
    , Test.IO.Entwine.Snooze.tests
    ]
