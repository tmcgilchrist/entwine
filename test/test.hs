import           Test.Disorder (disorderMain)

import           Test.Entwine.Async
import           Test.Entwine.Data.Duration
import           Test.Entwine.Data.Finalizer
import           Test.Entwine.Data.Gate
import           Test.Entwine.Data.Parallel
import           Test.Entwine.Data.Pin
import           Test.Entwine.Data.Queue
import           Test.Entwine.Parallel

main :: IO ()
main =
  disorderMain [
      Test.Entwine.Async.tests
    , Test.Entwine.Data.Duration.tests
    , Test.Entwine.Data.Finalizer.tests
    , Test.Entwine.Data.Gate.tests
    , Test.Entwine.Data.Parallel.tests
    , Test.Entwine.Data.Pin.tests
    , Test.Entwine.Data.Queue.tests
    , Test.Entwine.Parallel.tests
    ]
