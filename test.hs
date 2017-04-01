import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad

type Tester a = StateT Int IO a

nums = [1,5..41]

test :: Tester ()
test = do
    mapM (\a -> modify (+a)) nums
    y <- get
    lift $ putStrLn (show y)

main :: IO ()
main = do
    runStateT test 0
    return ()
