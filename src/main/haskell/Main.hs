import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

import Partitions

main :: IO ()
main = do
  args <- Env.getArgs
  arg <- MaybeT.runMaybeT $
    let
      liftMaybe = MaybeT.MaybeT . return
    in do
      firstArg <- liftMaybe . Maybe.listToMaybe $ take 1 args
      liftMaybe . Read.readMaybe $ firstArg
  case arg of
    (Just arg') -> mapM_ print $ Set.toList . partitions $ arg'
    Nothing -> do
      IO.hPutStrLn IO.stderr $ "Please provide a valid number to partition."
      Exit.exitFailure
