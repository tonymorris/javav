module Main where

import Data.Function(($))
import Data.List((++), null)
import Data.Foldable(mapM_)
import Data.Functor(Functor(fmap))
import Language.Java.Javav(alreadyInUseError, doesNotExistError, permissionError, unknownError, printv)
import System.Environment(getArgs)
import System.IO(IO, putStrLn)

main ::
  IO ()
main =
  do a <- getArgs
     if null a
        then mapM_ putStrLn $
                     [
                      "Prints the Java version of one or more class files"
                     , "Usage: javav <filename(s)>"
                     ] ++
                     ("  " ++) `fmap`
                       [
                        "Error conditions"
                       , alreadyInUseError ++ " File is in use"
                       , doesNotExistError ++ " File does not exist"
                       , permissionError ++ " File does not have read permission"
                       , unknownError ++ " An unknown error occurred"
                       ]
        else printv a

