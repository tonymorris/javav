module Language.Java.Javav where

import Data.Bool(Bool(True))
import Data.List(lookup, zip, (++))
import Data.Char(Char)
import Data.String(String)
import Data.Foldable(mapM_)
import Data.Function((.), ($))
import Data.Functor(Functor(fmap))
import Data.Traversable(mapM)
import System.IO(IO, FilePath, hGetChar, putStrLn, hSeek, withBinaryFile, IOMode(ReadMode), SeekMode(AbsoluteSeek))
import System.IO.Error(IOError, isDoesNotExistError, isAlreadyInUseError, isPermissionError)
import Data.Maybe(fromMaybe)
import Control.Arrow(Arrow(first))
import Control.Monad(Monad(return))
import Control.Exception(catch)

type Version =
  String

versions ::
  [((Char, Char), Version)]
versions =
  [
    (('\NUL', '2'), "1.6 (50.0 0x32)")
  , (('\NUL', '1'), "1.5 (49.0 0x31)")
  , (('\NUL', '0'), "1.4 (48.0 0x30)")
  , (('\NUL', '/'), "1.3 (47.0 0x29)")
  , (('\NUL', '.'), "1.2 (46.0 0x28)")
  , (('\NUL', '-'), "1.1 (45.0 0x28)")
  ]

type Error =
  String

alreadyInUseError ::
  Error
alreadyInUseError =
  "-u-"

doesNotExistError ::
  Error
doesNotExistError =
  "-e-"

permissionError ::
  Error
permissionError =
  "-p-"

unknownError ::
  Error
unknownError =
  "-?-"

handleError ::
  IOError
  -> Error
handleError e =
  let err = first ($e) `fmap`
              [
                (isAlreadyInUseError, alreadyInUseError)
              , (isDoesNotExistError, doesNotExistError)
              , (isPermissionError  , permissionError)
              ]
  in unknownError `fromMaybe` lookup True err

versionOutput ::
  Char
  -> Char
  -> Version
versionOutput c1 c2 =
  unknownError `fromMaybe` lookup (c1, c2) versions

version ::
  FilePath
  -> IO Version
version n =
  withBinaryFile n ReadMode (\h ->
    do hSeek h AbsoluteSeek 6
       c1 <- hGetChar h
       c2 <- hGetChar h
       return (versionOutput c1 c2)) `catch`
  (return . handleError)

version' ::
  [FilePath]
  -> IO [Version]
version' =
  mapM version

printv ::
  [FilePath]
  -> IO ()
printv n =
  do s <- version' n
     (\(t, o) -> putStrLn (t ++ ' ' : o)) `mapM_ ` zip s n
