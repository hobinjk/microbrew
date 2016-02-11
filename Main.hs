{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import qualified Data.Text as T
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Control.Monad
import qualified System.IO as SIO

data Installation = Installation {getPath :: FilePath, isInstalled :: Bool} deriving (Eq, Show)

parseInstallations :: Text -> [Installation]
parseInstallations line
  | T.null line = []
  | T.head line == '/' = let path = (fromText . head . T.words) line
                             installed = (last . T.words) line == "*"
                        in [Installation path installed]
  | otherwise = []

getPathText :: FilePath -> Text
getPathText path = let Right realPath = toText path in realPath

removeExtraInstallation :: Installation -> Shell ()
removeExtraInstallation (Installation _ True) = empty
-- removeExtraInstallation (Installation path False) = liftIO $ echo $ case toText path of Right p -> p
--                                                                                         Left  errorMessage -> errorMessage
removeExtraInstallation (Installation path False) = liftIO $ do
                                                                echo $ T.concat ["Removing ", getPathText path]
                                                                rmtree path

isYesAnswer :: Text -> Bool
isYesAnswer answer
  | T.null answer = False
  | T.head answer == 'y' = True
  | T.head answer == 'Y' = True
  | otherwise = False

showRemovePrompt :: Installation -> [Installation] -> Shell ()
-- this is an absolutely disgusting way of circumventing different Monads and
-- there is probably a better way to do this.
showRemovePrompt _ [] = empty
showRemovePrompt firstInstalled obsoleteInstallations = do
  response <- liftIO $ do
                 echo $ T.concat [getPathText $ getPath firstInstalled, " obsoletes:"]
                 mapM_ (echo . getPathText . getPath) obsoleteInstallations
                 putStr "Proceed with deletion [yN]?"
                 getLine
  if isYesAnswer $ T.pack response
     then mapM_ removeExtraInstallation obsoleteInstallations
     else empty

getInstallations :: Text -> Shell [Installation]
getInstallations program = liftM parseInstallations (inshell (T.concat ["brew info ", program]) empty)

removeExtraInstallations :: Text -> Shell ()
removeExtraInstallations program = do
  installations <- liftIO $ fold (getInstallations program) Fold.mconcat
  case filter isInstalled installations of
        []  -> empty -- liftIO $ echo $ T.concat ["The installation of ", T.pack (show program), " is nonstandard: skipping"]
        firstInstalled:_ -> showRemovePrompt firstInstalled $ filter (not . isInstalled) installations

main :: IO ()
main = do
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  sh $ do
    brewList <- inshell "brew list" empty
    mapM_ removeExtraInstallations $ T.lines brewList
