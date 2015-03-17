{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Control.Monad

data Installation = Installation FilePath Bool deriving (Eq, Show)

installationsFromInfo :: Text -> [Installation]
installationsFromInfo = installations . T.lines

installations :: [Text] -> [Installation]
installations (line:xs)
  | T.head line == '/' = let path = (fromText . head . T.words) line
                             installed = (last . T.words) line == "*"
                        in Installation path installed : installations xs
  | otherwise = installations xs
installations [] = []

removeExtraInstallation :: Installation -> Shell ()
removeExtraInstallation (Installation _ True) = empty
-- removeExtraInstallation (Installation path False) = liftIO $ echo $ case toText path of Right p -> p
--                                                                                         Left  errorMessage -> errorMessage
removeExtraInstallation (Installation path False) = let Right realPath = toText path
                                                     in liftIO $ do
                                                                echo $ T.concat ["Removing ", realPath]
                                                                rmtree path
-- 6.6 GiB start
removeExtraInstallations :: Text -> Shell ()
removeExtraInstallations program =
  liftM installationsFromInfo (inshell (T.concat ["brew info ", program]) empty) >>=
  mapM_ removeExtraInstallation

main :: IO ()
main = sh $ do
  brewList <- inshell "brew list" empty
  mapM_ removeExtraInstallations $ T.lines brewList
