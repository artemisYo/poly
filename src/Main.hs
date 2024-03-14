{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monomer
import Data.Text
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

data State = State {} deriving (Eq)
data Event
    = Init
    | Exit
    deriving (Eq)

type Env = WidgetEnv State Event
type Node = WidgetNode State Event

textNode :: Text -> Node
textNode t = label t `styleBasic` [ textFont "default", textSize 50, textColor black ]

view :: Text -> Env -> State -> Node
view t env state = (keystroke [("q", Exit)] $ textNode t) `nodeFocusable` True

update
    :: Env
    -> Node
    -> State
    -> Event
    -> [AppEventResponse State Event]
update env node state event =
    case event of
        Init -> []
        Exit -> [ exitApplication ]

getFileName :: IO String
getFileName = do
  args <- getArgs
  case listToMaybe args of
    Nothing -> error "Please provide a file name!"
    Just a -> return a

main :: IO ()
main = do
    fl <- getFileName
    ct <- pack <$> readFile fl
    let config =
          [ appInitEvent Init
          , appFontDef "default" "./assets/GeistMono-Regular.otf"
          ]
    let nullState = State {}
    startApp nullState update (view ct) config
