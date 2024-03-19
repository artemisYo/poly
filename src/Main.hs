{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monomer
import Data.Text
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

data State = State
    { keys :: [(Text, Event)]
    , textSz :: Double
    } deriving (Eq)

defaultState :: State
defaultState = State
    { keys =
        [ ("q", Exit)
        , ("s", TextSmaller)
        , ("b", TextBigger)
        ]
    , textSz = 50
    }

data Event
    = Init
    | Exit
    | TextSmaller
    | TextBigger
    deriving (Eq)

type Env = WidgetEnv State Event
type Node = WidgetNode State Event

textNode :: Double -> Text -> Node
textNode size t =
    label_ t [ multiline ]
    `styleBasic` [ textFont "default", textSize size, textColor black ]

view :: Text -> Env -> State -> Node
view t env state =
    ( keystroke [("q", Exit)]
    $ textNode (textSz state) t
    ) `nodeFocusable` True

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
        TextSmaller -> [ Model smallerText ]
        TextBigger -> [ Model biggerText ]
  where
    smallerText = state { textSz = textSz state - 4 }
    biggerText = state { textSz = textSz state + 4 }

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
    startApp defaultState update (view ct) config
