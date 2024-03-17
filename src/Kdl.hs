{-# LANGUAGE OverloadedStrings #-}

module Kdl where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Applicative (Alternative, empty, (<|>), some, many)
import Control.Monad (MonadPlus)
import Data.Foldable (find)

type Document = [Node]

type Ident = Text

data Value
  = Str Text
  | Num Double
  | Bool Bool
  | Null

data Node = Node
  { name  :: Ident
  , args  :: [Value]
  , props :: Map.Map Ident Value
  , children :: [Node]
  }

parse_doc :: Text -> Document
parse_doc _ = []

voidP :: (Monad m) => Parser i m o -> Parser i m ()
voidP = fmap void
void :: a -> ()
void _ = ()

linespace :: Parser Text Maybe ()
linespace = voidP newlineP <|> ws <|> singleLineComment

newlineP :: Parser Text Maybe Char
newlineP = Parser $ startsPred newline
newline :: Char -> Bool
newline = flip elem
  [ '\x000d', '\x000a' -- CRLF is skipped as that only changes how
  , '\x0085', '\x000c' --  many newlines it produces and idc that
  , '\x2028', '\x2029'
  ]

ws :: Parser Text Maybe ()
ws = bom <|> unicodeSpace <|> multiLineComment

singleLineComment :: Parser Text Maybe ()
singleLineComment = voidP $ start *> middle *> end
  where
    start = Parser $ startsText "//"
    middle = some $ Parser $ startsPred $ not . newline
    end = newlineP <|> eof

multiLineComment :: Parser Text Maybe ()
multiLineComment = start *> commentedBlock
  where
    start = Parser $ startsText "/*"

commentedBlock :: Parser Text Maybe ()
commentedBlock = end <|> (nest *> commentedBlock)
  where
    nest  = multiLineComment <|> star <|> slash <|> rest
    rest  = voidP $ some $ Parser $ startsPreds notEnd
    star  = voidP $ Parser $ startsChar '*'
    slash = voidP $ Parser $ startsChar '/'
    end   = voidP $ Parser $ startsText "*/"
    notEnd t = if not $ Text.isPrefixOf "*/" t
      then Just 1
      else Nothing -- this might be inefficient as fuck

bom :: Parser Text Maybe ()
bom = voidP $ Parser $ startsChar '\xfeff'

unicodeSpace :: Parser Text Maybe ()
unicodeSpace = voidP $ Parser $ startsPred $ flip elem
  [ '\x0009', '\x0020'
  , '\x00a0', '\x1680'
  , '\x2000', '\x2001'
  , '\x2002', '\x2003'
  , '\x2004', '\x2005'
  , '\x2006', '\x2007'
  , '\x2008', '\x2009'
  , '\x200a', '\x202f'
  , '\x205f', '\x3000'
  ]

eof = fail ""

-- | Right to left function composition, allowing the right function to be applied twice
infixr 9 .:
{-# INLINE (.:) #-}
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f = \a b -> g $ f a b

f2map :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
f2map = fmap . fmap

data Parser i m o = (Monad m) => Parser
  { run :: i -> m (i, o) }

instance (Monad m) => Functor (Parser i m) where
  fmap :: (o -> r) -> Parser i m o -> Parser i m r
  fmap f = Parser . f2map f .: run

-- instance (Monad m) => Bifunctor (Parser i) where
--   bimap :: (Monad n) => (m -> n) -> (o -> r) -> Parser i m o -> Parser i n r
--   bimap f g p = Parser $ f . fmap g . run p

instance (Monad m) => Applicative (Parser i m) where
  pure :: o -> Parser i m o
  pure = Parser . pure .: flip (,)
  (<*>) :: Parser i m (o -> r) -> Parser i m o -> Parser i m r
  (<*>) p q = Parser $ \i -> do
      (r, f) <- run p i
      (s, o) <- run q r
      return (s, f o)

instance (Monad m) => Monad (Parser i m) where
  (>>=) :: Parser i m o -> (o -> Parser i m r) -> Parser i m r
  (>>=) p f = Parser $ \i -> do
    (r, o) <- run p i
    let q = f o
    run q r

instance (MonadFail m) => MonadFail (Parser i m) where
  fail :: String -> Parser i m o
  fail s = Parser $ \_ -> fail s

instance (Monad m, Alternative m) => Alternative (Parser i m) where
  empty :: Parser i m o
  empty = Parser $ \_ -> empty
  (<|>) :: Parser i m o -> Parser i m o -> Parser i m o
  (<|>) p q = Parser $ \i -> (run p i) <|> (run q i)

instance (Monad m, Alternative m) => MonadPlus (Parser i m) where

-- instance NatTrans (Parser i _ o) where
ntrans :: (Monad m, Monad n) => (m (i, o) -> n (i, o)) -> Parser i m o -> Parser i n o
ntrans f p = Parser $ \i ->  f $ run p i

-- | represents a natural transformation: a morphism over functors
infixl 4 ~>
(~>) :: (Monad m, Monad n) => (m (i, o) -> n (i, o)) -> Parser i m o -> Parser i n o
(~>) = ntrans

startsChar :: Char -> Text -> Maybe (Text, Char)
startsChar c = find ((c ==) . snd) . Text.unsnoc

startsText :: Text -> Text -> Maybe (Text, Text)
startsText a = fmap (a,) . Text.stripPrefix a

startsPred :: (Char -> Bool) -> Text -> Maybe (Text, Char)
startsPred p = find (p . snd) . Text.unsnoc

startsPreds :: (Text -> Maybe Int) -> Text -> Maybe (Text, Text)
startsPreds p t = flip Text.splitAt t <$> p t
