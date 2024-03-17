module Kdl where

import qualified Data.Map.Strict as Map
import Control.Applicative (Alternative, empty, (<|>))
import Data.Bifunctor (Bifunctor, bimap)
import Data.List (isPrefixOf, length)
import Control.Monad (MonadPlus)
import Data.Function ((&))
import Data.Tuple (swap)

type Document = [Node]

type Ident = String

data Value
  = Str String
  | Num Double
  | Bool Bool
  | Null

data Node = Node
  { name  :: Ident
  , args  :: [Value]
  , props :: Map.Map Ident Value
  , children :: [Node]
  }

parse_doc :: String -> Document
parse_doc _ = []

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

startsChar :: Char -> String -> Maybe (String, Char)
startsChar _ [] = Nothing
startsChar c (x:xs)
  | x == c = Just (xs, c)
  | otherwise = Nothing

startsString :: String -> String -> Maybe (String, String)
startsString a b = if a `isPrefixOf` b
  then Just (a, drop (length a) b)
  else Nothing
