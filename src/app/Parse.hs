module Parse (Parser, Result (..), Pair (..), pair, alt, star, discard) where

import Control.Monad ((>=>))

data Result e a = Ok a | Err e

instance Functor (Result e) where
  fmap f (Ok a) = Ok (f a)
  fmap f (Err e) = Err e

instance Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok a) = Ok (f a)
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e

instance Monad (Result e) where
  (Ok a) >>= f = f a
  (Err e) >>= _ = Err e

infixr 0 :>

data Pair a b = a :> b

type Parser e a b = a -> Result e (a, b)

-- | Parse with `p`, then with `q` returning both results in a `Pair`.
--   Right-associative, works well with `:>` destructuring
pair :: Parser e t a -> Parser e t b -> Parser e t (Pair a b)
pair p q t = do
  (t', a) <- p t
  (t'', b) <- q t'
  Ok (t'', a :> b)

infixr 0 `pair`

-- | Match the first parser, then discard the result and move onto the next
--   parser
discard :: Parser e t a -> Parser e t b -> Parser e t b
discard p q t = do
  (t', _ :> b) <- (p `pair` q) t
  Ok (t', b)

infixr 0 `discard`

-- | Parse with the first successful parser. Must contain at least one parser.
alt :: [Parser e t a] -> Parser e t a
alt [] t = error "alt must contain at least one parser"
alt [p] t = p t
alt (p : ps) t = case p t of
  Err _ -> alt ps t
  ok -> ok

-- | An always succesful parser, repeat `p` 0 or more times sequentially.
-- `Ok . star p` will give you a `Parser` type.
star :: Parser e t a -> (t -> (t, [a]))
star p t = case p t of
  Ok (t', a) ->
    let (t'', as) = star p t'
     in (t'', a : as)
  Err _ -> (t, [])