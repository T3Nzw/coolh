module Parser.Core
  ( Parser
  , runParser
  , item
  , sat
  , but
  , many
  , many1
  , oneOf
  , notOneOf
  , zeroOrOne
  , char
  , string
  , label
  , peek
  , lookahead
  , Parser.Core.until
  , untilPlus
  , Parser.Core.traverse
  , tryCatch
  , eof
  , inspect
  , chain1
  , choice
  , (<+>)
  )
where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (MonadPlus)

import qualified Control.Applicative as App
import qualified Data.Map as M

import Data.Sizeable (Sizeable (..))
import Data.Stream (Stream (..), take)

import qualified Parser.Position as Pos

newtype Parser e s a
  = MkParser {runParser :: Pos.State s -> Either e (a, Pos.State s)}

instance Functor (Parser e s) where
  fmap f (MkParser p) = MkParser $ \inp ->
    case p inp of
      Left err -> Left err
      Right (value, st) -> Right (f value, st)

instance Applicative (Parser e s) where
  pure value = MkParser $ \inp -> Right (value, inp)
  MkParser pf <*> MkParser p = MkParser $ \inp -> do
    (f, st1) <- pf inp
    (x, st2) <- p st1
    Right (f x, st2)

instance Monad (Parser e s) where
  return = pure
  MkParser p >>= f = MkParser $ \inp -> do
    (x, st1) <- p inp
    let (MkParser p') = f x
    p' st1

-- The error type has to be a Monoid instance
-- because empty needs a "default value" for
-- the parser, and it's impossible to create
-- a default value for just any type.
-- There are probably other, much more elegant
-- ways to do this, but oh well
instance Monoid e => App.Alternative (Parser e s) where
  empty = MkParser . const $ Left mempty

  -- FCFS
  MkParser lp <|> MkParser rp =
    MkParser $ \inp ->
      case (lp inp, rp inp) of
        (Right val, _) -> Right val
        (_, rhs) -> rhs

instance Monoid e => MonadPlus (Parser e s)

-- NOTE: it's likely i won't be needing this at all
instance (Monoid e, Read e) => MonadFail (Parser e s) where
  fail msg = MkParser . const $ Left $ read msg

-- | Run both parsers and apply maximal munch + FCFS strategies
(<+>)
  :: (Monoid e, Sizeable a, Stream s) => Parser e s a -> Parser e s a -> Parser e s a
MkParser lp <+> MkParser rp = MkParser $ \inp ->
  case (lp inp, rp inp) of
    (Right lhs@(val1, _), Right rhs@(val2, _)) ->
      Right $ if size val1 >= size val2 then lhs else rhs
    (Right lhs, _) -> Right lhs
    (_, rhs) -> rhs

infixl 3 <+>

-- | Consume the head of a non-empty stream unconditionally
item :: (Monoid e, Stream s) => (Element s -> a) -> Parser e s a
item f = MkParser $ \inp ->
  case uncons (inp ^. Pos.input) of
    Nothing -> Left mempty
    Just (h, t) -> Right (f h, computeOffset (inp & Pos.input .~ t) h)

-- | Consume if the head of a non-empty stream satisfies the predicate
sat :: (Monoid e, Stream s) => (Element s -> Bool) -> Parser e s (Element s)
sat p = MkParser $ \inp ->
  case uncons (inp ^. Pos.input) of
    Just (h, t) | p h -> Right (h, computeOffset (inp & Pos.input .~ t) h)
    _ -> Left mempty

-- | Consume if the head of a non-empty stream does not satisfy the predicate
but :: (Monoid e, Stream s) => (Element s -> Bool) -> Parser e s (Element s)
but p = sat (not . p)

-- | Kleene star
many :: (Monoid e, Stream s) => Parser e s a -> Parser e s [a]
many p = many1 p App.<|> pure []

-- | Kleene plus
many1 :: (Monoid e, Stream s) => Parser e s a -> Parser e s [a]
many1 p = do
  x <- p
  xs <- many p
  pure $ x : xs

choice :: (Monoid e, Stream s) => [Parser e s a] -> Parser e s a
choice = App.asum

-- | Consume if the head of a non-empty stream is an element of the range
oneOf
  :: (Eq (Element s), Monoid e, Stream s) => [Element s] -> Parser e s (Element s)
oneOf = foldr (\x -> (App.<|>) $ sat (== x)) App.empty

notOneOf
  :: (Eq (Element s), Monoid e, Stream s) => [Element s] -> Parser e s (Element s)
-- TODO: implement
notOneOf = undefined

-- | Apply the parser zero or one times
zeroOrOne :: (Monoid e, Stream s) => Parser e s a -> Parser e s (Maybe a)
zeroOrOne p = pure <$> p App.<|> pure Nothing

-- | Consume the head of a non-empty stream if it matches the given element
char
  :: (Eq (Element s), Monoid e, Stream s) => Element s -> Parser e s (Element s)
char x = sat (== x)

traverse
  :: (Monoid e, Stream s)
  => (Element s -> Parser e s (Element s)) -> s -> Parser e s s
traverse p s = case uncons s of
  Nothing -> pure empty
  Just (h, t) -> liftA2 cons (p h) (Parser.Core.traverse p t)

-- | Consume a sequence of stream elements if they match
string :: (Eq (Element s), Monoid e, Stream s) => s -> Parser e s s
string = Parser.Core.traverse char

-- | Inject an error label into the parser
label :: (Monoid e, Stream s) => e -> Parser e s a -> Parser e s a
label err (MkParser p) = MkParser $ \inp ->
  case p inp of
    Left _ -> Left err
    Right val -> Right val

-- | Inspect the first element of a non-empty stream
-- without consuming any input
peek :: (Monoid e, Stream s) => Parser e s (Element s)
peek = MkParser $ \inp ->
  case uncons (inp ^. Pos.input) of
    Nothing -> Left mempty
    Just (h, _) -> Right (h, inp)

lookahead :: (Monoid e, Stream s) => Parser e s a -> Parser e s (Maybe a)
lookahead (MkParser p) = MkParser $ \inp ->
  case p inp of
    Left _ -> Right (Nothing, inp)
    Right (res, _) -> Right (Just res, inp)

-- | Consume all characters uing the first parser until the second
-- parser succeeds
until
  :: (Monoid e, Stream s) => Parser e s a -> Parser e s end -> Parser e s [a]
until p end = MkParser $ \inp ->
  case runParser end inp of
    Right (_, t) -> Right ([], t)
    Left _ -> do
      (x, t1) <- runParser p inp
      (xs, t2) <- runParser (Parser.Core.until p end) t1
      pure (x : xs, t2)

untilPlus
  :: (Monoid e, Stream s) => Parser e s a -> Parser e s a -> Parser e s [a]
untilPlus p end = MkParser $ \inp ->
  case runParser end inp of
    Right (val, t) -> Right ([val], t)
    Left _ -> do
      (x, t1) <- runParser p inp
      (xs, t2) <- runParser (untilPlus p end) t1
      pure (x : xs, t2)

tryCatch :: (Monoid e, Stream s) => Parser e s a -> Parser e s (Either e a)
tryCatch p = MkParser $ \inp ->
  case runParser p inp of
    Left err -> Right (Left err, inp)
    Right (val, st) -> Right (Right val, st)

-- | Succeed if there is no more input to be consumed,
-- otherwise fail with a custom error
eof :: (Monoid e, Stream s) => e -> Parser e s ()
eof eofErr = MkParser $ \inp ->
  case uncons (inp ^. Pos.input) of
    Nothing -> Right ((), inp)
    Just _ -> Left eofErr

-- | Inspect what was read by a parser that succeeded
inspect :: (Monoid e, Stream s) => Parser e s a -> Parser e s (a, s)
inspect p = MkParser $ \inp -> do
  let abso1 = inp ^. Pos.pos . Pos.absOffset
  case runParser p inp of
    Left err -> Left err
    Right (val, st) ->
      let abso2 = st ^. Pos.pos . Pos.absOffset
       in Right ((val, Data.Stream.take (abso2 - abso1) $ inp ^. Pos.input), st)

-- precedence parsing combinator, inspired by pratt parsing
chain1Rhs :: (Monoid e, Ord repr, Stream s) => (s -> repr) -> M.Map repr (Int, Int) -> (Int, Int) -> a -> Parser e s a -> Parser e s (a -> a -> a) -> Parser e s a
chain1Rhs convert table (prevLbp, prevRbp) lhs atomp opp = do
  operation <- lookahead $ inspect opp
  case operation of
    Nothing -> pure lhs
    Just (op, sOp) ->
      case M.lookup (convert sOp) table of
        Nothing -> App.empty
        Just (lbp, rbp)
          -- TODO: handle non-associative operators
          | prevRbp < lbp -> do
              _ <- opp
              rhs <- chain1 convert table (lbp, rbp) atomp opp
              chain1Rhs convert table (prevLbp, prevRbp) (lhs `op` rhs) atomp opp
          | otherwise -> pure lhs

chain1 :: (Monoid e, Ord repr, Stream s) => (s -> repr) -> M.Map repr (Int, Int) -> (Int, Int) -> Parser e s a -> Parser e s (a -> a -> a) -> Parser e s a
chain1 convert table bp atompp opp = do
  lhs <- atompp
  chain1Rhs convert table bp lhs atompp opp
