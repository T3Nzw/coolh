module Lexer.Parser.Parser where

import Control.Applicative
import Control.Monad (void)
import Data.BSUtil (char8, controlToHex, isAsciiLower8, toLower8, word8)
import Data.ByteString (ByteString, length, pack, singleton, unpack)
import Data.Stream
import qualified Lexer.Error as LE
import Lexer.Lexer (Lexeme (..), LexemeTag (..), mkLexComment, mkLexError, mkLexStr)
import Lexer.Parser.Combinators
import qualified Parser.Core as P
import Prelude hiding (div)

-- Parser combinators for tokens.

type LParser = P.Parser LE.Error ByteString Lexeme

catlex :: Lexeme -> Lexeme -> Lexeme
catlex (Lexeme STR_CONST (Just s1)) (Lexeme STR_CONST (Just s2)) =
  Lexeme STR_CONST (Just $ s1 <> s2)
catlex lhs@(Lexeme (ERR _) _) _ = lhs
catlex _ rhs@(Lexeme (ERR _) _) = rhs
catlex lhs _ = lhs

-- parse directly as lexemes due to errors
strc :: P.Parser LE.Error ByteString Lexeme
strc = do
  h <- P.item id
  case word8 h of
    '\NUL' -> pure $ mkLexError LE.StringContainsNullCharacter
    '\n' -> pure $ mkLexError LE.StringContainsUnescapedNewLine
    '\t' -> pure $ mkLexStr "\t"
    '\b' -> pure $ mkLexStr "\b"
    '\f' -> pure $ mkLexStr "\f"
    '\\' -> do
      t <- P.zeroOrOne (P.item id)
      case t of
        Nothing -> pure $ mkLexError LE.UnknownError
        Just t' -> case word8 t' of
          '\NUL' -> pure $ mkLexError LE.StringContainsEscapedNullCharacter
          '\n' -> pure $ mkLexStr "\n"
          '\t' -> pure $ mkLexStr "\t"
          '\b' -> pure $ mkLexStr "\b"
          '\f' -> pure $ mkLexStr "\f"
          '"' -> pure $ mkLexStr "\""
          _ -> pure . mkLexStr . pack . map char8 $ controlToHex t'
    _ -> pure . mkLexStr . pack . map char8 $ controlToHex h

str :: P.Parser LE.Error ByteString Lexeme
str = do
  _ <- P.char (char8 '"')
  s <- P.tryCatch $ foldr1 catlex <$> untilP
  pure $ case s of
    Left _ -> mkLexError LE.UnterminatedStringAtEOF
    Right s' -> case s' of
      -- this is probably the only sane way to do this,
      -- as tracking things like absolute offset, etc,
      -- is not going to cut it due to escape sequences
      Lexeme STR_CONST (Just val) | Data.ByteString.length val > 1024 -> mkLexError (LE.StringConstantTooLong $ Data.ByteString.length val)
      _ -> s'
  where
    untilP =
      P.untilPlus
        strc
        ( (P.char (char8 '"') >> pure (mkLexStr ""))
            <|> (P.char (char8 '\n') >> pure (mkLexError LE.StringContainsUnescapedNewLine))
            <|> (eof >> pure (mkLexError LE.UnterminatedStringAtEOF))
        )

olcomment :: LParser
olcomment = do
  _ <- P.string "--"
  -- TODO: what error?
  _ <- P.until (P.item id) (newline <|> P.eof LE.UnknownError)
  pure mkLexComment

-- consume everything if the comment is not terminated
commentBegin :: LParser
commentBegin = P.string "(*" >> many (P.item id) >> pure (mkLexError LE.EOFInComment)

commentEnd :: LParser
commentEnd = P.string "*)" >> pure (mkLexError LE.UnmatchedCommentEnd)

comment :: LParser
comment = do
  _ <- P.string "(*"
  _ <- P.until (void comment <|> void (P.item id)) (P.string "*)")
  pure mkLexComment

comments :: LParser
comments = comment <|> commentBegin <|> commentEnd <|> olcomment

-- whoever decided that this is ok should be in jail
bool :: LParser
bool = Lexeme BOOL_CONST . Just . pack . map toLower8 . unpack <$> (t <|> f)
  where
    t = liftA2 cons (P.char $ char8 't') (stringCI "rue")
    f = liftA2 cons (P.char $ char8 'f') (stringCI "alse")

int :: LParser
int = Lexeme INT_CONST . Just . pack <$> P.many1 digit

objectid :: LParser
objectid = Lexeme OBJECTID . Just <$> liftA2 cons lower (fmap pack $ many $ alphanum <|> underscore)

typeid :: LParser
typeid = Lexeme TYPEID . Just <$> liftA2 cons upper (fmap pack $ many $ alphanum <|> underscore)

discard :: LexemeTag -> Char -> LParser
discard tag c = Lexeme tag . const Nothing . singleton <$> P.oneOf ulcases
  where
    ulcases = [char8 c, toLower8 (char8 c)]

discardS :: LexemeTag -> String -> LParser
discardS tag s = Lexeme tag . const Nothing . pack . map toLower8 . unpack <$> stringCI (pack $ map char8 s)

semicolon :: LParser
semicolon = discard SEMICOLON ';'

colon :: LParser
colon = discard COLON ':'

darrow :: LParser
darrow = discardS DARROW "=>"

lbrace :: LParser
lbrace = discard LBRACE '{'

rbrace :: LParser
rbrace = discard RBRACE '}'

lparen :: LParser
lparen = discard LPAREN '('

rparen :: LParser
rparen = discard RPAREN ')'

at :: LParser
at = discard AT '@'

comma :: LParser
comma = discard COMMA ','

dot :: LParser
dot = discard DOT '.'

add :: LParser
add = discard ADD '+'

sub :: LParser
sub = discard SUB '-'

mul :: LParser
mul = discard MUL '*'

div :: LParser
div = discard DIV '/'

tilda :: LParser
tilda = discard TILDA '~'

eq :: LParser
eq = discard Lexer.Lexer.EQ '='

le :: LParser
le = discardS LE "<="

lt :: LParser
lt = discard Lexer.Lexer.LT '<'

assign :: LParser
assign = discardS ASSIGN "<-"

-- :(
if' :: LParser
if' = discardS IF "if"

then' :: LParser
then' = discardS THEN "then"

else' :: LParser
else' = discardS ELSE "else"

fi :: LParser
fi = discardS FI "fi"

case' :: LParser
case' = discardS CASE "case"

esac :: LParser
esac = discardS ESAC "esac"

loop :: LParser
loop = discardS LOOP "loop"

pool :: LParser
pool = discardS POOL "pool"

while :: LParser
while = discardS WHILE "while"

class' :: LParser
class' = discardS CLASS "class"

in' :: LParser
in' = discardS IN "in"

inherits :: LParser
inherits = discardS INHERITS "inherits"

isvoid :: LParser
isvoid = discardS ISVOID "isvoid"

let' :: LParser
let' = discardS LET "let"

new :: LParser
new = discardS NEW "new"

of' :: LParser
of' = discardS OF "of"

not' :: LParser
not' = discardS NOT "not"

eof :: LParser
eof = whitespaces >> P.eof LE.InputNotExhausted >> pure (Lexeme EOF Nothing)

lexerP :: LParser
lexerP = do
  -- consume all whitespaces before extracting the head of the stream via peek
  _ <- whitespaces
  -- since the comment might be the last token that can be parsed,
  -- TRY to apply peek if possible so as not to fail immediately
  -- and never apply eof (which is at the end of the parser)
  h <- P.zeroOrOne P.peek
  comments
    <|> foldr1
      (P.<+>)
      [ str,
        semicolon,
        colon,
        darrow,
        lbrace,
        rbrace,
        lparen,
        rparen,
        at,
        comma,
        dot,
        add,
        sub,
        mul,
        div,
        tilda,
        eq,
        le,
        lt,
        assign,
        if',
        then',
        else',
        fi,
        case',
        esac,
        loop,
        pool,
        while,
        class',
        in',
        inherits,
        isvoid,
        let',
        new,
        of',
        not',
        bool,
        int,
        objectid,
        typeid
      ]
    <|> case h of
      Just h' -> P.item (const (mkLexError $ LE.InvalidSymbol $ controlToHex h'))
      Nothing -> eof
