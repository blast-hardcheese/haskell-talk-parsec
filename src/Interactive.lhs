For starters, one of Parsec's types, `Stream`, is of type:

``` {.sourceCode .literate .haskell}
class Monad m => Stream s (m :: * -> *) t | s -> t
```

Because of `m :: * -> *`, (I think,) we need:

>{-# LANGUAGE FlexibleContexts #-}

which lets us pass monad constructors (again, only about 30% sure) as arguments to `Stream`.

Let's also make this easy to import,

>module Interactive where

Imports
=======

> import Text.Parsec

gets us most of what will be discussed in this talk.

It will be handy to have the following functions available later on, but I
import them directly here as to not introduce name collisions.
In particular, `<|>` seems to be defined in Parsec and Control.Applicative,
better to just not mess with it.

> import Control.Applicative ((<$>))
> import Data.Char (ord)
> import Data.Maybe (catMaybes)
> import Data.List (findIndex)

Basic types
===========

Parsec s u
----------

    λ> :i Parsec
    type Parsec s u =
      ParsecT s u Data.Functor.Identity.Identity
            -- Defined in ‘Text.Parsec.Prim’

Basic wrapper around ParsecT, using the `Identity` monad for simplicity when starting out.

ParsecT s u m a
---------------

    λ> :i ParsecT
    newtype ParsecT s u (m :: * -> *) a
      = Text.Parsec.Prim.ParsecT {Text.Parsec.Prim.unParser :: forall b.
                                                               State s u
                                                               -> (a -> State s u -> ParseError -> m b)
                                                               -> (ParseError -> m b)
                                                               -> (a -> State s u -> ParseError -> m b)
                                                               -> (ParseError -> m b)
                                                               -> m b}
            -- Defined in ‘Text.Parsec.Prim’
    instance Monad (ParsecT s u m) -- Defined in ‘Text.Parsec.Prim’
    instance Functor (ParsecT s u m) -- Defined in ‘Text.Parsec.Prim’

More comprehensive, and (in fact,) the underlying type of (all?) parsers in Parsec. This gives full control over:

- s: Stream type. A stream represents a list of tokens. Text.Parsec.Prim (primitives) provides a `Stream` for `Char`, which is what we will be using here.
- u: User state type. Can be anything, if left empty is determined to be `Unit`.
- m: Underlying monad. Defaults to `Identity`, I haven't found a reason to change this.

Stream s m
----------

    λ> :i Stream
    class Monad m => Stream s (m :: * -> *) t | s -> t where
      uncons :: s -> m (Maybe (t, s))
            -- Defined in ‘Text.Parsec.Prim’
    instance Monad m => Stream [tok] m tok
      -- Defined in ‘Text.Parsec.Prim’

For many parsers, we'll need to mention `Stream`, to let the type system know that m and s are the same between `Stream` and `ParsecT`.

Simple parsers
==============

    char   :: Stream s m Char => Char   -> ParsecT s u m Char
    string :: Stream s m Char => String -> ParsecT s u m String

    λ> parseTest (char 'a') "a"
    'a'

    λ> parseTest (char 'a') "abcdef" -- Consumed: 'a', unconsumed "bcdef"
    'a'

    λ> parseTest (string "hello") "hello"
    "hello"

Combining parsers
=================

    many  ::                 ParsecT s u m a -> ParsecT s u m [a]
    many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]

    λ> parseTest (many $ char 'a') "aaa"
    "aaa"

    λ> parseTest (many $ char 'a') ""
    ""

    λ> parseTest (many1 $ char 'a') "aaa"
    "aaa"

    λ> parseTest (many1 $ char 'a') ""
    parse error at (line 1, column 1):
    unexpected end of input
    expecting "a"

Conditional parsers
===================

    manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
    skipMany :: ParsecT s u m a -> ParsecT s u m ()
    notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()

    λ> parseTest (manyTill (char 'a') (char 'b')) "aaab"
    "aaa"

    λ> parseTest (skipMany (char ' ')) "     abcdeg"
    ()

    λ> parseTest (notFollowedBy (char 'c')) "abc"
    ()

    λ> parseTest (notFollowedBy (char 'a')) "abc"
    parse error at (line 1, column 2):
    unexpected 'a'

Transformations
===============

    -- Parse reversed string
    λ> parseTest (reverse <$> (string xs)) "hello"
    "olleh"

    -- Parse base 10 number, returning it as an Int
    -- We have to specify the return type of the parser here, for `read`'s benefit

> baseTen :: Stream s m Char => ParsecT s u m Int
> baseTen = read <$> (many1 digit)

    λ> parseTest baseTen "100"
    100

    -- Simple octal-to-integer

> otoi :: [Char] -> Int
> otoi = foldl (\a x -> a * 8 + conv x) 0
>   where conv c = (ord c) - (ord '0')

    -- Parse octal number, returning it as an Int

> baseEight :: Stream s m Char => ParsecT s u m Int
> baseEight = otoi <$> (many1 octDigit)

    λ> parseTest baseEight "4"
    4

    λ> parseTest baseEight "10"
    8

    λ> parseTest baseEight "15"
    13

    λ> parseTest baseEight "9"
    parse error at (line 1, column 1):
    unexpected "9"
    expecting octal digit

Do notation
===========

> comment :: Stream s m Char => ParsecT s u m [Char]
> comment = do
>   _ <- char '-'
>   _ <- char '-'
>   _ <- spaces
>   chars <- many anyChar
>   return chars

    λ> parseTest comment "-- This is a comment"
    "This is a comment"

Parameterized parsers
=====================

> basen :: Stream s m Char => [Char] -> ParsecT s u m Integer
> basen nums = do
>   found <- many1 $ choice (map char nums)
>   let xs = catMaybes $ fmap (\x -> findIndex (== x) nums) found
>   return $ fromIntegral $ foldl (\a x -> a * n + x) 0 xs
>   where n = length nums

    λ> parseTest (basen "0123456789ABCDEF") "100"
    256

    λ> parseTest (basen "0123456789") "100"
    100

    λ> parseTest (basen "01234567") "100"
    64

    λ> parseTest (basen "01") "100"
    4

> prefix :: Stream s m Char => String -> ParsecT s u m r -> ParsecT s u m r
> prefix pre p = do
>   _ <- try $ string pre
>   p

    λ> parseTest (prefix "0b" (basen "01")) "0b101"
    5

Balanced parentheses
====================

> parens :: Stream s m Char => ParsecT s u m r -> ParsecT s u m r
> parens c = do
>   _ <- char '('
>   r <- c
>   _ <- char ')'
>   return r

    λ> parseTest (parens $ string "hey") "(hey)"
    "hey"

    λ> parseTest (parens $ prefix "0b" (basen "01")) "(0b101)"
    5

    λ> parseTest (parens $ parens $ prefix "0b" (basen "01")) "((0b101))"
    5

> neg :: Stream s m Char => Num n => ParsecT s u m n -> ParsecT s u m n
> neg c = do
>   _ <- char '-'
>   num <- c
>   return (-num)
