Introduction
============

This is intended to be a brief, directed introduction to Parsec.

Parsec is a parser combinator library for Haskell. Another popular choice is [attoparsec](http://hackage.haskell.org/package/attoparsec), which promises faster parsing, and binary parsing by way of `ByteString`. While very exciting, it is beyond the scope of this talk.

All the definitions in this file can be found as a Literate Haskell file, `src/Interactive.lhs`.

Let's get started.

Language Features
=================

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
In particular, `<|>` (or) seems to be defined in Parsec and Control.Applicative,
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

Stream s m t
------------

    λ> :i Stream
    class Monad m => Stream s (m :: * -> *) t | s -> t where
      uncons :: s -> m (Maybe (t, s))
            -- Defined in ‘Text.Parsec.Prim’
    instance Monad m => Stream [tok] m tok
      -- Defined in ‘Text.Parsec.Prim’

For many parsers, we'll need to mention `Stream`, to let the type system know that m and s are the same between `Stream` and `ParsecT`.

Simple parsers
==============

`char` and `string` are two of the most useful primitive parsers I am aware of.

    char   :: Stream s m Char => Char   -> ParsecT s u m Char
    string :: Stream s m Char => String -> ParsecT s u m String

When applied, we parse exactly what was specified. No flexibility, just the expected character (or a parser exception).

    λ> parseTest (char 'a') "a"
    'a'

    λ> parseTest (char 'a') "abcdef" -- Consumed: 'a', unconsumed "bcdef"
    'a'

Same with `string`; We can see that a `string` parser could really just be a sequence of `char` parsers, the return type of the parser being `[Char]`.

    λ> parseTest (string "hello") "hello"
    "hello"

Combining parsers
=================

`many` and `many1` both take parsers of return type `a`, and return `[a]` (much like `string`).

    many  ::                 ParsecT s u m a -> ParsecT s u m [a]
    many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]

We can see this in action...

    λ> parseTest (many $ char 'a') "aaa"
    "aaa"

    λ> parseTest (many $ char 'a') ""
    ""

    λ> parseTest (many1 $ char 'a') "aaa"
    "aaa"

... and an example of a match failure:

    λ> parseTest (many1 $ char 'a') ""
    parse error at (line 1, column 1):
    unexpected end of input
    expecting "a"

bringing us to...

Conditional and zero-width parsers
==================================

    manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
    skipMany :: ParsecT s u m a -> ParsecT s u m ()
    notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()

manyTill is a parser that will _not_ consume the whatever the second parser matches.
Equivalent to:

> manyTill' :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
> manyTill' c end = (c >>= (\x -> (x:) <$> (manyTill' c end))) <|> (end >> (return []))

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

As it turns out, we can fmap on the result of a parse:

    -- Parse reversed string
    λ> parseTest (reverse <$> (string xs)) "hello"
    "olleh"

Parse a base 10 number, returning it as an Int.
We do have to specify the return type here, so `read` knows what to look for.
If `read` fails, the parse fails, and the next is tried.

> baseTen :: Stream s m Char => ParsecT s u m Int
> baseTen = read <$> (many1 digit)

    λ> parseTest baseTen "100"
    100

> xTo999 :: Stream s m Char => ParsecT s u m Int
> xTo999 = ((\_ -> 999) <$> (char 'x'))

    λ> parseTest (baseTen <|> xTo999) "1234"
    1234

    λ> parseTest (baseTen <|> xTo999) "x"
    999

Naive octal-to-integer implementation

> otoi :: [Char] -> Int
> otoi = foldl (\a x -> a * 8 + conv x) 0
>   where conv c = (ord c) - (ord '0')

Match as many octal digits as we can, then map over that to transform our result.

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

Up until now, I've stayed away from `do` notation. This is partially because [do notation is considered harmful](https://wiki.haskell.org/Do_notation_considered_harmful), but also because it's helpful to remember that these combinators really are just plain Haskell, and behave as such.

This being said, do notation is very helpful for visualizing flow. In the following example, we can see very plainly see that the following example does four things:

- Parse a single `-`
- Parse another `-`
- Parse zero or more spaces
- Parse as many anyChars as we can (returning the result).

This will consume (and return), the rest of the input. The dashes and leading spaces are dropped, as they are only structure.

> comment :: Stream s m Char => ParsecT s u m [Char]
> comment = do
>   _ <- char '-'
>   _ <- char '-'
>   _ <- spaces
>   many anyChar

    λ> parseTest comment "-- This is a comment"
    "This is a comment"

Parameterized parsers
=====================

To introduce a familiar concept into our crazy world of parsers, we need a new building block:

    λ> :i choice
    choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
            -- Defined in ‘Text.Parsec.Combinator’

`choice` takes a list of parsers (all of the same type), and returns a parser that tries them all, in order, looking for one that succeeds.

If we think back, `char` is a `Char -> ParsecT s u m Char`, so why not fmap `char` over `[Char]` to get `[ParsecT s u m Char]`, exactly what we need for `choice`? This gets us a pretty clean implementation of a "base-n" parser, something that parses any arbitrary number base:

> basen :: Stream s m Char => Num a => [Char] -> ParsecT s u m a
> basen nums = do
>   found <- many1 $ choice (char <$> nums)
>   let xs = catMaybes $ fmap (\x -> findIndex (== x) nums) found
>   return $ fromIntegral $ foldl (\a x -> a * n + x) 0 xs
>   where n = length nums

Now, it becomes trivial to parse any number base:

    λ> parseTest (basen "0123456789ABCDEF") "100"
    256

    λ> parseTest (basen "0123456789") "100"
    100

    λ> parseTest (basen "01234567") "100"
    64

    λ> parseTest (basen "01") "100"
    4

This is cool, but as we saw, "100" had no designation; it could be any number! How should we add a prefix to a number to indicate which number base it is?

You guessed it, another parser!

> prefix :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
> prefix pre p = (string pre) >> p

    λ> parseTest (prefix "0b" (basen "01")) "0b101"
    5

Wow, that was easy!

Negative numbers
================

Positive numbers are pretty cool, but sometimes negative numbers are good too.

> neg :: Stream s m Char => Num n => ParsecT s u m n -> ParsecT s u m n
> neg c = (char '-') >> (negate <$> c)

This is pretty great, since now _any_ parser that has a `Num` return type can be negated:

    λ> parseTest (neg baseTen ) "-100"
    -100

    λ> parseTest (neg $ prefix "0b" $ basen "01") "-0b101"
    -5

even

    λ> parseTest (neg xTo999) "-x"
    -999

It also turns out to be trivial to parse either positive _or_ negative numbers, by way of `<|>`:

> b10n :: Stream s m Char => Num a => ParsecT s u m a
> b10n = (neg $ basen nums) <|> (basen nums)
>   where nums = "0123456789"

Now, b10n works the way we'd expect:

    λ> parseTest b10n "10"
    10

    λ> parseTest b10n "-10"
    -10

Balanced parentheses
====================

No talk about parser combinators would be complete without parentheses. Oh boy.

> parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
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

But wait! `parens` just passes through the return type of the parameterized parser! This means we can:

    λ> parseTest (neg $ parens $ prefix "0b" (basen "01")) "-(0b101)"
    -5
