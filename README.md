    data ParsecT s u m a Source

-   `ParsecT s u m a` is a parser with stream type `s`, user state type
    `u`, underlying monad `m` and return type `a`.

    class Monad m =\> Stream s m t

-   An instance of Stream has stream type s, underlying monad m and
    token type t determined by the stream

Simple parsers:

    char   :: Stream s m Char => Char   -> ParsecT s u m Char
    string :: Stream s m Char => String -> ParsecT s u m String

Examples:

    λ> parseTest (char 'a') "a"
    'a'

    λ> parseTest (char 'a') "abcdef" -- Consumed: 'a', unconsumed "bcdef"
    'a'

    λ> parseTest (string "hello") "hello"
    "hello"

Combining parsers:

    many  ::                 ParsecT s u m a -> ParsecT s u m [a]
    many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]

Examples:

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

Conditional parsers:

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

Transformations:

    -- Parse reversed string
    λ> parseTest (reverse <$> (string xs)) "hello"
    "olleh"

    -- Parse base 10 number, returning it as an Int
    -- We have to specify the return type of the parser here, for `read`'s benefit
    λ> :{
       let baseTen :: Stream s m Char => ParsecT s u m Int
           baseTen = read <$> (many1 digit)
       :}

    λ> parseTest baseTen "100"
    100

    -- Simple octal-to-integer
    λ> :{
       let otoi :: [Char] -> Int
           otoi = foldl (\a x -> a * 8 + conv x) 0 
             where conv c = (ord c) - (ord '0')
       :}

    -- Parse octal number, returning it as an Int
    λ> :{
       let baseEight :: Stream s m Char => ParsecT s u m Int
           baseEight = otoi <$> (many1 octDigit)
       :}


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

Do notation:

    λ> :{
       let comment :: Stream s m Char => ParsecT s u m [Char]
           comment = do
             _ <- char '-'
             _ <- char '-'
             _ <- spaces
             chars <- many anyChar
             return chars
       :}

    λ> parseTest comment "-- This is a comment"
    "This is a comment"

Parameterized parsers:

    :{
    let basen :: Stream s m Char => [Char] -> ParsecT s u m Integer
        basen nums = do
          found <- many1 $ choice (map char nums)
          let xs = catMaybes $ fmap (\x -> findIndex (== x) nums) found
          return $ fromIntegral $ foldl (\a x -> a * n + x) 0 xs
          where n = length nums
    :}

    λ> parseTest (basen "0123456789ABCDEF") "100"
    256

    λ> parseTest (basen "0123456789") "100"
    100

    λ> parseTest (basen "01234567") "100"
    64

    λ> parseTest (basen "01") "100"
    4

    λ> :{
       let prefix :: Stream s m Char => String -> ParsecT s u m r -> ParsecT s u m r
           prefix pre p = do
             _ <- try $ string pre
             p
       :}

    λ> parseTest (prefix "0b" (basen "01")) "0b101"
    5

Balanced parentheses:

    λ> :{
       let parens :: Stream s m Char => ParsecT s u m r -> ParsecT s u m r
           parens c = do
             _ <- char '('
             r <- c
             _ <- char ')'
             return r
       :}

    λ> parseTest (parens $ string "hey") "(hey)"
    "hey"

    λ> parseTest (parens $ prefix "0b" (basen "01")) "(0b101)"
    5

    λ> parseTest (parens $ parens $ prefix "0b" (basen "01")) "((0b101))"
    5

    λ> :{
       let neg :: Stream s m Char => Num n => ParsecT s u m n -> ParsecT s u m n
           neg c = do
             _ <- char '-'
             num <- c
             return (-num)
       :}
