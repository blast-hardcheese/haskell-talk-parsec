>{-# LANGUAGE FlexibleContexts #-}
> module Json where

> import Text.Parsec
> import Control.Applicative ((<$>), (<*), (*>))
> import Interactive (b10n)

> data JsValue = JsNumber Int | JsString String | JsArray [JsValue] | JsObject [(String, JsValue)] | JsBool Bool | JsNull
>   deriving (Show, Eq)

> jsonNumber :: Stream s m Char => ParsecT s u m JsValue
> jsonNumber = JsNumber <$> b10n

> jsonString :: Stream s m Char => ParsecT s u m JsValue
> jsonString = JsString <$> ((char '"') *> stringChar <* (char '"'))
>   where stringChar = many $ (char '\\' *> anyChar) <|> ((notFollowedBy (char '"')) *> anyChar)

> always :: Stream s m Char => a -> ParsecT s u m b -> ParsecT s u m a
> always x c = c *> (return x)

> jsonBool :: Stream s m Char => ParsecT s u m JsValue
> jsonBool = JsBool <$> ((always True (string "true")) <|> (always False (string "false")))

> jsonNull :: Stream s m Char => ParsecT s u m JsValue
> jsonNull = always JsNull (string "null")

> jsonArray :: Stream s m Char => ParsecT s u m JsValue
> jsonArray = jsonStructure ('[', ']') JsArray json


> jsonObject :: Stream s m Char => ParsecT s u m JsValue
> jsonObject = jsonStructure ('{', '}') JsObject kv
>   where kv = do
>           (JsString key) <- jsonString
>           _ <- spaces >> (char ':') >> spaces
>           value <- json
>           return (key, value)

> jsonStructure :: Stream s m Char => (Char, Char) -> ([a] -> JsValue) -> ParsecT s u m a -> ParsecT s u m JsValue
> jsonStructure (_o, _c) const val = const <$> (open *> (sepBy val comma) <* close)
>   where open = char _o *> spaces
>         comma = try $ spaces *> (char ',') <* spaces -- We use `try` here to force backtracking
>         close = spaces <* (char _c)

> json :: Stream s m Char => ParsecT s u m JsValue
> json = choice [jsonNumber, jsonString, jsonBool, jsonNull, jsonArray, jsonObject]
