succeed v inp = [(v, inp)]

fail' inp = []

satisfy p [] = fail' []
satisfy p (x:xs)
  | p x = succeed x xs
  | otherwise = fail' xs

literal x = satisfy (==x)

literalStringHelper (x:xs) inp
  | xs == [] = literal x inp
  | literal x inp == [] = []
  | otherwise = let
                  [(_, rem)] = literal x inp
                in
                  literalStringHelper xs rem

literalString str inp
  | literalStringHelper str inp == [] = []
  | otherwise = [(str, snd $ head $ literalStringHelper str inp)]


alt' p1 p2 = \inp -> p1 inp ++ p2 inp

then' p1 p2 = \inp -> [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2, out2) <- p2 out1]

using' p f = \inp -> [(f v, out) | (v, out) <- p inp]

many' p = ((p `then'` (many' p)) `using'` uncurry (:)) `alt'` (succeed [])

some' p = (p `then'` (many' p)) `using'` uncurry (:)

number' = some' (satisfy digit)
          where digit x = '0' <= x && x <= '9'

word' = some' (satisfy letter)
        where letter x = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

string' [] = succeed []
string' (x:xs) = (literal x `then'` string' xs) `using'` uncurry (:)



