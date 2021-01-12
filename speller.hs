-- Speller

spellOut :: [[Char]] -> [[Char]]
-- Point Free function defination
spellOut = map (\word -> (head word):" is for " ++ word)

speller :: [[Char]] -> [Char]
speller xs = foldr (\elm acc -> case acc of 
                                  "" -> elm
                                  xs -> elm ++ ", " ++ acc
                    ) "" (spellOut xs)