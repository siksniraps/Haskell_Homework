
{-
 - Vārdnīcai izveidoti datu tipu sinonīmi.
 - Vārdnīcas ieraksts Entry ir divu elementu korteži (a, b), kur b ir a tulkojums. 
 - a un b ir String vērtības.
 - Vārdnīca Dictionary ir saraksts ar vārdnīcas ierakstiem.
 -}
type Entry = (String, String)
type Dictionary = [Entry]


{-
 - Funkcija aa saņem sarakstu ar tulkojamiem vārdiem un vārdnīcu.
 - Funkcijas rezultāts ir saraksts ar visiem doto vārdu tulkojumiem.
 - Rezultējošajā sarakstā vārdi ir unikāli.
 -
 - Vispirms tiek izveidots jauns saraksts, kas satur visus tulkojamo vārdu iespējamos tulkojumus
 -
 -}
aa :: [String] -> Dictionary -> [String]
aa xx dict = foldr f [] [z | x <- xx, (y, z) <- dict, x == y]
    where f :: String -> [String] -> [String]
          f a acc
              | a `notElem` acc = a : acc
              | otherwise = acc
