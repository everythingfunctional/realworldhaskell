module Prettify where

import           Data.Bits  (shiftR, (.&.))
import           Data.Char  (ord)
import           Numeric    (showHex)
import           SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

char :: Char -> Doc
char c = Char c

double :: Double -> Doc
double d = text (show d)

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

text :: String -> Doc
text "" = Empty
text s  = Text s

empty :: Doc
empty = Empty

line :: Doc
line = Line

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
            case d of
                Empty        -> best col ds
                Char c       -> c : best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                          (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

fill :: Int -> Doc -> Doc
fill desiredWidth doc = processNode 0 [doc]
    where processNode col (d:ds) =
            case d of
                Empty        -> Empty <> processNode col ds
                Char c       -> Char c <> processNode (col + 1) ds
                Text s       -> Text s <> processNode (col + (length s)) ds
                Line         -> spaceOut col <> Line <> processNode 0 ds
                a `Concat` b -> processNode col (a:b:ds)
                a `Union` b  -> processNode col (a:ds) `Union` processNode col (b:ds)
          processNode col [] = spaceOut col
          spaceOut col
                | desiredWidth - col < 1 = Empty
                | otherwise              = Text (replicate (desiredWidth - col) ' ')
