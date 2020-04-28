module Lib where

data RegEx c
	= Null
	| Empty
	| Const c
	| Union (RegEx c) (RegEx c)
	| Concat (RegEx c) (RegEx c)
	| Star (RegEx c)

match :: Eq c => ([c] -> r) -> r -> RegEx c -> [c] -> r

match _ rej Null _ = rej

match acc _ Empty [] = acc []
match _ rej Empty _ = rej

match acc _ (Const c) (x:xs) | x == c = acc xs
match _ rej (Const _) _ = rej

match acc rej (Union a b) xs = match acc (match acc rej b xs) a xs
match acc rej (Concat a b) xs = match (match acc rej b) rej a xs
match acc rej (Star r) xs = match (match acc rej (Star r)) (acc xs) r xs
