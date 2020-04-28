module Lib where

data RegEx c
	= Null
	| Empty
	| Const c
	| Union (RegEx c) (RegEx c)
	| Concat (RegEx c) (RegEx c)
	| Star (RegEx c)
	deriving Show

match :: Eq c => RegEx c -> [c] -> Maybe [c]

match Null _ = Nothing

match Empty [] = Just []
match Empty _ = Nothing

match (Const c) (x:xs) | x == c = Just xs
match (Const _) _ = Nothing

match (Union a b) xs =
	case match a xs of
		Nothing -> match b xs
		r -> r

match (Concat a b) xs =
	case match a xs of
		Just suf -> match b suf
		n -> n

match (Star r) xs =
	case match r xs of
		Just suf -> match (Star r) suf
		Nothing -> Just xs
