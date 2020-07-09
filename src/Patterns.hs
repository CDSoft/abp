{-
    This file is part of ABP.

    ABP is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ABP is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ABP.  If not, see <https://www.gnu.org/licenses/>.

    For further information about ABP you can visit
    http://cdelord.fr/abp
-}

module Patterns
    ( (|>)
    , whenJust
    , replace'
    , lookupBy
    )
where

import Data.List.Extra

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

whenJust :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust (Just x) f = f x
whenJust Nothing _ = return mempty

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

replace' :: [(String, String)] -> String -> String
replace' replacements = compose [ replace s0 s1 | (s0, s1) <- replacements ]

lookupBy :: (a -> Bool) -> [a] -> Maybe a
lookupBy p (x:_) | p x = Just x
lookupBy p (_:xs) = lookupBy p xs
lookupBy _ [] = Nothing
