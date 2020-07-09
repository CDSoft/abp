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

module Package
    ( abpName
    , abpVersion
    , abpLicense
    , abpCopyright
    , abpDescription
    )
where

import Language.Haskell.TH

abpName :: ExpQ
abpName = getParam "name"

abpVersion :: ExpQ
abpVersion = getParam "version"

abpLicense :: ExpQ
abpLicense = getParam "license"

abpCopyright :: ExpQ
abpCopyright = getParam "copyright"

abpDescription :: ExpQ
abpDescription = getParam "description"

getParam :: String -> ExpQ
getParam name = stringE =<< runIO (getYamlParam name)

getYamlParam :: String -> IO String
getYamlParam name = checkValue <$> getValue
    where
        getValue :: IO (Maybe String)
        getValue = lookup name . map parse . lines <$> readFile "package.yaml"

        checkValue (Just value) = value
        checkValue Nothing = error $ "Unknown parameter '"++name++"` in package.yaml"

        parse s = (param, value)
            where
                (param, rest) = break (==':') s
                value = (reverse . dropWhile (=='"') . reverse . dropWhile (`elem` [':', '"', ' ', '\t'])) rest
