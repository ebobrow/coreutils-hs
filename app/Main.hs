{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Char
import Data.Function
import Data.Functor
import Data.List
import System.Console.CmdArgs
import System.Directory
import System.Environment

data Args = Args {all_ :: Bool, almostAll :: Bool, path :: FilePath} deriving (Show, Data, Typeable)

defaultArgs =
    Args
        { all_ = False &= name "a" &= help "do not ignore entries starting with ."
        , almostAll = False &= name "A" &= help "do not list implied . and .."
        , path = "." &= args &= typFile -- TODO: multiple files
        }

-- TODO: args
main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    ls (constructF args) $ path args
    putStrLn ""

constructF :: Args -> ([FilePath] -> [FilePath])
constructF args
    | almostAll args = sortCaseInsensitive . filter (not . isImpliedEntry)
    | all_ args = sortCaseInsensitive
    | otherwise = defaultF

ls :: ([FilePath] -> [FilePath]) -> FilePath -> IO ()
ls f filepath = getDirectoryContents filepath >>= (mapM (colorEntry filepath) . f) >>= (mapM_ putStr . intersperse "  ")

lsDefault :: FilePath -> IO ()
lsDefault = ls (sortCaseInsensitive . filter (not . isHidden))

defaultF :: [FilePath] -> [FilePath]
defaultF = sortCaseInsensitive . filter (not . isHidden)

isHidden :: FilePath -> Bool
isHidden ('.' : _) = True
isHidden _ = False

isImpliedEntry :: FilePath -> Bool
isImpliedEntry filepath = (filepath == ".") || (filepath == "..")

sortCaseInsensitive :: [String] -> [String]
sortCaseInsensitive = sortBy (compare `on` map toLower)

colorEntry :: FilePath -> FilePath -> IO FilePath
colorEntry root filename = do
    dircolors <- getEnv "LS_COLORS" <&> splitAtChar ':'
    s <- foldr foldFunc (return "0") (reverse dircolors)
    return $ style s filename
  where
    -- TODO: naming horrendous
    fullPath = root ++ "/" ++ filename -- TODO: check if there is already a slash, etc. (or we don't need to?)
    foldFunc ('r' : 's' : '=' : s) x = return s
    foldFunc ('d' : 'i' : '=' : s) x = applyF di s x
    foldFunc ('f' : 'i' : '=' : s) x = applyF fi s x
    foldFunc ('l' : 'n' : '=' : s) x = applyF ln s x
    foldFunc ('p' : 'i' : '=' : s) x = applyF pi s x
    foldFunc ('s' : 'o' : '=' : s) x = applyF so s x
    foldFunc ('b' : 'd' : '=' : s) x = applyF bd s x
    foldFunc ('c' : 'd' : '=' : s) x = applyF cd s x
    foldFunc ('o' : 'r' : '=' : s) x = applyF or s x
    foldFunc ('m' : 'i' : '=' : s) x = applyF mi s x
    foldFunc ('e' : 'x' : '=' : s) x = applyF ex s x
    foldFunc ext x = do
        let [extName, s] = splitAtChar '=' ext
        let extNoStar = last $ splitAtChar '.' extName
        case splitAtChar '.' filename of
            [] -> x
            xs -> if extNoStar == last xs then return s else x
    applyF f s x = f fullPath >>= (\res -> if res then return s else x)
    -- TODO: also just inline these?
    di = doesDirectoryExist
    fi = doesFileExist
    ln = pathIsSymbolicLink
    -- TODO: all of these
    pi _ = return False
    so _ = return False -- TODO: just always ends in `.sock`?
    bd _ = return False
    cd _ = return False
    or _ = return False
    mi _ = return False
    ex filepath = executable <$> getPermissions filepath

splitAtChar :: Char -> String -> [String]
splitAtChar c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : splitAtChar c s''
      where
        (w, s'') = break (== c) s'

style :: String -> String -> String
style s str = "\x1b[" ++ s ++ "m" ++ str ++ "\x1b[0m"
