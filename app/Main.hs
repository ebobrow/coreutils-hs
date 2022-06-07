{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import System.Console.CmdArgs
import System.Directory
import System.Environment

data Args = Args
    { all_ :: Bool
    , almostAll :: Bool
    , reverse_ :: Bool
    , directory :: Bool
    , path :: [FilePath]
    }
    deriving (Show, Data, Typeable)

defaultArgs =
    Args
        { all_ = False &= name "a" &= help "do not ignore entries starting with ."
        , almostAll = False &= name "A" &= help "do not list implied . and .."
        , reverse_ = False &= name "r" &= help "reverse order while sorting"
        , directory = False &= name "d" &= help "list directories themselves, not their contents"
        , path = [] &= args &= typFile
        }

-- TODO: args
-- --recursive: recurse into subdirectories (could expand to tree)
-- -l
-- --color[=WHEN]
-- ...
main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    -- TODO: there has to be a nicer way to write
    --                                      this
    let cmd = (\filepath -> ls (lsCmd args) (colorF filepath args <=< sorterF args <=< hiddenF args) filepath)
    case length (path args) of
        0 -> cmd "."
        1 -> cmd $ head (path args)
        _ -> mapM_ liftIO $ intersperse (putStrLn "") $ map (\p -> putStrLn (p ++ ":") >> cmd p) (path args)

sorterF :: Args -> ([FilePath] -> IO [FilePath])
sorterF args
    | reverse_ args = return . reverse . sortCaseInsensitive
    | otherwise = return . sortCaseInsensitive
  where
    -- TODO: handle multi line
    -- also GNU ls sorts hidden files last
    sortCaseInsensitive = sortBy (compare `on` map toLower)

hiddenF :: Args -> ([FilePath] -> IO [FilePath])
hiddenF args
    | almostAll args = return . filter (not . isImpliedEntry)
    | all_ args = return
    | otherwise = return . filter (not . isHidden)
  where
    isHidden ('.' : _) = True
    isHidden _ = False
    isImpliedEntry filepath = (filepath == ".") || (filepath == "..")

colorF :: FilePath -> Args -> ([FilePath] -> IO [FilePath])
colorF filepath args
    | directory args = mapM $ colorEntry ""
    | otherwise = mapM $ colorEntry filepath

lsCmd :: Args -> (FilePath -> IO [FilePath])
lsCmd args
    | directory args = return . return
    | otherwise = getDirectoryContents

ls :: (FilePath -> IO [FilePath]) -> ([FilePath] -> IO [FilePath]) -> FilePath -> IO ()
ls cmd f filepath = cmd filepath >>= f >>= (mapM_ putStr . intersperse "  ") >> putStrLn ""

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
