{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.UnixTime
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Posix.Files
import System.Posix.User

data Color = NEVER | ALWAYS | AUTO deriving (Show, Enum, Data, Eq)

data Args = Args
    { all_ :: Bool
    , almostAll :: Bool
    , reverse_ :: Bool
    , directory :: Bool
    , l :: Bool
    , color :: Color
    , path :: [FilePath]
    }
    deriving (Show, Data, Typeable)

defaultArgs =
    Args
        { all_ = False &= name "a" &= help "do not ignore entries starting with ."
        , almostAll = False &= name "A" &= help "do not list implied . and .."
        , reverse_ = False &= name "r" &= help "reverse order while sorting"
        , directory = False &= name "d" &= help "list directories themselves, not their contents"
        , l = False &= help "use a long listing format"
        , color = ALWAYS &= typ "WHEN" &= help "color the output WHEN"
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
    let cmd = genCmd args
    case length (path args) of
        0 -> cmd "."
        1 -> cmd $ head (path args)
        _ -> mapM_ liftIO $ intersperse (putStrLn "") $ map (\p -> putStrLn (p ++ ":") >> cmd p) (path args)

genCmd :: Args -> FilePath -> IO ()
genCmd args filepath = lsF (lsCmd args) (chainFs args) (colorF filepath args) filepath
  where
    lsF
        | l args = lsLong
        | otherwise = ls
    chainFs args = foldr1 (<=<) $ map ($ args) fs
    fs = [sorterF, hiddenF]

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

colorF :: FilePath -> Args -> (FilePath -> IO FilePath)
-- TODO: color=AUTO
colorF root args =
    if color args == ALWAYS
        then if directory args then colorEntry "" else colorEntry root
        else return

lsCmd :: Args -> (FilePath -> IO [FilePath])
lsCmd args
    | directory args = return . return
    | otherwise = getDirectoryContents

ls :: (FilePath -> IO [FilePath]) -> ([FilePath] -> IO [FilePath]) -> (FilePath -> IO FilePath) -> FilePath -> IO ()
ls cmd f color filepath = cmd filepath >>= (return . intersperse "  " <=< mapM color <=< f) >>= mapM_ putStr >> putStrLn ""

-- TODO: line that goes `total: ___` (is this recursive?)
lsLong :: (FilePath -> IO [FilePath]) -> ([FilePath] -> IO [FilePath]) -> (FilePath -> IO FilePath) -> FilePath -> IO ()
lsLong cmd f color filepath = cmd filepath >>= f >>= mapM_ (putStrLn <=< long)
  where
    long filename = do
        ft <- fileType fullPath
        permissions <- getPermissions fullPath
        colored <- color filename
        fileStatus <- getFileStatus fullPath
        owner <- userName <$> getUserEntryForID (fileOwner fileStatus)
        size <- getFileSize fullPath
        modTime <- formatUnixTime "%b %-d %H:%M" $ fromEpochTime $ modificationTime fileStatus
        -- TODO: each of these will be a separate `putStr` for alignment; use `ByteString.putStr` for modTime
        return $ intercalate "    " [ft : displayPermissions permissions, owner, show size, show modTime, colored]
      where
        fullPath = filepath ++ "/" ++ filename
    fileType filepath = do
        dir <- doesDirectoryExist filepath
        if dir
            then return 'd'
            else do
                symlink <- pathIsSymbolicLink filepath
                -- TODO: character file?
                if symlink then return 'l' else return '-'

displayPermissions :: Permissions -> String
displayPermissions p = [if readable p then 'r' else '-', if writable p then 'w' else '-', if executable p then 'x' else '-']

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
