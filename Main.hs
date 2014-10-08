{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Exception

import Data.Maybe (isJust)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.String (fromString)
import Data.Monoid

import System.IO (hIsTerminalDevice, stdout)
import System.Directory

import Network.RTorrent
import Options.Applicative
    
toMB :: Int -> Int
toMB = (`div` (1024*1024))

toKB :: Int -> Int
toKB = (`div` 1024)

newtype Color = Color String

makeFg :: Int -> Color
makeFg i = Color $ "\x1b[3" ++ show i ++ "m"

red :: Color
red = makeFg 1

green :: Color
green = makeFg 2

yellow :: Color
yellow  = makeFg 3

magenta :: Color
magenta = makeFg 5

cyan :: Color
cyan = makeFg 6

colorStr :: Color -> String -> String
colorStr (Color c) s = c ++ s ++ "\x1b[39;49m"


colorBs :: Color -> Text -> Text
colorBs (Color c) s = (fromString c) <> s <> "\x1b[39;49m"

type FileI = FileInfo

convert :: String -> Text
convert = fromString

renderFile :: (Color -> Text -> Text) -> FileI -> IO ()
renderFile colorize file =
    T.putStrLn $ "\t\t" <> path <> " [" <> colorize cyan (fromString $ show p) <> "%]"
  where
    path = convert $ filePath file
    ch = fileSizeChunks file
    p = if ch == 0 
           then 100 
           else (100 * fileCompletedChunks file) `div` ch

renderTorrent :: Bool -> Bool -> (Int, TorrentInfo :*: [FileI]) -> IO ()
renderTorrent c files (i, torrent :*: fileData) =  do
    T.putStrLn $ colorB magenta (fromString $ show i) <> ". " <> name <> ":"
    putStrLn $ "      " ++ sizeS ++ 
                "\t[" ++ colorS cyan (show percent) ++ "%]:\t"
               ++ status
    when files $ do
        T.putStrLn path 
        mapM_ (renderFile colorB) fileData
  where
    path = "\tIn: " <> colorB yellow (convert $ torrentDir torrent)

    colorS = if c then colorStr else const id
    colorB = if c then colorBs else const id

    name = convert $ torrentName torrent
    size = torrentSize torrent
    compl = size - torrentBytesLeft torrent
    percent = (100 * compl) `div` size
    up = torrentUpRate torrent
    down = torrentDownRate torrent
    sizeS = colorS yellow (show (toMB compl)) ++ " MB / " 
                ++ colorS yellow (show (toMB size)) ++ " MB"
    status = 
        if torrentOpen torrent
           then "Up: " ++ colorS green (show (toKB up)) 
                ++ " KB | Down: " ++ colorS green (show (toKB down)) ++ " KB"
           else colorS red $ "CLOSED"

data Opts = Opts { 
    showFiles :: Bool
  , nameFilt :: Maybe String
  , idFilt :: Maybe Int
  , doCd :: Bool
  , doExec :: Maybe String
  , quiet :: Bool
  , load :: Maybe String
  , forceColor :: Bool
}

parse :: Parser Opts
parse = Opts
    <$> switch (
            short 'f'
            <> help "Show files")
    <*> optional (strOption $
            short 't'
            <> metavar "torrent"
            <> help "Filter by torrent name (checks substrings)")
    <*> optional (option auto $
            short 'i'
            <> metavar "ID"
            <> help "Filter by id")
    <*> switch (
            short 'c'
            <> help "CD into last matching dir")
    <*> optional (strOption $
            short 'e'
            <> long "exec"
            <> metavar "program"
            <> help "Run program on the first file on the last matching torrent")
    <*> switch (
            short 'q'
            <> help "Be quiet")
    <*> optional (strOption $
            short 'l'
            <> long "load"
            <> metavar "torrent"
            <> help "Load a new torrent")
    <*> switch (
            long "color"
            <> help "Force colors on"
        )

checkId :: Int -> (Int, a) -> Bool
checkId i (j, _) = i == j

checkName :: Text -> a :*: String -> Bool
checkName find (_ :*: t) = find `T.isInfixOf` str
  where
    str = T.toLower . convert $ t

shFile :: FilePath
shFile = ".rc_sh"

call :: Command a => a -> IO (Either String (Ret a))
call = callRTorrent "localhost" 5000

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let shFilePath = homeDir ++ "/" ++ shFile
    writeFile shFilePath ""

    let parserOpts = info (helper <*> parse)
          ( fullDesc
         <> header "RC - rtorrent cli remote control")
    opts <- execParser parserOpts

    colorize <- (forceColor opts ||) <$> hIsTerminalDevice stdout

    Right torrentNames <- call (allTorrents (getTorrentId <+> getTorrentName))

    let mkFilt = maybe (const True) 
    let beforeFilter = mkFilt checkName (fmap (T.toLower . convert) $ nameFilt opts)
    let afterFilter = mkFilt checkId (idFilt opts)
    let torrentsToGet = map snd
                        . filter afterFilter 
                        . zip [1..] 
                        . filter beforeFilter
                        $ torrentNames

    let filesRequired = isJust (doExec opts) || showFiles opts
    let getData = if filesRequired 
            then getTorrent <+> getTorrentFiles
            else fmap (:*: []) . getTorrent

    Right torrents <- fmap (fmap (zip [1..])) $ 
                        call (map (\(i :*: _) -> getData i) torrentsToGet)

    unless (quiet opts) $ 
       mapM_ (renderTorrent colorize (showFiles opts))
            torrents

    case load opts of
        Just file -> do
            path <- canonicalizePath file
                    `catch` (\(e :: IOException) -> 
                                return file)
            _ <- call $ loadStartTorrent path
            return ()
        Nothing -> return ()

    case reverse torrents of
        ((_, t :*: (f:_)) : _) -> do
            let dir = convert $ torrentDir t 
            let file = convert $ filePath f
            T.writeFile shFilePath $ 
                  (if doCd opts then "cd '" <> dir <> "'\n" else "")
              <> case doExec opts of
                    Just program -> T.pack program <> " '" 
                                       <> dir <> "/" 
                                       <> file
                                       <> "'"   
                        
                    Nothing -> ""
        _ -> return ()
