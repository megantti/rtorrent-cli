{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad

import System.IO (hIsTerminalDevice, stdout)
import System.Directory

import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import Data.String (fromString)

import Data.Monoid

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
}

parse :: Parser Opts
parse = Opts
    <$> switch (
            short 'f'
            <> help "Show files")
    <*> optional (strOption $
            short 't'
            <> metavar "TORRENT"
            <> help "Filter by torrent name")
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
            <> metavar "PROGRAM"
            <> help "Run program on the first matching file on the last torrent")
    <*> switch (
            short 'q'
            <> help "Be quiet")
    <*> optional (strOption $
            short 'l'
            <> long "load"
            <> metavar "torrent"
            <> help "Load a new torrent")

checkId :: Int -> (Int, a) -> Bool
checkId i (j, _) = i == j

checkName :: Text -> TorrentInfo :*: [FileI] -> Bool
checkName find (t :*: _) = find `T.isInfixOf` str
  where
    str = T.toLower . convert $ torrentName t

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
         <> progDesc "RTorrent cli interface"
         <> header "RC - rtorrent cli remote control")
    opts <- execParser parserOpts

    colorize <- hIsTerminalDevice stdout

    Right torrents <- call $
            allTorrents (getTorrent <+> getTorrentFiles) 
    let mkFilt = maybe (const True) 
    let afterFilter = mkFilt checkId (idFilt opts)
    let beforeFilter = mkFilt checkName (fmap (T.toLower . convert) $ nameFilt opts)
    let fTorrents = filter afterFilter . zip [1..] . filter beforeFilter $
            torrents

    unless (quiet opts) $ 
       mapM_ (renderTorrent colorize (showFiles opts))
            fTorrents

    case load opts of
        Just file -> do
            path <- canonicalizePath file
                    `catch` (\(e :: IOException) -> 
                                return file)
            _ <- call $ loadStartTorrent path
            return ()
        Nothing -> return ()

    case reverse fTorrents of
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
