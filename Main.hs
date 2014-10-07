{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Data.Char
import Data.List (isInfixOf, intercalate)

import System.IO (hIsTerminalDevice, stdout)
import System.Environment (getEnv)

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

color' :: Color -> String -> String
color' (Color c) s = c ++ s ++ "\x1b[39;49m"

type FileI = FileInfo

renderFile :: (Color -> String -> String) -> FileI -> String
renderFile colorize file =
    "\t\t" ++ path ++ " [" ++ colorize cyan (show p) ++ "%]"
  where
    path = filePath file
    p = (100 * fileCompletedChunks file) `div` fileSizeChunks file

renderTorrent :: Bool -> Bool -> (Int, TorrentInfo :*: [FileI])-> String
renderTorrent c files (i, torrent :*: fileData) = 
    color magenta (show i) ++ ". " ++ name ++ ":\n      " 
    ++ sizeS ++ "\t[" ++ color cyan (show percent) ++ "%]:\t"
    ++ status
    ++ if files 
          then path ++ intercalate "\n" (map (renderFile color) fileData)
          else ""
  where
    path = "\n\tIn: " ++ color yellow (torrentDir torrent) ++ "\n"
    color = if c then color' else const id
    name = torrentName torrent
    size = torrentSize torrent
    compl = size - torrentBytesLeft torrent
    percent = (100 * compl) `div` size
    up = torrentUpRate torrent
    down = torrentDownRate torrent
    sizeS = color yellow (show (toMB compl)) ++ " MB / " 
                ++ color yellow (show (toMB size)) ++ " MB"
    status = 
        if torrentOpen torrent
           then "Up: " ++ color green (show (toKB up)) 
                ++ " KB | Down: " ++ color green (show (toKB down)) ++ " KB"
           else color red $ "CLOSED"

data Opts = Opts { 
    showFiles :: Bool
  , nameFilt :: Maybe String
  , idFilt :: Maybe Int
  , doCd :: Bool
  , doExec :: Maybe String
  , quiet :: Bool
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

checkId :: Int -> (Int, a) -> Bool
checkId i (j, _) = i == j

checkName :: String -> TorrentInfo :*: [FileI] -> Bool
checkName find (t :*: _) = findStr `isInfixOf` str
  where
    findStr = map toLower find
    str = map toLower $ torrentName t

shFile :: FilePath
shFile = ".rc_sh"

main :: IO ()
main = do
    homeDir <- getEnv "HOME"
    let shFilePath = homeDir ++ "/" ++ shFile
    writeFile shFilePath ""

    let parserOpts = info (helper <*> parse)
          ( fullDesc
         <> progDesc "RTorrent cli interface"
         <> header "RC - rtorrent cli remote control")
    opts <- execParser parserOpts

    colorize <- hIsTerminalDevice stdout

    Right torrents <- callRTorrent "localhost" 5000 $
            allTorrents (getTorrent <+> getTorrentFiles) -- allFiles getFileAbsolutePath) 
    let mkFilt = maybe (const True) 
    let afterFilter = mkFilt checkId (idFilt opts)
    let beforeFilter = mkFilt checkName (nameFilt opts)
    let fTorrents = filter afterFilter . zip [1..] . filter beforeFilter $
            torrents

    unless (quiet opts) $ 
       mapM_ (putStrLn . renderTorrent colorize (showFiles opts))
            fTorrents

    case reverse fTorrents of
        ((_, t :*: (f:_)) : _) -> do
            let dir = torrentDir t 
            writeFile shFilePath $ 
                  (if doCd opts then "cd " ++ show dir ++ "\n" else "")
              ++ case doExec opts of
                    Just program -> program ++ " " 
                                       ++ show dir ++ "/" 
                                       ++ show (filePath f)
                    Nothing -> []
        _ -> return ()
