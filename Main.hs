{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Exception

import Data.Maybe (isJust)
import Data.List (isInfixOf)
import Data.Char (toLower)
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

type FileI = FileInfo

renderFile :: (Color -> String -> String) -> FileI -> IO ()
renderFile colorize file =
    putStrLn $ "\t\t" <> path <> " [" <> colorize cyan (show p) <> "%]"
  where
    path = filePath file
    ch = fileSizeChunks file
    p = if ch == 0 
           then 100 
           else (100 * fileCompletedChunks file) `div` ch

renderTorrent :: Bool -> Bool -> (Int, TorrentInfo :*: [FileI]) -> IO ()
renderTorrent c files (i, torrent :*: fileData) =  do
    putStrLn $ color magenta (show i) <> ". " <> name <> ":"
    putStrLn $ "      " ++ sizeS ++ 
                "\t[" ++ color cyan (show percent) ++ "%]:\t"
               ++ status
    when files $ do
        putStrLn path 
        mapM_ (renderFile color) fileData
  where
    path = "\tIn: " <> color yellow (torrentDir torrent)

    color = if c then colorStr else const id

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
           else color red "CLOSED"

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

checkName :: String -> a :*: String -> Bool
checkName find (_ :*: t) = find `isInfixOf` map toLower t

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
    let beforeFilter = mkFilt checkName (map toLower <$> nameFilt opts)
    let afterFilter = mkFilt checkId (idFilt opts)
    let torrentsToGet = filter afterFilter 
                        . zip [1..] 
                        . filter beforeFilter
                        $ torrentNames

    let filesRequired = isJust (doExec opts) || showFiles opts
    let getData = if filesRequired 
            then getTorrent <+> getTorrentFiles
            else fmap (:*: []) . getTorrent

    Right torrents <- call (
        map (\(k, tId :*: _) -> (\d -> (k, d)) <$> getData tId) torrentsToGet)

    unless (quiet opts) $ 
       mapM_ (renderTorrent colorize (showFiles opts))
            torrents

    case load opts of
        Just file -> do
            path <- canonicalizePath file
                    `catch` (\(_ :: IOException) -> 
                                return file)
            _ <- call $ loadStartTorrent path
            return ()
        Nothing -> return ()

    case reverse torrents of
        ((_, t :*: files) : _) -> do
            let dir = torrentDir t 
            writeFile shFilePath $ 
              (if doCd opts then "cd '" <> dir <> "'\n" else "")
              <> case (files, doExec opts) of
                    (f : _, Just program) -> 
                          program <> " '" 
                          <> dir <> "/" 
                          <> filePath f
                          <> "'"   
                    _ -> ""
        _ -> return ()
