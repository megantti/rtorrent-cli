{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}

module Render (
    renderTorrent
) where

import Control.Monad
import Data.Monoid
import Network.RTorrent
    
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

renderFile :: (Color -> String -> String) -> FileInfo -> IO ()
renderFile colorize file =
    putStrLn $ "\t\t" <> path <> " [" <> colorize cyan (show p) <> "%]"
  where
    path = filePath file
    ch = fileSizeChunks file
    p = if ch == 0 
           then 100 
           else (100 * fileCompletedChunks file) `div` ch

renderTorrent :: Bool -> Bool -> (Int, TorrentInfo :*: [FileInfo]) -> IO ()
renderTorrent c files (i, torrent :*: fileData) =  do
    putStrLn $ color magenta (show i) <> ". " <> name 
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
