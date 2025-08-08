{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}

module Render (
    renderTorrent
  , RenderOpts (..)
) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Control.Monad
import Data.Maybe (isJust)
import Data.Monoid
import Network.RTorrent
import Data.List.Split
import qualified Data.Vector.Split as VS
    
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

data RenderOpts = RenderOpts {
    colorize :: Bool
  , renderFiles :: Bool
  , renderChunks :: Bool
}

renderFile :: (Color -> String -> String) -> Bool -> FileInfo -> IO ()
renderFile colorize hasChunks file =
    if hasChunks
        then do
            putStrLn $ "\t\t"  <> T.unpack path 
            putStrLn $ "\t\t " <> colorize cyan (show size) <> " MB [" 
                       <> colorize cyan (show p) <> "%]"
        else 
            putStrLn $ "\t\t" <> T.unpack path 
                              <> " [" <> colorize cyan (show p) <> "%]"
  where
    size = toMB $ fileSizeBytes file
    path = filePath file
    ch = fileSizeChunks file
    p = if ch == 0 
           then 100 
           else (100 * fileCompletedChunks file) `div` ch

renderAllFiles :: (Color -> String -> String) 
               -> V.Vector FileInfo
               -> Maybe (V.Vector Bool) :*: Int
               -> Bool
               -> IO ()
renderAllFiles colorize files (bits :*: chunkSize) renderChunks = do
    let factor = max (2^20 `div` chunkSize) 1
    let realSize = toKB $ chunkSize * factor
    when renderChunks $
        putStrLn $ "\t\t  (Bit: " <> show realSize <> "KB)"
    forM_ files $ \file -> do
      renderFile colorize renderChunks file
      case bits of 
        Just chunks -> let
              start = fileOffset file `div` chunkSize
              end = (fileSizeBytes file + fileOffset file) `div` chunkSize
              len = end - start + 1
              fileBits = V.take len . V.drop start $ chunks
            in 
              renderBitList "\t\t  " fileBits factor
        Nothing -> return ()

renderChunk :: Maybe (V.Vector Bool) :*: Int -> IO ()
renderChunk (Nothing :*: _) = return ()
renderChunk (Just bits :*: chunkSize) = do
    let factor = max (2^20 `div` chunkSize) 1
    let realSize = (chunkSize * factor) `div` 1024
    putStrLn $ "\t (Bit: " <> show realSize <> "KB)"
    renderBitList "\t" bits factor

renderBitList :: String -> V.Vector Bool -> Int -> IO ()
renderBitList start bits factor = do
    let rows = chunksOf 8 . chunksOf 8 . map and . VS.chunksOf factor $ bits
    forM_ rows $ \row -> do
      putStr start
      forM_ row $ \col -> do
        putStr $ map (\t -> if t then '1' else '0') col
        putStr " "
      putStrLn ""

renderTorrent :: RenderOpts -> (Int, TorrentInfo :*: V.Vector FileInfo :*: Maybe (V.Vector Bool) :*: Int) -> IO ()
renderTorrent opts (i, torrent :*: fileData :*: chunkInfo) =  do
    putStrLn $ color magenta (show i) <> ". " <> T.unpack name 
    putStrLn $ "      " ++ sizeS ++ 
                "\t[" ++ color cyan (show percent) ++ "%]:\t"
               ++ status
    when (renderFiles opts) $ do
        putStrLn path 
        renderAllFiles color fileData chunkInfo (renderChunks opts)
    when (renderChunks opts && not (renderFiles opts)) $ do
        renderChunk chunkInfo
  where

    path = "\tIn: " <> color yellow (T.unpack (torrentDir torrent))

    color = if colorize opts then colorStr else const id

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
