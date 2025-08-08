{-# LANGUAGE TypeOperators, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Control.Exception

import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (isInfixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Char (toLower)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO (hIsTerminalDevice, stdout)
import System.Directory
import System.Environment (lookupEnv)

import Data.Traversable (traverse)

import Network.RTorrent
import Network.RTorrent.Action (pureAction, simpleAction, Action)
import Options.Applicative

import Render
import Network.RTorrent.Value

data Opts = Opts { 
    showFiles :: Bool
  , showChunks :: Bool
  , nameFilt :: T.Text
  , idFilt :: Int -> Bool
  , quiet :: Bool
  , verbose :: Bool
  , forceColor :: Bool
  , cmd :: Maybe RCommand
}

data RCommand = 
      CCD 
    | CStop
    | CStart
    | CDelete
    | CExec String [String]
    | CLoad String

type ChunkInfo = Maybe (Vector Bool) :*: Int

type TInfo = TorrentInfo :*: Vector FileInfo :*: ChunkInfo

parseIdList :: String -> ReadM (Int -> Bool)
parseIdList = fmap (\fs i -> any ($ i) fs) 
            . traverse check . splitOn ","
  where
    readInt :: String -> ReadM Int
    readInt str = case reads str of
        [(i, "")] -> pure i
        _         -> readerError "Trash found in ID"
    check = cases . splitOn "-" 
      where
        cases c = case c of
            ["", a] -> (>=) <$> readInt a
            [a, ""] -> (<=) <$> readInt a
            [a,  b] -> (\s e i -> s <= i && i <= e) 
                        <$> readInt a
                        <*> readInt b
            [a]     -> (==) <$> readInt a
            _       -> readerError "Trash found in ID"

parse :: Parser Opts
parse = Opts
    <$> switch (
            short 'f'
            <> help "Show files")
    <*> switch (
            short 'u'
            <> help "Show chunks")
    <*> strOption (
            short 't'
            <> value ""
            <> metavar "torrent"
            <> help "Filter by torrent name (checks substrings)")
    <*> option (str >>= parseIdList) (
            short 'i'
            <> value (const True)
            <> metavar "ID"
            <> help "Filter by id, where -2,3-5,6,7- matches every id")
    <*> switch (
            short 'q'
            <> help "Be quiet, overrides verbose")
    <*> switch (
            short 'v'
            <> help "Be verbose")
    <*> switch (
            short 'c'
            <> long "color"
            <> help "Force colors on")
    <*> optional (
          subparser (
             command "load"   load
          <> command "cd"     cd
          <> command "exec"   exec
          <> command "start"  start
          <> command "stop"   stop
          <> command "delete" delete
        ))
  where
    load = info (helper <*> arg) $
        progDesc "Load a new torrent file."
      where 
        arg = CLoad <$> strArgument (metavar "url")
    cd = info (helper <*> pure CCD) $
        progDesc "Change directory to base directory of a matching torrent."
    start = info (helper <*> pure CStart) $
        progDesc "Start torrents."
    stop = info (helper <*> pure CStop) $
        progDesc "Stop torrents."
    delete = info (helper <*> pure CDelete) $
        progDesc "Delete torrents."
    exec = info (helper <*> (CExec <$> prg <*> many args)) $
        progDesc "Execute a program on the first file of the last matching torrent."
      where
        prg = strArgument (metavar "program")
        args = strArgument (metavar "args")

checkName :: T.Text -> a :*: T.Text -> Bool
checkName find (_ :*: t) = find `T.isInfixOf` T.toLower t

shFile :: FilePath
shFile = ".rc_sh"

call :: Command a => a -> IO (Either String (Ret a))
call command = do
    uri <- fromMaybe "tcp://localhost:5000" . filterEmpty <$> lookupEnv "RT_URL"
    callRTorrent uri command
  where
    filterEmpty (Just "") = Nothing
    filterEmpty a = a
    checkPort s = case reads s of
        [(i, _)] -> return i
        _ -> throwIO $ ErrorCall "RT_PORT is not an integer"

addPath :: String -> IO ()
addPath url = do
    path <- canonicalizePath url
            `catch` (\(_ :: IOException) -> 
                        return url)
    _ <- call $ loadStartTorrent (T.pack path)
    return ()
    
changeDir :: String -> Vector (a, TorrentInfo :*: b) -> IO ()
changeDir shFilePath torrents = 
    case V.unsnoc torrents of
        Just (_, (_, t :*: _)) -> do
            let dir = torrentDir t 
            writeFile shFilePath $ "cd '" <> T.unpack dir <> "'\n"
        _ -> return ()

exec :: String -> [String] -> String -> Vector (a, TorrentInfo :*: Vector FileInfo :*: b) -> IO ()
exec program args shFilePath torrents =
    case V.unsnoc torrents of
        Just (_, (_, t :*: files :*: _)) -> do
            case V.uncons files of 
                Just (file, _) -> do
                    let dir = T.unpack (torrentDir t)
                    writeFile shFilePath $ 
                      intercalate " " (program : args)
                      <> " '" <> dir <> "/" 
                      <> T.unpack (filePath file)
                      <> "'"   
                _ -> return ()
        _ -> return ()

commandOn :: (TorrentId -> TorrentAction Int) -> Vector (a, TorrentInfo :*: b) -> IO ()
commandOn cmd torrents = void . call $
    V.map (\(_, t :*: _) -> cmd (torrentId t)) torrents

filesRequired :: Bool -> Opts -> Bool
filesRequired doShow opts = (doShow && showFiles opts) || check (cmd opts)
  where
    check (Just (CExec _ _)) = True
    check _ = False

getRight :: Either String a -> IO a
getRight (Right a) = return a
getRight (Left e) = throwIO $ ErrorCall e 

commandSimpleArray :: T.Text -> Global (Vector T.Text)
commandSimpleArray = commandSimple

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let shFilePath = homeDir ++ "/" ++ shFile
    writeFile shFilePath ""

    let parserOpts = info (helper <*> parse)
          ( fullDesc
         <> header "RC - rtorrent cli remote control"
         <> footer ("Use environment variables RT_HOST and RT_PORT to "
                   ++ "set host and port, the defaults are localhost and 5000. "
                   ++ "Note that the commands act on all matching torrents.")
         )
    opts <- execParser parserOpts

    colorize <- (forceColor opts ||) <$> hIsTerminalDevice stdout

    torrentNames <- getRight =<< 
                        call (allTorrents (getTorrentId <+> getTorrentName))

    let beforeFilter = checkName (T.toLower $ nameFilt opts)
    let afterFilter = idFilt opts . fst
    let torrentsToGet = V.filter afterFilter 
                        . V.imap (\i v -> (i + 1, v))
                        . V.filter beforeFilter
                        $ torrentNames

    let doShow = (verbose opts || isNothing (cmd opts)) && (not $ quiet opts) 

    let getFiles = if filesRequired doShow opts
            then getTorrentFileInfo
            else pureAction V.empty

    let getChunks = if showChunks opts 
            then getTorrentChunks <+> getTorrentChunkSize
            else pureAction Nothing <+> pureAction 0

    let getData = getTorrent
                  <+> getFiles
                  <+> getChunks

    let callTo = V.map (\(k, tId :*: _) -> (\d -> (k, d)) <$> getData tId) torrentsToGet
    torrents <- getRight =<< call callTo

    let renderOpts = RenderOpts {
        colorize = colorize
      , renderFiles = showFiles opts
      , renderChunks = showChunks opts
    }

    when doShow $ 
       V.mapM_ (renderTorrent renderOpts)
            torrents

    case cmd opts of
        Nothing -> return ()
        Just c ->
            case c of 
              CLoad url -> addPath url
              CCD -> changeDir shFilePath torrents
              CExec prg args -> exec prg args shFilePath torrents
              CStart -> commandOn start torrents
              CStop -> commandOn closeStop torrents
              CDelete -> commandOn erase torrents
