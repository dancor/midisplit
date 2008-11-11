module Main where

import Codec.Midi
import Control.Exception
import Control.Monad
import Data.List
import FUtil
import System.Environment
import System.FilePath.Posix
import System.IO

splitTrack :: Track Ticks -> [Track Ticks]
splitTrack = filter (not . null) . splitTrackAccum [] where
  splitTrackAccum :: Track Ticks -> Track Ticks -> [Track Ticks]
  splitTrackAccum accum [] = [accum]
  splitTrackAccum accum (m@(time, change):ms) =
    if time > 100 * 1000
      then [accum] ++ splitTrackAccum [(0, change)] ms
      else splitTrackAccum (accum ++ [m]) ms

doFile :: String -> IO ()
doFile file = do
  when (not $ ".mid" `isSuffixOf` file) $ error "i only like .mid right now"
  --fileDir = (\ x -> if null x then "." else x) $ takeDirectory file
  let fileName = takeBaseName file
  res <- try $ importFile file
  case res of
    Left err ->
      hPutStrLn stderr $ file ++ ": error: " ++ show (err :: IOException)
    Right res -> case res of
      Left err ->
        hPutStrLn stderr $ file ++ ": error: " ++ err
      Right (Midi fileType timeDiv [tempo:time:track]) -> do
        let
          trackMain = init track
          trackEnd = last track
          tracks = splitTrack trackMain
          tracks' = map (\ t -> [tempo, time] ++ t ++ [trackEnd]) tracks
          mids = map (Midi fileType timeDiv . (:[])) tracks'
        when (null mids) . hPutStrLn stderr $ file ++ ": no tracks"
        sequence_ $ zipWith
          (\ i -> exportFile (fileName ++ "-" ++ show i ++ ".mid")) [1..] mids
      Right (Midi _fileType _timeDiv tracks) ->
        hPutStrLn stderr $
          file ++ ": error: expecting 1 track (of size > 2) but found: " ++
          show (length tracks) ++ "."

main :: IO ()
main = do
  files <- getArgs
  mapM_ doFile files
