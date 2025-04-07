{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics

import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO

import           Domain.Parser
import           Infra.Shahlaa
import           Domain.ME

main :: IO()
main = do
    args <- getArgs

    when (length args /= 2) ( do
        progName <- getProgName
        hPutStrLn stderr $ "Usage:\t./" ++ progName ++ " --trades|--traces <input_file>"
        exitFailure
        )
    let func = head args
    let addr = args !! 1
    when (func /= "--trades" && func /= "--traces")
        $ error $ "Wrong func " ++ func

    handle <- openFile addr ReadMode
    contents <- hGetContents handle

    let parsed = B.pack contents
    case eitherDecode parsed :: Either String [Request] of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right rawRequests -> do
            let requests = [assignId rqid rawRequest | (rqid, rawRequest) <- indexRequests rawRequests]
            let tc = addOracle requests

            if func == "--trades"
                then putStrLn $ fTestCase tc
                else putStrLn $ fCoverageInOrder $ coverage tc

    hClose handle
