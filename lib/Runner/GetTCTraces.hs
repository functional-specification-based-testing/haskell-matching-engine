import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO

import           Parser
import           Shahlaa

import           Data.Time

f a = a
main :: IO()
main = do
    startTime <- getCurrentTime
    args <-     getArgs

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
    let rawRequests = lines contents
    let requests = [genRequest rqid $ words rawRequest | (rqid, rawRequest) <- indexRequests rawRequests]
    let tc = addOracle requests

    if func == "--trades"
        then putStrLn $ fTestCase tc
        -- else putStrLn $ fCoverage $ coverage tc
        else putStrLn $ fCoverageInOrder $ coverage tc
    
    hClose handle

    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Execution Time: " ++ (show diff)