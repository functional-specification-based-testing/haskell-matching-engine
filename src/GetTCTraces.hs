import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO
import           Data.Time

import           Domain.Parser
import           Infra.Shahlaa
import qualified DataFlow.HieDUJungleGenerator as DF

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
    let rawRequests = lines contents
    let requests = [genRequest rqid $ words rawRequest | (rqid, rawRequest) <- indexRequests rawRequests]
    let tc = addOracle requests

    if func == "--trades"
        then putStrLn $ fTestCase tc
        -- else putStrLn $ fCoverage $ coverage tc
        else putStrLn $ fCoverageInOrder $ coverage tc

    hClose handle

    startTime <- getCurrentTime

    -- (totalCount, coveredCount) <- DF.coverage "./.hie" "./run.out" 
    -- putStrLn $ "coveredCount: " ++ show coveredCount
    -- putStrLn $ "totalcount: " ++ show totalCount
    
    dfCoverage <- DF.analyze "./.hie" "./run.out"  
    putStrLn $ dfCoverage

    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Execution Time: " ++ (show diff)

    
