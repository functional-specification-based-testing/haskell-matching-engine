import           Data.Time
import qualified DataFlow.HieDUJungleGenerator as DF
import qualified DataFlow.HieASTGraphGenerator as HA

main :: IO()
main = do
    startTime <- getCurrentTime

    -- (totalCount, coveredCount) <- DF.coverage "./.hie" "./run.out" 
    -- putStrLn $ "coveredCount: " ++ show coveredCount
    -- putStrLn $ "totalcount: " ++ show totalCount

    dfCoverage <- DF.coverage "./.hie" "./run.out"  
    putStrLn $ show dfCoverage

    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Execution Time: " ++ (show diff)

    --ast <- HA.printAsts "./.hie/Domain/Parser.hie"
    --ast <- HA.printAsts "./.hie/Main.hie"
    --ast <- HA.loadAST "./.hie/Main.hie"
    --putStrLn $ show $ foldr DF.convertToGraphNode (DF.DefNode "" []) ast 
    --putStrLn ast

    
