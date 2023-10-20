module Shahlaa
    ( addOracle
    , coverage
    , fTestCase
    , fCoverage
    , fCoverageInOrder
    ) where

import           Control.Monad.Trans.State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Text.Printf

import           Domain.ME
import           Domain.MEService
import           Printer
import           Infra.Coverage


data TestCase = TestCase
    { input    :: [Request]
    , output   :: [Response]
    , coverage :: [CoverageInfo]
    } deriving (Eq, Show)


type TestState = (MEState, [Response], [CoverageInfo])


initTestState :: TestState
initTestState = (initMEState, [], [])


handleRequest :: TestState -> Request -> TestState
handleRequest (s, rss, covs) rq =
    (s', rss ++ [rs], covs ++ [cov])
  where
    (rs, cov) = runState (requestHandler rq s) emptyCoverage
    s' = Domain.ME.state rs


addOracle :: [Request] -> TestCase
addOracle rqs =
    TestCase rqs rss covs
  where
    (s, rss, covs) = foldl handleRequest initTestState rqs


coverageScoreTC :: TestCase -> Int
coverageScoreTC = coverageScore . concat . coverage


avgCoverageScoreTS :: [TestCase] -> Double
avgCoverageScoreTS ts = fromIntegral (sum $ map coverageScoreTC ts) / fromIntegral (length ts)


coverageSetTC :: [CoverageInfo] -> Set.Set CoverageItem
coverageSetTC = Set.fromList . concat


fCoverage :: [CoverageInfo] -> String
fCoverage cs = unwords $ Set.elems $ coverageSetTC cs


fCoverageInOrder :: [CoverageInfo] -> String
fCoverageInOrder cs = unwords $ concat cs


fInput :: [Request] -> String
fInput rqs = foldl (++) (printf "%d\n" $ length rqs) $ map fRequest rqs


fOutput :: [Response] -> String
fOutput = concatMap fResponse


fTestCase :: TestCase -> String
fTestCase (TestCase inp out _) = fInput inp ++ fOutput out ++ "\n"


fTestSuite :: [TestCase] -> String
fTestSuite ts = foldl (\acc tc -> acc ++ fTestCase tc ++ "\n") (printf "%d\n" $ length ts) ts
