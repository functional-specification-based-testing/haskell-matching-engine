module DataFlow.HieDUJungleGenerator
where

import System.Directory.Recursive
import GHC.Iface.Ext.Types
import Text.Regex
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified DataFlow.HieASTGraphGenerator as AG
import qualified GHC.Utils.Outputable as GHC

import qualified Debug.Trace as T


data GraphNode = DefNode String [GraphNode] | UseNode Span [String] [GraphNode]
  deriving (Show)

convertToGraphNode :: AG.AST -> GraphNode -> GraphNode
convertToGraphNode (AG.AST _ "VarPat"  _ [name]  ) (DefNode defName children) = DefNode defName ((DefNode name []):children)

convertToGraphNode (AG.AST s ""        _ [name]  ) (DefNode _ children) = (DefNode name children)
convertToGraphNode (AG.AST s "FunBind" children _) (DefNode name nodeChildren) = DefNode name (funcNode:nodeChildren)
  where funcNode = foldr convertToGraphNode (DefNode "" []) children

convertToGraphNode (AG.AST s "FunBind" children _) (UseNode span uses nodeChildren) = UseNode span uses (funcNode:nodeChildren)
  where funcNode = foldr convertToGraphNode (DefNode "" []) children

convertToGraphNode (AG.AST _ "HsVar" _      [name]) (UseNode s uses children) = UseNode s (name:uses) children 
convertToGraphNode (AG.AST s "HsIf"  [cond, first@(AG.AST fs _ _ _), second@(AG.AST ss _ _ _)] _ ) graphNode = 
  addChild (addChild condNode firstNode) secondNode
  where
    firstNode  = convertToGraphNode first  (UseNode fs [] [])
    secondNode = convertToGraphNode second (UseNode ss [] [])
    condNode = convertToGraphNode cond graphNode
    
convertToGraphNode (AG.AST _ "GRHS"  children@(first:_)  _   ) graphNode = addChild graphNode newNode
  where 
    (AG.AST s _ _ _) = first
    newNode = foldr convertToGraphNode (UseNode s [] []) children
convertToGraphNode (AG.AST s a     children  _  )   graphNode = (foldr convertToGraphNode graphNode children)

addChild :: GraphNode -> GraphNode -> GraphNode
addChild (DefNode name children) newChild  = DefNode name (newChild:children)
addChild (UseNode s uses children) newChild = UseNode s uses (newChild:children)

createDefMap :: GraphNode -> M.Map String GraphNode -> M.Map String GraphNode
createDefMap def@(DefNode name children) defMap  = M.insert name def childMap
  where childMap = foldr createDefMap defMap children
createDefMap (UseNode _ _ children) defMap = foldr createDefMap defMap children
createDefMap _ defMap = defMap

createUseArray :: GraphNode -> [GraphNode] -> [GraphNode]
createUseArray (UseNode _ [] children) useArray  = foldr createUseArray useArray children
createUseArray (DefNode _ children) useArray = foldr createUseArray useArray children
createUseArray (UseNode s uses children) useArray  = (UseNode s uses []):childArray
  where childArray = foldr createUseArray useArray children
createUseArray _ useArray = useArray

    
createDefPath :: GraphNode -> [String]
createDefPath (UseNode span uses children) = case children of
    [] -> (ppWhere):(subPaths)
    _  -> subPaths
  where
      subPaths = if isUseless 
        then concat $ fmap createDefPath children
        else concat $ fmap ( fmap ((ppWhere ++ "->") ++) . createDefPath) children
      ppWhere = GHC.renderWithContext GHC.defaultSDocContext ( GHC.ppr span )
      isUseless = length children == 1
createDefPath _ = []

createUsePath :: M.Map String GraphNode -> GraphNode -> [String]
createUsePath defMap (UseNode span uses _) = concat $ fmap createPath uses
  where
    createPath use = case M.lookup use defMap of
      Just (DefNode _ children) -> createDefPath (UseNode span uses children)
      _ -> []
    ppWhere = GHC.renderWithContext GHC.defaultSDocContext ( GHC.ppr span )
createUsePath _ _ = []

createDefUsePath :: GraphNode -> [String]
createDefUsePath node = concat $ fmap (createUsePath defMap) useArray
  where 
    defMap = createDefMap node M.empty
    useArray = createUseArray node []

convertToGraphNodeInit :: AG.AST -> GraphNode
convertToGraphNodeInit ast = convertToGraphNode ast (DefNode "" [])

uniq:: [String] -> [String]
uniq (a:ax) = if elem a ax then uniq ax else a:(uniq ax)
uniq [] = []

analyzePath :: String -> String -> String
analyzePath trace path = 
  case matchRegex (mkRegex regexString) trace of
       Just _ -> "Covered: " ++ path
       _ -> "NotCovered: " ++ path
  where 
    regexString = L.replace "->" ".*"  rParReplace
    rParReplace = L.replace ")" "[)]" lParReplace
    lParReplace = L.replace "(" "[(]" slashReplace
    slashReplace = L.replace "\\" "[\\]" path

analyzeCoverage :: String -> String -> Bool
analyzeCoverage trace path = 
  case matchRegex (mkRegex regexString) trace of
       Just _ -> True
       _ -> False
  where 
    regexString = L.replace "->" ".*"  rParReplace
    rParReplace = L.replace ")" "[)]" lParReplace
    lParReplace = L.replace "(" "[(]" slashReplace
    slashReplace = L.replace "\\" "[\\]" path

processTrace :: [String] -> [String] -> M.Map String GraphNode -> GraphNode -> [String]
processTrace _ [] _ _ = []

processTrace path trace defMap (DefNode _ children) = uniq  $ concat $ fmap (processTrace path trace defMap) children

processTrace path (current:rest) defMap (UseNode span usedSymbols []) = case current of 
    ppWhere -> [L.intercalate "->" nextPath]
    _ -> []
  where 
    createPath use = case M.lookup use defMap of
      Just node -> processTrace [] rest defMap node
      _ -> []
    ppWhere = GHC.renderWithContext GHC.defaultSDocContext ( GHC.ppr span )
    nextPath = path ++ [current]

processTrace path (current:rest) defMap (UseNode span usedSymbols children) = case current of 
    ppWhere -> uniq $ concat $ (fmap (processTrace (path ++ [current]) rest defMap) children) ++ (fmap createPath usedSymbols)
    _ -> []
  where 
    createPath use = case M.lookup use defMap of
      Just node -> processTrace [] rest defMap node
      _ -> []
    ppWhere = GHC.renderWithContext GHC.defaultSDocContext ( GHC.ppr span )
    nextPath = path ++ [current]

analyze :: String -> String -> IO (String)
analyze hieDir runFile = do
  files <- getFilesRecursive hieDir
  let 
    hieFiles = filter (L.isSuffixOf ".hie") files
  asts <- mapM (\f -> AG.loadAST f)  hieFiles
  runData <- readFile runFile
  let 
    concatedAsts = concat asts
    graphNodes = fmap convertToGraphNodeInit concatedAsts
    node = DefNode "ALL" graphNodes
    defMap = createDefMap node M.empty
    trace = lines runData
    result = getCoveredPath trace
  return $ (L.intercalate "\n" $ uniq $ result)

coverage :: String -> String -> IO (Int, Int)
coverage hieDir runFile = do
  files <- getFilesRecursive hieDir
  let 
    hieFiles = filter (L.isSuffixOf ".hie") files
  asts <- mapM (\f -> AG.loadAST f)  hieFiles
  runData <- readFile runFile
  let 
    concatedAsts = concat asts
    graphNodes = fmap convertToGraphNodeInit concatedAsts
    duPath = uniq $ createDefUsePath (DefNode "ALL" graphNodes)
    trace = L.intercalate "->" (lines runData)
    coverageData = fmap (analyzeCoverage trace) duPath
    totalCount = length coverageData
    coveredCount = length $ filter (== True) coverageData
  return (totalCount, coveredCount)

isDef :: [String] -> Bool
isDef (_:"D:":_) = True
isDef _ = False

getLoc :: [String] -> String
getLoc [_,_,loc] = loc
getLoc _ = []

isSameValue :: [String] -> [String] -> Bool
isSameValue (addr1:_) (addr2:_) = addr1 == addr2
isSameValue _ _ = False

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString sep str = 
    let (left, right) = break (==sep) str 
    in left : splitString sep (drop 1 right)

addToDefMap :: [[String]] -> M.Map String [String] -> M.Map String [String]
addToDefMap items@(first:_) pathMap = M.insert key value pathMap 
  where
    (key:_) = first
    value = reverse $ map getLoc items

addDefDataToUse :: M.Map String [String] -> [String] -> String
addDefDataToUse defMap usage = case defPath of
    Just def -> L.intercalate ">" $ loc:def
    Nothing -> []
  where 
    (key:_) = usage
    loc = getLoc usage
    defPath = M.lookup key defMap

getCoveredPath :: [String] -> [String]
getCoveredPath trace = uniq $ map (addDefDataToUse defMap) usageData 
  where 
    splitedData = map (splitString '#') trace
    deinfintionData = L.filter isDef splitedData
    usageData = L.filter (not . isDef) splitedData
    groupedData = L.groupBy isSameValue splitedData
    defMap = foldr addToDefMap M.empty groupedData