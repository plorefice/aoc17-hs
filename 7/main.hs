import Data.List

data Proc  = Proc String Int [String] deriving (Show, Eq)
data Node  = Node Proc (Maybe Node) deriving Show
type Graph = [Node]

main = do
    input <- getContents
    let procs = procList . lines $ input
    print . root . build $ procs

procList :: [String] -> [Proc]
procList [] = []
procList (e:es) = proc : (procList es)
    where toks     = words e
          name     = toks !! 0
          weight   = read . init . tail $ toks !! 1
          children = if length toks < 3
                     then []
                     else map (filter (not . (==) ',')) . drop 3 $ toks
          proc     = Proc name weight children

build :: [Proc] -> Graph
build pl = build' pl . map (\p -> Node p Nothing) $ pl

build' :: [Proc] -> Graph -> Graph
build' [] g = g
build' (p:pl) g = build' pl $ connect p g

connect :: Proc -> Graph -> Graph
connect (Proc pn pw []) g = g
connect (Proc pn pw (c:cl)) g =
    let (Just parentNode)  = find (\(Node (Proc name _ _) _) -> name == pn) g
        (Just childIndex)  = findIndex (\(Node (Proc name _ _) _) -> name == c) g
        (Node childProc _) = g !! childIndex
        childNode          = Node childProc (Just parentNode)
        top                = take childIndex g
        bottom             = drop (childIndex + 1) g
    in  connect (Proc pn pw cl) $ top ++ (childNode : bottom)
 
root :: Graph -> Proc
root g = root' (g !! 0) g

root' :: Node -> Graph -> Proc
root' node@(Node proc _) g =
    case findParent node g of
        Nothing   -> proc
        (Just pn) -> root' pn g

findParent :: Node -> Graph -> Maybe Node
findParent (Node _ Nothing) g = Nothing
findParent (Node _ (Just (Node (Proc pname _ _) _))) g =
    find (\(Node (Proc name _ _) _) -> name == pname) g
