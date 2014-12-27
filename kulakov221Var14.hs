import System.Random
import Data.List
data Tree a = Node a [Tree a]
	deriving (Show)

isLinear :: Tree a -> Bool
isLinear (Node a children)
	|(length children) == 0 = True
	|(length children) == 1 = isLinear (head children)
	|otherwise = False

buildRandomTree :: Int -> IO (Tree Int)
buildRandomTree h = do
	rootVal <- randomInt 100
	buildInnerRandomTree (Node rootVal []) h
	where
		buildInnerRandomTree (Node r c) h = do
		childrenCount <- randomInt 3
		childrenVals <- randomList childrenCount
		case h of
			0 -> return (Node r c)
			otherwise -> do
				children <- mapTree buildInnerRandomTree (createSublingsArr childrenVals) (h-1)
				return (Node r children)
		where
			createSublingsArr [x] = [Node x []]
			createSublingsArr (x:xs) = (Node x []):(createSublingsArr xs)
			mapTree f (x:xs) h = inner f (x:xs) h []
			inner f [] h acc = sequence acc
			inner f (x:xs) h acc = inner f xs h ((f x h):acc)

randomInt :: Int -> IO (Int)
randomInt n = randomRIO (1, n)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
	num <- randomRIO (1, 100)
	list <- randomList (n-1)
	return (num:list)

forTrueTest = Node 1 [Node 2 [Node 3 [Node 4 []]]]
forFalseTest = Node 1 [Node 2 [Node 4 []], Node 3 [Node 6 [], Node 7 []]]

main :: IO ()
main = do
	rndTree <- buildRandomTree 6
	print $ isLinear rndTree
