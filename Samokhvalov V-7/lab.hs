import System.Random
import Data.List

data Tree a = Node a [Tree a]
	deriving (Show)

tree2List' level (Node val nodes) = (length nodes, level): concat (map (tree2List' (level+1)) nodes )
tree2List node = tree2List' 0 node

levels' list n acc
	| isFinish 	== True = acc
	| otherwise = levels' list (n - 1) ((sum [ fst x | x <- list, snd x == n] , n) : acc) 
	where
		isFinish = n < 0

levels list = levels' list (maximum [snd x | x <- list]) []

level tree = maximum $ levels $ tree2List tree

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
						where
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


test = Node 01 [Node 11  [Node 21 [Node 31 []], Node 22 [Node 31 [], Node 32 [], Node 33 [], Node 34 []]]]
t = tree2List test

main :: IO ()
main = do
	rndTree <- buildRandomTree 10
	let testExamples = 
		[
			Node 01 [Node 11 [], Node 12 [], Node 13 [], Node 14 []],
			Node 01 [Node 11  [Node 31 [Node 31 []] , Node 21  [Node 31 [], Node 32 [], Node 33 [], Node 34 []]]],
			rndTree
		]
	mapM_ 
		(\(i, tree) -> do
			putStrLn $ show i ++ ") nodes: " ++ (show $ fst $ level tree) ++ " level: " ++ (show $ snd $ level tree)
		)

		$ zip [1..] testExamples
