import System.Random

data Tree a = Node a [Tree a] 
	deriving (Show)

checkMax a | a == [] = (-1)
		   | otherwise = maximum a

treeFold :: (a -> [t] -> t) -> Tree a -> t
treeFold node = f 
	where
		f (Node a ts) = node a (map f ts)

height :: Tree a -> Integer
height = treeFold node 
	where
      node val subTrees = 1 + checkMax subTrees

example :: Tree Integer
example = Node 3 [Node 2 [], Node 5 [Node 2 [],Node 1 [Node 5 []]], Node 1 []]
		
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

ioTreeOutput :: Show a => IO a -> IO ()
ioTreeOutput ioTree = do
	tree <- ioTree
	putStrLn $ show tree

randomInt :: Int -> IO (Int)
randomInt n = randomRIO (1, n)

randomList :: Int -> IO [Int] 
randomList 0 = return [] 
randomList n = do
	num <- randomRIO (1, 100) 
	list <- randomList (n-1) 
	return (num:list)

main :: IO ()
main = do
	rndTree <- buildRandomTree 5
	let testExamples = 
		[
			Node 1 [Node 2 [Node 3 []], Node 4 []],
			Node 1 [Node 2 [Node 3 [Node 4 [Node 5 [Node 6 [Node 7 []]]]]]],
			rndTree
		]
	mapM_ 
		(\tree -> do
			putStrLn $ "-----Test case-----\nThe height of tree: \n" ++ (show tree) ++ 
				"\nis " ++ (show $ height tree) ++ "\n-------------------\n"
		)
		testExamples

