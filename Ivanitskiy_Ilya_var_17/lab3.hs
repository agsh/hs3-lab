import System.Random
data Tree a = Empty | Node a [Tree a] 
	deriving (Show, Eq)

incNonOdd :: (Num a) => Tree a -> Tree a
incNonOdd Empty = Empty
incNonOdd (Node t x) = Node (t+1) (map nonIncNonOdd x)

nonIncNonOdd :: (Num a) => Tree a -> Tree a
nonIncNonOdd Empty = Empty
nonIncNonOdd (Node t x) = Node t (map incNonOdd x)

buildingTreeWithRandom h = do
	rootVal <- createRandomInt 10
	buildInsideTree (Node rootVal []) h
		where 	
			buildInsideTree (Node r c) h = do
			branchsCount <- createRandomInt 3
			branchsVals <- randomList branchsCount
			case h of 
				0 -> return (Node r c)
				otherwise -> do
					branchs <- mapTree buildInsideTree (tempArr branchsVals) (h-1)
					return (Node r branchs)
			where 	
				tempArr [x] = [Node x []]
				tempArr (x:xs) = (Node x []):(tempArr xs)
				mapTree f (x:xs) h = inner f (x:xs) h []
					where 	
						inner f [] h acc = sequence acc
						inner f (x:xs) h acc = inner f xs h ((f x h):acc)

createRandomInt :: Int -> IO (Int)
createRandomInt n = randomRIO (1, n)

randomList :: Int -> IO [Int] 
randomList 0 = return [] 
randomList n = do
	num <- randomRIO (1, 10) 
	list <- randomList (n-1) 
	return (num:list)

lab3 = do
	putStrLn "Testing...\n\n"
	randomGeneratedTree <- buildingTreeWithRandom 5
	let treeTest = 
		[Empty, Node 2 [Node 3 [Node 4 []], Node 7 [], Node 9 [Node 17[],Node 666[]]],Node 0 [Node 2 [Node 3 [Node 4 [Node 5 [Node 6 [Node 7 []]]]]]],Node 0 [Node 2[Node 17[Node 17[]]],Node 5[Node 1[Node 1[]]],Node 4[Node 17[Node 7[]]],Node 3[Node 17[Node 17[]]]],randomGeneratedTree]
	mapM_ (\tree -> putStrLn $ "Each element on the odd level add 1\nOriginal tree:\n" ++ (show tree) ++ "\nResult tree: \n" ++ (show $ nonIncNonOdd tree) ++ "\n"
		) treeTest