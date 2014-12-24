-- Кузнецов К.А К07-221
-- Вариант 17
-- Написать функцию, которая проверят находятся ли на одном уровне все листы дерева



import System.Random

data Tree a = Empty | Node a [Tree a]
	deriving(Show)

listOfHieght :: Tree a -> [Int]
listOfHieght tree = listOfHieght' tree 0 []
listOfHieght' (Node a []) acc listHieght = acc : listHieght
listOfHieght' (Node a children) acc listHieght = foldr (++) [] $ map (\x -> listOfHieght' x (acc + 1) listHieght ) children

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

checkTree :: [Int] -> Bool
checkTree (x:xs) = checkTree' (x:xs) x
checkTree' (x:xs) a
		| x == a = checkTree' xs x
		| otherwise = False
checkTree' [] a = True

testTree = checkTree . listOfHieght

test = Node 1 [Node 2 [], Node 3 [Node 5 []]]
test2 = Node 1 [Node 3 [], Node 4 [], Node 5 []]

main :: IO ()
main = do
		rn <- buildRandomTree 6
		let example = [Node 1 [Node 2 [], Node 3 [Node 5 []]] ,Node 1 [Node 3 [], Node 4 [], Node 5 []] ,rn]
		mapM_ (\tree -> do putStrLn (show $ testTree tree)) example


	
