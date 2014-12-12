import Control.Monad.State
import System.Random

type GeneratorState = State StdGen

data Tree a =
	Node a [Tree a]
	deriving (Show)

vertexNumWithEqualValAndDegree :: Tree Int -> Int
vertexNumWithEqualValAndDegree tree = 
	let
		func (Node a [])
			| a == 0 = 1
			| otherwise = 0
		func (Node a childs)
			| a == (length childs) = 1 + child_num
			| otherwise = child_num
			where
				child_num = foldl (+) 0 $ map func childs
	in
		func tree

generateList :: Int -> Int -> Int -> Int -> GeneratorState [Int]
generateList lborder hborder lenLborder lenHborder = do
	gen <- get
	let
		(listLen, newGen) = runState (randomInt lenLborder lenHborder) gen
		
		generateList' :: Int -> GeneratorState [Int]
		generateList' len = do
			cgen <- get
			case len of
				0 -> return []
				_ -> do
					let 
						(x, newGen')   = runState (randomInt lborder hborder) cgen
						(xs, newGen'') = runState (generateList' $ len - 1) newGen'
					put newGen''
					return (x:xs)

		(list, resGen) = runState (generateList' listLen) newGen
	put resGen
	return list

mapS :: (a -> GeneratorState b) -> [a] -> GeneratorState [b]
mapS f [] = do
	return []
mapS f (x:xs) = do
	gen <- get
	let
		(lhead, newGen) = runState (f x) gen
		(ltail, newGen') = runState (mapS f xs) newGen
	put newGen'
	return $ lhead:ltail

generateTree :: Int -> Int -> Int -> Int -> Int -> GeneratorState (Tree Int)
generateTree lborder hborder childNumLborder childNumHborder heigth = do
	gen <- get
	let
		(headVal, newGen) = runState (randomInt lborder hborder) gen

		generateTree' :: Int -> Int -> GeneratorState (Tree Int)
		generateTree' vertexVal heigth = do
			cgen <- get
			case heigth of 
				0 -> return (Node vertexVal []) 
				_ -> do 
					let
						(list, newGen') = runState (generateList lborder hborder childNumLborder childNumHborder) cgen
						(childTrees, newGen'') = runState (mapS (\c -> generateTree' c $ heigth - 1) list) newGen'
					put newGen''
					return (Node vertexVal childTrees)

		(tree, resGen) = runState (generateTree' headVal heigth) newGen
	put resGen
	return tree

randomInt :: Int -> Int -> GeneratorState Int
randomInt lborder hborder = do
	gen <- get
	let (num, newGenerator) = randomR (lborder, hborder) gen
	put newGenerator
	return num

main = do
	let
		testTrees = 
			[
				(Node 2 [(Node 1 [(Node 0 [])]), (Node 4 [])]),
				(Node 3 [(Node 2 [(Node 0 []), (Node 1 [])]), (Node 2 [(Node 4 []), (Node 2 [(Node 1 []), (Node 0 [])])])]),
				(evalState (generateTree 0 5 0 10 2) $ mkStdGen 0)
			]
	mapM_ 
		(\tree -> do
			let
				res = "tree:\n" ++ (show tree) ++ "\n" ++ "vertex number: " ++ 
						(show $ vertexNumWithEqualValAndDegree tree) ++ "\n"
			putStrLn res 
		)
		testTrees