import Data.VEBTree
import Debug.Trace

import Test.QuickCheck


args :: Args
args = stdArgs --{chatty = False, maxSuccess = 100}

member_insert :: IO ()
member_insert = verboseCheck ((\l i -> 
	let v = fromList l 
	 in member i (insert i v) == True || i<0 || i >= (u v)) :: [Int] -> Int -> Bool)


nomember_delete :: IO ()
nomember_delete = quickCheckWith args ((\v i -> member i (delete i v) == False) :: VEBTree -> Int -> Bool)

main =
	member_insert >>
	nomember_delete
