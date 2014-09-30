import Data.VEBTree

import Test.QuickCheck


args :: Args
args = stdArgs {chatty = False, maxSuccess = 10000}

member_insert :: IO ()
member_insert = quickCheckWith args ((\v i -> member i (insert i v) == True) :: VEBTree -> Int -> Bool)

nomember_delete :: IO ()
nomember_delete = quickCheckWith args ((\v i -> member i (delete i v) == False) :: VEBTree -> Int -> Bool)

main =
	member_insert >>
	nomember_delete