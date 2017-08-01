import Data.List (group)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

main = print $ encode "aaaabccaadeeee"
