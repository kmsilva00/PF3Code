splitAtx :: Int -> [a] -> ([a],[a])
splitAt n xs = (take (n ns), drop (n xs))