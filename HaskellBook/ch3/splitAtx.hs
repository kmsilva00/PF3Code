splitAtx :: Int -> [a] -> ([a],[a])
splitAtx n ns = (take (n ns), drop (n ns))