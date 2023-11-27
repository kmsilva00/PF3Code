init' xs = reverse ( tail ( reverse xs) )

init1' xs =  take ( (length xs )-1 ) xs