
-- list [1,2,3]
f = (reverse x) !! 1


-- list ["yes","no"]
f = (reverse x) !! 1
["no","yes"] !! 1 = yes

-- list [1,2,3]
g = reverse (x !! 1)
reverse 2 = error

--list["yes","no"]
g = reverse (x !! 1)
reverse "no" = "on"