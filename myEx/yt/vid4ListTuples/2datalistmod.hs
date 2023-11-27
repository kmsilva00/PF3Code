import Data.List

--1:2:..:x-1:x

--head 1
-- tail 2:x

-- last x
--init 1:x-1

-- length

-- null , return true if empty, else false

-- [a] polymorphic list, list of (e.g. lists)



main ::IO()
main = do
    let result = fac 5
    putStrLn $ "result : " ++ show result
