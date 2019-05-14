x = [(Flat 10 35.4 10, 20000),(Room 3 24.1 5 10.2, 5000),(House 105.12, 120000),(Flat 10 54.1 32, 35000),(House 73.2, 80000), (Flat 3 26.1 5, 7500), (House 103.9, 125000)]
data Nedvig = Flat Int Double Int
                        | Room Int Double Int Double
                        | House Double deriving (Show, Eq)

type Agency = (Nedvig, Integer)

isHouse :: Nedvig -> Bool
isHouse (House _) = True
isHouse (Flat _ _ _) = False
isHouse (Room _ _ _ _) = False




getHouses :: [Agency] -> [Agency]
getHouses [] = []
getHouses (x:xs) = if (isHouse (fst x)) then x : getHouses xs
                                        else getHouses xs

getByPrice :: Integer -> [Agency] -> [Agency]
getByPrice _ [] = []
getByPrice a (x:xs) = if ((snd x) <= a) then x : getByPrice a xs
                                        else getByPrice a xs

fndFloor :: Int -> Nedvig -> Bool
fndFloor a (House _) = False
fndFloor a (Flat x _ _) = if (a==x) then True else False
fndFloor a (Room x _ _ _) = if (a==x) then True else False

fndGoodFloor :: Nedvig -> Bool
fndGoodFloor (House _) = False
fndGoodFloor (Flat x _ y) = if ((x/=1)&&(x/=y)) then True else False
fndGoodFloor (Room x _ y _) = if ((x/=1)&&(x/=y)) then True else False

getByLevel :: Int -> [Agency] -> [Agency]
getByLevel _ [] = []
getByLevel a (x:xs) = if (fndFloor a (fst x)) then x : getByLevel a xs
                                              else getByLevel a xs

getExceptBounds :: [Agency] -> [Agency]
getExceptBounds [] = []
getExceptBounds (x:xs) = if (fndGoodFloor (fst x)) then x : getExceptBounds xs
else getExceptBounds xs
