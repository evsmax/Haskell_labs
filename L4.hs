data File = DataFile String Int | Folder String [File] deriving (Eq, Show)

dirAllTmp :: File -> String -> [String]
dirAllTmp (DataFile s _) path = [path ++ "/" ++ s]
dirAllTmp (Folder s[]) path = []
dirAllTmp (Folder s(x:xs)) path = (dirAllTmp x (path ++ "/" ++ s)) ++ (dirAllTmp (Folder s xs) path)

dirAll :: File -> [String]
dirAll x = dirAllTmp x ""

findTmp :: String -> File -> String -> String
findTmp x (DataFile s _) path = if s == x then (path ++ "/" ++ s)
                                          else  ""
findTmp x (Folder s []) path = ""
findTmp x (Folder s (p:ps)) path = (findTmp x p (path ++"/" ++ s)) ++ (findTmp x (Folder s ps ) path)

find :: String -> File -> String
find x y = findTmp x y ""

du :: File -> Int
du (DataFile _ x) = x
du (Folder s (x:xs)) = (du x) + (du (Folder s xs))
du (Folder s[]) = 0

one = Folder "home" [(Folder "img" []), (Folder "films" []),(Folder "doc" [(DataFile "lab1.hs" 112), (DataFile "lab2.hs" 4)])]
two = Folder "home" [(Folder "img" []), (Folder "films" [])]
three = Folder "home" [(Folder "doc" [(DataFile "lab2.hs" 4)])]
