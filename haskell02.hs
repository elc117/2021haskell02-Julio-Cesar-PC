-- Prática 02 de Haskell
-- Nome: Julio Cesar Polmann Cuencas

eFebre :: Float -> Bool
eFebre x = if x > 37.80 then True else False

comFebre :: [Float] -> [Float]
comFebre f = filter eFebre f

comFebre' :: [Float] -> [Float]
comFebre' f = filter (\f -> f > 37.80) f

itemize :: [String] -> [String]
itemize s = map (\s -> "<li>"++s++"</li>") s

bigCircles :: Float -> [Float] -> [Float]
bigCircles n f = filter ((\x y -> x < (pi*(y^2))) n) f

quarentena :: [(String,Float)] -> [(String,Float)]
quarentena tupla = filter (\(_,t) -> t > 37.8) tupla

idadesEm :: [Int] -> Int -> [Int]
idadesEm list ano = map ((\x y -> x - y)ano) list

changeNames :: [String] -> [String]
changeNames list = map (\x -> if head x == 'A' then "Super " ++ x  else x) list

onlyShorts :: [String] -> [String]
onlyShorts list = filter (\x -> length x < 5) list