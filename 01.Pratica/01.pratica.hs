dobro :: Float -> Float
dobro x = 2 * x

quadruplicar :: Float -> Float
quadruplicar x = 2 * dobro x

hipotenusa :: Float -> Float -> Float
hipotenusa co ca = sqrt((co * co) + (ca * ca))

distancia :: Float -> Float -> Float -> Float -> Float
distancia ax ay bx by = hipotenusa (bx - ax) (by - ay)
