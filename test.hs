makePoint :: Double -> Double -> [Double]
makePoint x y = [x,y]

r = 10.0
circleCenter = makePoint 1 1
somePoint = makePoint 1 1

calcLineK :: [Double] -> [Double] -> Double
calcLineK p1 p2 = (y1 + y2) / (x1 + x2)
  where
    x1 = head p1
    x2 = head p2
    y1 = last p1
    y2 = last p2
    
calcLineB :: [Double] -> Double -> Double
calcLineB p k = y - k * x
  where
    y = last p
    x = head p

pointsEqual :: [Double] -> [Double] -> [Char]
pointsEqual p1 p2 = if p1==p2 then "yes" else "no"

pointInCircle :: [Double] -> [Double] -> Double -> [Char] 
pointInCircle p c r 
  | ((px - cx) ^ 2) + ((py -cy) ^ 2) < (r ^2) = "yes"
  | otherwise = "no"
  where 
	px = head p
	py = last p
	cx = head c
	cy = last c
					
pointEqualCircleCenter = pointInCircle somePoint circleCenter r

main = do
	   print $ "Enter x of circle center:"
	   input_cx <- getLine
	   print $ "Enter y of circle center:"
	   input_cy <- getLine
	   print $ "Enter r:"
	   input_r <- getLine
	   print $ "Enter x of point:"
	   input_px <- getLine
	   print $ "Enter y of point:"
	   input_py <- getLine
	   let circle = makePoint (read input_cx :: Double) (read input_cy :: Double)
	   let point = makePoint (read input_px :: Double) (read input_py :: Double)
	   let radius = (read input_r :: Double)
	   print $ pointInCircle point circle radius
