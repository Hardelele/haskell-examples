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

calcD :: [Double] -> [Double] -> [Double] -> Double -> Double
calcD line_p1 line_p2 circ_center r = (2 * (cx + k * b + k * cy))^2 - 4 * (1 + k^2) * (cx^2 + (b - cy)^2 - r)
									where
									  k = calcLineK line_p1 line_p2
									  b = calcLineB line_p1 k
									  cx = head circ_center
									  cy = last circ_center
lineInCircleCoords :: Double -> Double -> Double -> [Char]								  
lineInCircleCoords d k b
  | d < 0  = "no"
  | d == 0 = "yes, 1 point"
  | d > 0  = "yes, 2 points"

main = do
	   print $ "Enter x of circle center:"
	   input_cx <- getLine
	   print $ "Enter y of circle center:"
	   input_cy <- getLine
	   print $ "Enter r:"
	   input_r <- getLine
	   print $ "Enter x1 of line:"
	   input_lx1 <- getLine
	   print $ "Enter y1 of line:"
	   input_ly1 <- getLine
	   print $ "Enter x2 of line:"
	   input_lx2 <- getLine
	   print $ "Enter y2 of line:"
	   input_ly2 <- getLine
	   let circle = makePoint (read input_cx :: Double) (read input_cy :: Double)
	   let line_point1 = makePoint (read input_lx1 :: Double) (read input_ly1 :: Double)
	   let line_point2 = makePoint (read input_lx2 :: Double) (read input_ly2 :: Double)
	   let radius = (read input_r :: Double)
	   print $ lineInCircleCoords (calcD line_point1 line_point2 circle radius) (calcLineK line_point1 line_point2) (calcLineB line_point1 (calcLineK line_point1 line_point2))
