makePoint :: Double -> Double -> [Double]
makePoint x y = [x,y]

calc_line_k :: [Double] -> [Double] -> Double
calc_line_k line_point1 line_point2 = (line_point1_y + line_point2_y) / (line_point1_x + line_point2_x)
where
line_point1_x = head line_point1
line_point2_x = head line_point2
line_point1_y = last line_point1
line_point2_y = last line_point2
	
calc_line_b :: [Double] -> [Double] -> Double
calc_line_b line_point1 line_point2 = line_point_y - line_k * line_point_x
where
line_point_x = head line_point1
line_point_y = last line_point1
line_k       = calc_line_k line_point1 line_point2
								  
calc_discriminant :: [Double] -> [Double] -> [Double] -> Double -> Double
calc_discriminant line_point1 line_point2 circle_center radius = b^2 - 4 * a * c
where
line_k          = calc_line_k line_point1 line_point2
line_b          = calc_line_b line_point1 line_point2
circle_center_x = head circle_center
circle_center_y = last circle_center
a               = 1 + line_k^2
b               = 2 * (circle_center_x + line_k * line_b + line_k * circle_center_y)
c               = circle_center_x^2 + (line_b - circle_center_y)^2 - radius
									  
intersections :: [Double] -> [Double] -> [Double] -> Double -> [Char]								  
intersections line_point1 line_point2 circle_center radius
| condition1 = " X:" ++ show line_point1_x ++ " Y:" ++ show circle_center_y
| d < 0      = "Have no intersections!"
| d == 0     = " X:" ++ show result_x_d0 ++ " Y:" ++ show result_y_d0
| d > 0      = " X1:" ++ show result_x1 ++ " Y1:" ++ show result_y1 ++ " X2:" ++ show result_x2 ++ " Y2:" ++ show result_y2
where
circle_center_x = head circle_center
circle_center_y = last circle_center
line_point1_x   = head line_point1
line_point2_x   = head line_point2
result_x_d0     = b / (2 * a)
result_y_d0     = line_k * result_x_d0 + line_b
result_x1       = (b + discriminant) / (2 * a)
result_x2       = line_k * result_x1 + line_b
result_y1       = (b - discriminant) / (2 * a)
result_y2       = line_k * result_x2 + line_b
condition1      = (line_point1_x == line_point2_x) && ((line_point1_x == (circle_center_x + radius)) || (line_point1_x == (circle_center_x - radius)))
line_k          = calc_line_k line_point1 line_point2
line_b          = calc_line_b line_point1 line_point2
discriminant    = calc_discriminant line_point1 line_point2 circle_center radius
a               = 1 + line_k^2
b               = 2 * (circle_center_x + line_k * line_b + line_k * circle_center_y)
c               = circle_center_x^2 + (line_b - circle_center_y)^2 - radius
	
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
	   let circle      = makePoint (read input_cx :: Double) (read input_cy :: Double)
	   let line_point1 = makePoint (read input_lx1 :: Double) (read input_ly1 :: Double)
	   let line_point2 = makePoint (read input_lx2 :: Double) (read input_ly2 :: Double)
	   let radius      = (read input_r :: Double)
	   print $ intersections line_point1 line_point2 circle radius