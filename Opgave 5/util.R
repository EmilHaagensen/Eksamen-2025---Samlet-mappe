calculate_triangle_area <- function(x1, y1, x2, y2, x3, y3) {
  area <- 0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  return(area)
}


is_opponent_inside_triangle <- function(o_x, o_y,shooter_x,shooter_y) {
  # Coordinates of the three vertices of the triangle
  #goalkeeper_x=117
  #goalkeeper_y=40
  x1 <- 120
  y1 <- 44
  x2 <- 120
  y2 <- 36
  x3 <- shooter_x
  y3 <- shooter_y
  
  # Calculate the area of the entire triangle
  total_area <- calculate_triangle_area(x1, y1, x2, y2, x3, y3)
  
  # Calculate the area of three sub-triangles formed by the goalkeeper and two vertices of the triangle
  area1 <- calculate_triangle_area(o_x, o_y, x2, y2, x3, y3)
  area2 <- calculate_triangle_area(x1, y1, o_x, o_y, x3, y3)
  area3 <- calculate_triangle_area(x1, y1, x2, y2, o_x, o_y)
  
  # If the sum of the areas of the sub-triangles is equal to the total area, the goalkeeper is inside the triangle
  trisum = (area1 + area2 + area3)
  diff=trisum - total_area < 0.1
  return(diff)
}

generate_pass_cone <- function(from, to, width_deg = 3, length = NULL, points = 10) {
  if (is.null(length)) {
    length <- sqrt((to["x"] - from["x"])^2 + (to["y"] - from["y"])^2)
  }
  
  width_rad <- (width_deg / 180) * pi
  angle <- atan2(to["y"] - from["y"], to["x"] - from["x"])
  angles <- seq(angle - width_rad, angle + width_rad, length.out = points)
  
  data.frame(
    x = c(from["x"], from["x"] + cos(angles) * length),
    y = c(from["y"], from["y"] + sin(angles) * length)
  )
}


# Shotcone-funktion mod stolperne
generate_shot_cone <- function(shooter, goal_x = 120, goal_y_top = 44, goal_y_bottom = 36) {
  data.frame(
    x = c(shooter["x"], goal_x, goal_x),
    y = c(shooter["y"], goal_y_top, goal_y_bottom)
  )
}
