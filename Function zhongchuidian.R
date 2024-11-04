# Function to compute the unit perpendicular vector of a line segment AB
# xA, yA: coordinates of point A
# xB, yB: coordinates of point B
unit_perpendicular <- function(xA, yA, xB, yB) {
  AB <- c(xB - xA, yB - yA)
  perp_AB <- c(-yB + yA, xB - xA)
  length_perp <- sqrt(sum(perp_AB^2))
  unit_perp_AB <- perp_AB / length_perp
  return(unit_perp_AB)
}


#计算两个中垂点
zhongchuidian = function(xA,yA,xB,yB,dh=1){
  xM = (xA + xB)/2
  yM = (yA + yB)/2
  #zhongchuidian1
  zcd1 = c(xM,yM) + unit_perpendicular(xA, yA, xB, yB)*dh#upperleft
  zcd2 = c(xM,yM) - unit_perpendicular(xA, yA, xB, yB)*dh#lowerright
  zcd = rbind(zcd1,zcd2)
  return(zcd)
}
# compute the unit perpendicular vector of line segment AB



if(0){
  xA <- 0
  yA <- 0
  xB <- 3
  yB <- 4
  C = zhongchuidian(xA, yA, xB, yB,5)
  
  plot(xA, yA,xlim = c(-9,9),ylim = c(-9,9),asp=1)
  points(xB,yB)
  points(C,col=6)
}




































