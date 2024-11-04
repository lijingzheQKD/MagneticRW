# this function is used to rotate the coordinates in 2d map:
# pts is a 2-col matrix or dataframe;
# degreeIndRad is anti_clockwise.
# the output is also a 2-col matrix or dataframe.
rotation = function(pts,degreeInRad){
  output = pts
  for (i in 1:length(pts[,1])) {
    now.pts = as.matrix(pts)[i,]
    now.mat = rbind(c(cos(degreeInRad),-sin(degreeInRad)),
                    c(sin(degreeInRad), cos(degreeInRad))     )
    new.pts = as.vector(now.mat %*% now.pts)
    output[i,] = new.pts
  }
  return(output)
}
# example
if(0){
  pts = data.frame(x=1:10,y=1:10)
  degreeInRad = pi/2
  plot(pts,xlim = range(-10,10),ylim = range(-10,10),asp=1)
  pts.new = rotation(pts,degreeInRad)
  points(pts.new,col=6)
}