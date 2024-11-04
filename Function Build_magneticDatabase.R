# this function is used to make magneticdatabase based on points:
Build_magneticDatabase = function(
              myPtsRot,
              x.min ,
              y.min ,
              x.max ,
              y.max ,
              grid_size 
){
  #
  myPtsRot$isChannel[myPtsRot$isChannel!=0] = 10
  x.seq = seq(x.min,x.max,length.out = (x.max - x.min)/grid_size)
  y.seq = seq(y.min,y.max,length.out = (y.max - y.min)/grid_size)
  xygrid = expand.grid(x.seq,y.seq)
  xygrid$Z = 0
  names(xygrid) = c('X','Y','Z')
  #通过对 I0 进行向量化赋值，使之不进循环。
  I0 <- ifelse(myPtsRot$isChannel == 0, -10, myPtsRot$isChannel)
  for (i in seq_along(xygrid$X)) {
    dists <- sqrt((xygrid$X[i] - myPtsRot$X)^2 + (xygrid$Y[i] - myPtsRot$Y)^2)
    magnetic.add <- I0 * exp(-dists^2 / (grid_size * 200))
    xygrid$Z[i] <- sum(magnetic.add)
  }
  
  return(xygrid)
}
  



#example
if(0){
  nowdir = dirname(rstudioapi::getSourceEditorContext()$path);setwd(nowdir)
source("函数guiyihua，数字序列归一化.r")
myPtsRot = readRDS('myPtsRot.rds')
xygrid = Build_magneticDatabase(myPtsRot = myPtsRot)
plot(
  xygrid$X,
  xygrid$Y,
  pch=19,
  col = heat.colors(100)[guiyihua(xygrid$Z)*100]
)
}










