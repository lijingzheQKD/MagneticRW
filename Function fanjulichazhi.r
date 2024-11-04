#本函数用于给空间一个点（x, y）插值，观测点数据来自df1，是一个至少三列的dataframe
#df1的第一列是x坐标，第二列是y坐标，第三列是观测值z
fanjulichazhi = function(x,y,df1,julizhishu = 2){
 #首先计算观测数据集有多少个点
  n = dim(df1)[1]
 #计算研究对象点与各个观测点的距离
  distance = rep(0,times = n)
  for (i in 1:n) {
    distance[i] = sqrt(
      (   x - df1[i,1]    )**2 + (   y - df1[i,2]   )**2
    ) 
  }
 
  #找出立着预测点最近的3个点
  distance1 = distance
  min1.index = which.min(distance1); distance1[min1.index] =  NA
  min2.index = which.min(distance1); distance1[min2.index] =  NA
  min3.index = which.min(distance1); distance1[min3.index] =  NA
  index.top.3 = c(min1.index, min2.index, min3.index)
  #更新df1和distance
  df2 = df1[index.top.3,]
  distance2 = distance[index.top.3]
  distance2[is.infinite(distance2)] = NA
  #计算公式的分子
  fenzi = sum( df2[,3]/(distance2**julizhishu),na.rm = TRUE )
 #计算公式的分母
  fenmu = sum( 1/(distance2**julizhishu),na.rm = TRUE )
  
  #组合分子与分母
  Z = fenzi / fenmu
  return(Z)

}





