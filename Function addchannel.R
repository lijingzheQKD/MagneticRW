###更新河道井点的H,addchannel
source_here = function(filename,nowdir = dirname(rstudioapi::getSourceEditorContext()$path)){
  fulldir = paste(nowdir,'/',filename,sep = '');  source(fulldir, encoding = encoding)}

Hchannel <- function(addchannel,
              xygrid1,
              basechannel,
              num5){
  addchannel = cbind(well.pointnumber = 1:length(addchannel[,1]),addchannel,isChannel = 0 )####添加井点编号，先将ischannel设置成水井
  for (b in 1:length(addchannel$y)){
    nowstep = addchannel[b,]####开始添加H
    distanceToNonChannelWells.vector1 = sqrt((nowstep$x - xygrid1$x)**2 + (nowstep$y - xygrid1$y)**2)  ###对每一个点进行遍历
    min.index = which.min(distanceToNonChannelWells.vector1)  ###找出最近点的编号
    tnowzht = xygrid1$z[min.index]
    nowzht = rbind(nowzht,tnowzht)  
    #####添加H完成
  }
  H = nowzht
  addchannel = cbind(addchannel,H)
  for (b in 1:length(addchannel$y)){####开始划分井点类型
    nowstep = addchannel[b,]####开始添加H
    distanceTotrackpoint.vector = sqrt((nowstep$x - basechannel$X)**2 + (nowstep$y - basechannel$Y)**2)
    minDistanceTotrackpoint = min(distanceTotrackpoint.vector) ###找到轨迹点与勘探井点的最小距离
    distancetochannelWells <- cbind(basechannel,distanceTotrackpoint.vector)####生成临时的data(加上了距离向量)
    df5 <- arrange(distancetochannelWells,distanceTotrackpoint.vector)###按照某一列排序，提取某一行，默认升序排列使用arrange函数需要下载dplyr包
    AlreadyChannelScattersXYH_add1 <-df5[1,]###提取最小的那一行
    if(minDistanceTotrackpoint < safetydistance){
      addchannel[c(b),][addchannel[c(b),] == "0"] = AlreadyChannelScattersXYH_add1$isChannel
    }}
    ###在这里应该分一下
  {
    Trackinitialwell = addchannel[addchannel$isChannel==20|addchannel$isChannel==10,]
    for (a in 1:length(Trackinitialwell$y)) {
      if(a < length(Trackinitialwell$y) ){
      Trackinitialwell1 = Trackinitialwell[a,]
      Trackinitialwell2 = Trackinitialwell[a+1,]
      tracknum = floor((Trackinitialwell1$well.pointnumber+Trackinitialwell2$well.pointnumber)/2)
      num1 = Trackinitialwell1$isChannel
      addchannel[c(initialpoint:tracknum),][addchannel[c(initialpoint:tracknum),] == "0"] = num1
      if(num1 == 10){addchannel[c(initialpoint:tracknum),]$H <- max(addchannel[c(initialpoint:tracknum),]$H)}else{
        addchannel[c(initialpoint:tracknum),]$H <- min(addchannel[c(initialpoint:tracknum),]$H)
      }
      initialpoint = tracknum 
      
      }else{
        Trackinitialwell3 = Trackinitialwell[a,]
        num3 = Trackinitialwell3$isChannel
        num4 = length(addchannel$y)
        addchannel[c(initialpoint:num4),][addchannel[c(initialpoint:num4),] == "0"] = num3
        if(num3 == 10){addchannel[c(initialpoint:num4),]$H <- max(addchannel[c(initialpoint:num4),]$H)}else{
          addchannel[c(initialpoint:num4),]$H <- min(addchannel[c(initialpoint:num4),]$H)
        }
        }
    }
  }

      addchannel1 <- addchannel[,c(1,2,3,5,4)]###调换两列的位置
      names(addchannel1)<-c("Well.identifier","X","Y","H","isChannel") ###此时列名会变成
      well.iden <- addchannel1[,c(1)]+num5
      addchannel1$Well.identifier <- well.iden####需要给well.pointnumber加上一个数保证，在运行的时候能够不与之前的那些点相矛盾
     ###将每条河道同类型的井点设置的深度一样
     #  minnumoil <- min(addchannel1$H)
    #   if(addchannel1$isChannel == 10){addchannel1$H <- min(addchannel1)}
       
    #####结束
      return(addchannel1)
}



#example
#if(num1 == 10){addchannel[c(initialpoint:tracknum),]$H <- max(addchannel[c(initialpoint:tracknum),]$H)}else{
#addchannel[c(initialpoint:tracknum),]$H <- min(addchannel[c(initialpoint:tracknum),]$H)
#}