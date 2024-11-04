#二维磁力随机行走函数
####更新7
##### 参数意义:
  # firstSource：初始XY坐标，data.frame，必须有X与Y两列； 
  # ,N = 100：醉汉行走的最大步数；
  # ,magneticDatabase：磁力地图，由磁力主程序计算得到,data.frame包括XYZ三列； 
  # ,drwMapRot：旋转后的河道观察点和非河道观察点，是一个list（包含baseWellsXY，channelWellsXY和non_channelWellsXY三个data.frame）； 
  # ,StepLength =26：随机行走的步长；
  # ,stabilityIndex = 2：稳定指数，这个数越大越稳定，即随机性越低；
  # ,safetyDistance = 10：触及非河道观察点的安全距离（小于这个距离就停止游走）；
  # ,AlreadyScattersXY_add = NULL：已经游走过形成散点的点，默认是NULL，如果不是NULL，则新的行走会绕开这些点；
  # ,channelwellsSkip = FALSE：先前行走过的点经历过的井，默认是FALSE，如果是TRUE，则跳过这些被先前走过的井（AlreadyScattersXY_add参数如果使用，则这个参数不需要使用）；
  # ,OnlySouthward = FALSE：是否只向南游走；
  # ,OnlyNorthward = FALSE：是否只向北游走；
  # ,OnlySouthwest = FALSE：是否只向西南游走；
  # ,OnlyNortheast = FALSE：是否只向东北游走。
  #最后四个参数至少开一个TRUE！！！
  
##### 返回值:
  # 一个数据框(2列)，包含每一步的x和y坐标。
# set.seed(random_seed)     ##设置随机数种子
source_here = function(filename,nowdir = dirname(rstudioapi::getSourceEditorContext()$path)){
  fulldir = paste(nowdir,'/',filename,sep = '');  source(fulldir, encoding =encoding)}



random_walk_2d <- function( firstSource 
                           ,N = 100
                           ,magneticDatabase 
                           ,drwMapRot 
                           ,StepLength =26
                           ,stabilityIndex = 2
                           ,safetyDistance = 10 #一般小于Steplength！！！
                           ,DistanceForAvoidAlreadyTouch = 15
                           ,co.horizontal = 1/300#横向的偏置系数，默认是1/300;
                           ,AlreadyScattersXY_add = NULL
                           ,channelwellsSkip = FALSE
                           ,OnlySouthward = FALSE
                           ,OnlyNorthward = FALSE
                           ,MagneticRadius = 25
                           ,tmyPtsRot
                           ){
  if((!OnlySouthward)&&(!OnlyNorthward)){OnlySouthward = TRUE}#默认情况向南游走！
  source_here('Function is.withinTheCircle.R')
  source_here('Function probCompute.r')
  
  x0 = firstSource$X  
  y0 = firstSource$Y 
  x <- x_temp <- x0 
  y <- y_temp <- y0
  non_channelWellsXY = drwMapRot$non_channelWellsXY
  baseWellsXY = drwMapRot$baseWellsXY
  channelWellsXY = drwMapRot$channelWellsXY
  kkk = 1
  while(TRUE){
    if(kkk>N){print("get the maximum step limit");break}
    # if(y_temp<min(magneticDatabase$Y)){break}
    # if(y_temp>max(magneticDatabase$Y)){break}
    # if(x_temp<min(magneticDatabase$X)){break}
    # if(x_temp>max(magneticDatabase$X)){break}
    #start判断是否出了边界=============================================
      n.baseWellsXY = length(baseWellsXY$X)
      index.chull = chull(baseWellsXY[,2:3])
      index.replace = (1:n.baseWellsXY)[-index.chull][1]
      baseWellsXY.copy = baseWellsXY
      baseWellsXY.copy$X[index.replace] = x_temp
      baseWellsXY.copy$Y[index.replace] = y_temp
      index.chull.replace = chull(baseWellsXY.copy[,2:3])
      # if(sum(index.chull)!= sum( index.chull.replace )|| length(index.chull)!= length( index.chull.replace )){print("out of map");break}
      xmax = max(baseWellsXY$X)+1*StepLength; xmin = min(baseWellsXY$X)-1*StepLength
      ymax = max(baseWellsXY$Y)+1*StepLength; ymin = min(baseWellsXY$Y)-1*StepLength
      if(x_temp > xmax || x_temp < xmin || y_temp > ymax || y_temp < ymin){print("out of map");break}
    #end判断是否出了边界=============================================
    #start判断是不是原地踏步===================================================
      if(length(x)>=5){
        center.x = x[length(x)-4]
        center.y = y[length(y)-4]
        center = c(center.x,center.y)
        radius = StepLength*1.15
        IsInCircle = is.withinTheCircle(center = center,radius = radius, p = data.frame(x=x[(length(x)-4):length(x)],y=y[(length(x)-4):length(x)]) )
        if(all(IsInCircle)){
          print("stray and stay")
          x_temp = x[length(x)] = x[(length(x)-1)]
          if(OnlyNorthward){
            y_temp = y[length(y)] = y[(length(y)-1)] + StepLength
            }else{
              y_temp = y[length(y)] = y[(length(y)-1)] - StepLength
                }
          }
      }
    #end判断是不是原地踏步===================================================
      
      
    ####(加)start由于油水保护机制更新了河道井，更新非河道井==============  
      drwMapRot = list()
      drwMapRot[['baseWellsXY']] = tmyPtsRot
      drwMapRot[['channelWellsXY']] = tmyPtsRot[tmyPtsRot$isChannel==20|tmyPtsRot$isChannel==10,]###加一个或语言
      drwMapRot[['non_channelWellsXY']] = tmyPtsRot[tmyPtsRot$isChannel==0,]
      non_channelWellsXY = drwMapRot$non_channelWellsXY
      baseWellsXY = drwMapRot$baseWellsXY
      channelWellsXY = drwMapRot$channelWellsXY
   ####(加)end更新完成
      
      
      #start判断是否碰到了无河道井观察，碰到就try again=============================================
      nowLocation = data.frame(X=x_temp,Y=y_temp)
      distanceToNonChannelWells.vector = sqrt((nowLocation$X - non_channelWellsXY$X)**2 + (nowLocation$Y - non_channelWellsXY$Y)**2)
      min.index = which.min(distanceToNonChannelWells.vector)
      nowNonChannelLocation.X = non_channelWellsXY$X[min.index]
      nowNonChannelLocation.Y = non_channelWellsXY$Y[min.index]
      minDistanceToNonChannelWells = min(distanceToNonChannelWells.vector)
      if(minDistanceToNonChannelWells<safetyDistance){
        if(kkk>1){
                  deltaX = x[length(x)] - x[(length(x)-1)]
                  x_temp = x[length(x)] + (x[length(x)]-nowNonChannelLocation.X)/abs((x[length(x)]-nowNonChannelLocation.X))*StepLength
                  x[length(x)] = x_temp
                  }
        print("touch non-channel wells, and try again")
      }
    #end判断是否碰到了无河道井观察，碰到就try again=============================================
      
    #start判断是否碰到了无河道井观察，碰到就停止=============================================
     nowLocation = data.frame(X=x_temp,Y=y_temp)
      distanceToNonChannelWells.vector = sqrt((nowLocation$X - non_channelWellsXY$X)**2 + (nowLocation$Y - non_channelWellsXY$Y)**2)
      minDistanceToNonChannelWells = min(distanceToNonChannelWells.vector)
      if(minDistanceToNonChannelWells<safetyDistance){
        if(kkk>1){x <- x[-length(x)]
        y <- y[-length(y)]
        x_temp = x[length(x)]
        y_temp = y[length(y)]}
        print("touch non-channel wells");break
      }
    #end判断是否碰到了无河道井观察，碰到就停止=============================================
      
    #start判断是否碰到了先前已经游走过的散点，碰到就停止=============================================
      AlreadyScattersXY_original = NULL
      AlreadyScattersXY = rbind( AlreadyScattersXY_original, AlreadyScattersXY_add)
      if(!is.null(AlreadyScattersXY)){
        nowLocation = data.frame(X=x_temp,Y=y_temp)
        distanceToAlreadyScatters.vector = sqrt((nowLocation$X - AlreadyScattersXY$X)**2 + (nowLocation$Y - AlreadyScattersXY$Y)**2)
        minDistanceToAlreadyScatters = min(distanceToAlreadyScatters.vector)
        if(minDistanceToAlreadyScatters<DistanceForAvoidAlreadyTouch){
          # if(kkk>1){x <- x[-length(x)];y <- y[-length(y)]}
          print("touch previous scatters");break
        }
      }
    #end判断是否碰到了先前已经游走过的散点，碰到就停=============================================
    #start跳过已经被游走过的井===================================================
      if(channelwellsSkip){
        distanceToChannelWells.vector = sqrt((nowLocation$X - channelWellsXY$X)**2 + (nowLocation$Y - channelWellsXY$Y)**2)
        distanceToChannelWells.vector = distanceToChannelWells.vector[distanceToChannelWells.vector!=0]
        minDistanceToChannelWells = min(distanceToChannelWells.vector)
        if(minDistanceToChannelWells<6.68){
          # if(kkk>1){x <- x[-length(x)];y <- y[-length(y)]}
          print("touch previous crossed wells");break
        } 
      }
    #end跳过已经被游走过的井===================================================

      ###油水保护机制结束
      #避免交叉
      nowLocation = data.frame(X=x_temp,Y=y_temp)
      distanceToAlreadyScattersXYall.vector = sqrt((nowLocation$X - PointsOfThisPeriodall$X)**2 + (nowLocation$Y  - PointsOfThisPeriodall$Y)**2)
      min.index = which.min(distanceToAlreadyScattersXYall.vector)
      nowAlreadyScattersXYallLocation.X = PointsOfThisPeriodall$X[min.index]
      nowAlreadyScattersXYallLocation.Y = PointsOfThisPeriodall$Y[min.index]
      mindistanceToAlreadyScattersXYall = min(distanceToAlreadyScattersXYall.vector)
      if(mindistanceToAlreadyScattersXYall<18){
        print("avoid river crossing")
        kkk = kkk+100
        }
      ###避免交叉结束

    #####油水矛盾机制 
      if(kkk < 101){
      nowLocation = data.frame(X=x_temp,Y=y_temp)
      library("dplyr")
      distanceToChannelWells.vector = sqrt((nowLocation$X - channelWellsXY$X)**2 + ( nowLocation$Y- channelWellsXY$Y)**2)###生成当前位置与河道点的距离向量
      minDistanceToChannelWells = min(distanceToChannelWells.vector)
      temporarychannelWells <- cbind(channelWellsXY,distanceToChannelWells.vector)####生成临时的data(加上了距离向量)
      df3 <- arrange(temporarychannelWells,distanceToChannelWells.vector)###按照某一列排序，提取某一行，默认升序排列使用arrange函数需要下载dplyr包
      AlreadyChannelScattersXYH_add <-df3[1,]###提取最小的那一行
      ###判断河道井是油井还是水井
      if(minDistanceToChannelWells<safetydistance){if(AlreadyChannelScattersXYH_add[['isChannel']] == 10){
        oilcomparechannel1 = tmyPtsRot[tmyPtsRot$isChannel==20,]
        oilcomparechannel1.subset<-subset(oilcomparechannel1, H<AlreadyChannelScattersXYH_add$H)
        tmyPtsRot[c(oilcomparechannel1.subset$Well.identifier),][tmyPtsRot[c(oilcomparechannel1.subset$Well.identifier),] == "20"] = 0
        #####不符合油水矛盾时输出临时的磁力地图
        x.min1 <- min(tmyPtsRot$X,na.rm=T)
        y.min1 <- min(tmyPtsRot$Y,na.rm=T)
        x.max1 <- max(tmyPtsRot$X,na.rm=T)
        y.max1 <- max(tmyPtsRot$Y,na.rm=T)
        print(AlreadyChannelScattersXYH_add$Well.identifier)
        grid_size <- round((x.max1 - x.min1)/50)
        x.seq <- seq(x.min1,x.max1,length.out = (x.max1 - x.min1)/grid_size)
        y.seq <- seq(y.min1,y.max1,length.out = (y.max1 - y.min1)/grid_size)
        tmagneticDatabase <- Build_magneticDatabase(myPtsRot = tmyPtsRot,
                                                    x.min = x.min1,
                                                    y.min = y.min1,
                                                    x.max = x.max1,
                                                    y.max = y.max1,
                                                    grid_size = grid_size)
        ####结束输出临时磁力地图
        drawmyRot <- subset(tmyPtsRot,select=-Well.identifier)
        drawmyRot <- subset(drawmyRot,select=- H)
        names(drawmyRot)<-c("X","Y","Z")
        drawmyRot1 <- subset(drawmyRot,drawmyRot$Z=="0")
        drawmyRot2 <- subset(drawmyRot,drawmyRot$Z=="10")
        drawmyRot3 <- subset(drawmyRot,drawmyRot$Z=="20")
        ###start ggplot the temporary magnetic maps
        v <- ggplot(tmagneticDatabase, aes(X, Y, z=Z))
        p12 = v + geom_contour_filled() + coord_equal()+
          geom_point(data = drawmyRot1, aes(X,Y), colour = 'grey', size = 3)+ 
          geom_point(data = drawmyRot2, aes(X,Y), colour = 'red', size = 3) + 
          geom_point(data = drawmyRot3, aes(X,Y), colour = 'blue', size = 3)
        t.p.name = paste(AlreadyChannelScattersXYH_add$Well.identifier+i*1000,sep = ',')
        # t.p.name = gsub(':',',',t.p.name)
        date = substring(Sys.time(),1,10)
        # path.ggsave = paste(getwd(),'/',date,sep = '')
        # ggsave(paste(t.p.name,'.jpg'),plot = p12,width = 50,height = 30,units = 'cm'
               # ,path = path.ggsave)
        ###end ggplot the temporary magnetic maps
        ####结束输出临时磁力地图
      }else {
        watercomparechannel1 = tmyPtsRot[tmyPtsRot$isChannel==10,]
        watercomparechannel1.subset<-subset(watercomparechannel1, H>AlreadyChannelScattersXYH_add$H)
        tmyPtsRot[c(watercomparechannel1.subset$Well.identifier),][tmyPtsRot[c(watercomparechannel1.subset$Well.identifier),] == "10"] = 0
        #####不符合油水矛盾时输出临时的磁力地图
        x.min2 <- min(tmyPtsRot$X,na.rm=T)
        y.min2 <- min(tmyPtsRot$Y,na.rm=T)
        x.max2 <- max(tmyPtsRot$X,na.rm=T)
        y.max2 <- max(tmyPtsRot$Y,na.rm=T)
        print(AlreadyChannelScattersXYH_add$Well.identifier)
        grid_size <- round((x.max2 - x.min2)/50)
        x.seq <- seq(x.min2,x.max2,length.out = (x.max2 - x.min2)/grid_size)
        y.seq <- seq(y.min2,y.max2,length.out = (y.max2 - y.min2)/grid_size)
        tmagneticDatabase <- Build_magneticDatabase(myPtsRot = tmyPtsRot,
                                                    x.min = x.min2,
                                                    y.min = y.min2,
                                                    x.max = x.max2,
                                                    y.max = y.max2,
                                                    grid_size = grid_size)
        ####结束输出临时磁力地图
        drawmyRot <- subset(tmyPtsRot,select=-Well.identifier)
        drawmyRot <- subset(drawmyRot,select=- H)
        names(drawmyRot)<-c("X","Y","Z")
        drawmyRot1 <- subset(drawmyRot,drawmyRot$Z=="0")
        drawmyRot2 <- subset(drawmyRot,drawmyRot$Z=="10")
        drawmyRot3 <- subset(drawmyRot,drawmyRot$Z=="20")
        ###start ggplot the temporary magnetic maps
        v <- ggplot(tmagneticDatabase, aes(X, Y, z=Z))
        p12 = v + geom_contour_filled() + coord_equal()+
          geom_point(data = drawmyRot1, aes(X,Y), colour = 'grey', size = 3)+ 
          geom_point(data = drawmyRot2, aes(X,Y), colour = 'red', size = 3) + 
          geom_point(data = drawmyRot3, aes(X,Y), colour = 'blue', size = 3)
        t.p.name = paste(AlreadyChannelScattersXYH_add$Well.identifier+i*1000,sep = ',')
        # t.p.name = gsub(':',',',t.p.name)
        # date = substring(Sys.time(),1,10)
        # path.ggsave = paste(getwd(),'/',date,sep = '')
        # ggsave(paste(t.p.name,'.jpg'),plot = p12,width = 50,height = 30,units = 'cm'
               # ,path = path.ggsave)
        ###end ggplot the temporary magnetic maps
         }
      print("new magnetic map")}}
      #tmagneticDatabase = updataChannelMagneticDatabase(
      #  tmyPtsRot =tmyPtsRot,
      #  x.min = x.min,
      #  y.min = y.min,
      #  x.max = x.max,
      #  y.max = y.max,
      #  grid_size = grid_size,
      # nowLocation = data.frame(X=x_temp,Y=y_temp),
      # channelWellsXY = channelWellsXY
      #)
      
      ####结束输出临时磁力地图
      
    rand <- sample.int( 8
                       ,1
                       ,prob = probCompute(nowLocation =  nowLocation    
                                           ,magneticDatabase = tmagneticDatabase 
                                           ,stabilityIndex = stabilityIndex
                                           ,co.horizontal = co.horizontal
                                           ,OnlySouthward = OnlySouthward
                                           ,OnlyNorthward = OnlyNorthward
                                           ,MagneticRadius = 25
                                           )**4
                       )   ##产生随机数
    
    if(rand==1L){            ##如果为1, 向右走一步，x坐标加1
      x_temp <- x_temp+StepLength
    }else if(rand==2L){       ##如果为2, 向左走一步，x坐标减1
      x_temp <- x_temp-StepLength
    }else if(rand==3L){       ##如果为3, 向上走一步，y坐标加1
      y_temp <- y_temp+StepLength
    }else if(rand==4L){       ##如果为4, 向下走一步，y坐标减1
      y_temp <- y_temp-StepLength
    }else if(rand==5L){       ##如果为5, 向左下走一步，x坐标减0.707，y坐标减0.707
      x_temp <- x_temp-StepLength*0.707;         y_temp <- y_temp-StepLength*0.707
    }else if(rand==6L){       ##如果为6, 向右下走一步，x坐标加0.707，y坐标减0.707
      x_temp <- x_temp+StepLength*0.707;         y_temp <- y_temp-StepLength*0.707
    }else if(rand==7L){       ##如果为7, 向左上走一步，x坐标减0.707，y坐标加0.707
      x_temp <- x_temp-StepLength*0.707;         y_temp <- y_temp+StepLength*0.707
    }else{                    ##如果为8, 向右上走一步，x坐标加0.707，y坐标加0.707
      x_temp <- x_temp+StepLength*0.707;         y_temp <- y_temp+StepLength*0.707
    }
    
    x <- c(x, x_temp)
    y <- c(y, y_temp)
    print(kkk)
    kkk = kkk+1
    print(x_temp)
    print(x_temp)
    }
  result <- data.frame( x=x, y=y)   ##结果数据为数据框
  return(result)   ##返回结果
}


# example
# source('~/R/自编的常用R函数/RandomWalking项目/算出磁力地图map，算出MagenticDatabase，磁力图，磁场图.R')
# PointsOfThisPeriod = list()
# for (i in 1:length(channelWellsXY$Y)) {
#   
#   Northresult = random_walk_2d(  channelWellsXY[i,] 
#                            ,N =  100
#                            ,magneticDatabase = readRDS('magneticDatabase.rds')
#                            ,drwMapRot = readRDS("E:\\胜2\\new\\直井的测井曲线\\drwMapRot.rds")
#                            ,StepLength = 29
#                            ,stabilityIndex = 1
#                            ,safetyDistance =9
#                            ,OnlyNortheast = TRUE)
#   Southresult = random_walk_2d(  channelWellsXY[i,] 
#                             ,N =  100
#                             ,magneticDatabase = readRDS('magneticDatabase.rds')
#                             ,drwMapRot = readRDS("E:\\胜2\\new\\直井的测井曲线\\drwMapRot.rds")
#                             ,StepLength = 29
#                             ,stabilityIndex = 9
#                             ,safetyDistance = 50
#                             ,OnlySouthwest = TRUE)
#   PointsOfThisPeriod[[i]] = rbind(Northresult,Southresult)
#     
#   points(rbind(Northresult,Southresult)[,c(1,2)],pch=19)
# }
# for (i in 1:length(PointsOfThisPeriod)) {
#   lines(x = PointsOfThisPeriod[[i]]$x, y = PointsOfThisPeriod[[i]]$y,col='purple',lwd = 5)
# }









