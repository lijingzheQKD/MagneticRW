# 计算左右下三个方向游走的概率（向上默认为零）
source_here = function(filename,nowdir = dirname(rstudioapi::getSourceEditorContext()$path)){
  fulldir = paste(nowdir,'/',filename,sep = '');  source(fulldir, encoding = encoding)}

probCompute = function(nowLocation
                      ,magneticDatabase
                      ,stabilityIndex = 3
                      ,co.horizontal = 1/500
                      ,OnlySouthward = FALSE
                      ,OnlyNorthward = FALSE
                      ,MagneticRadius = 25
                      ){
  source_here('Function fanjulichazhi.r')
  source_here('Function sum1.R')
  localValue = fanjulichazhi(nowLocation$X,nowLocation$Y,magneticDatabase)
  #左右上下的概率
  LeftValue = fanjulichazhi(nowLocation$X-MagneticRadius,nowLocation$Y,magneticDatabase)#;gradLeft = LeftValue - localValue
  RightValue = fanjulichazhi(nowLocation$X+MagneticRadius,nowLocation$Y,magneticDatabase)#;gradRight = RightValue - localValue
  DownValue = fanjulichazhi(nowLocation$X,nowLocation$Y-MagneticRadius,magneticDatabase)#;gradDown = DownValue - localValue
  UpValue =  fanjulichazhi(nowLocation$X,nowLocation$Y+MagneticRadius,magneticDatabase)#;gradUp = UpValue - localValue
  #左下和右下的概率
  LowerLeft = fanjulichazhi(nowLocation$X-MagneticRadius*0.707,nowLocation$Y-MagneticRadius*0.707,magneticDatabase)#;gradLowerleft = LowerLeft - localValue
  LowerRight = fanjulichazhi(nowLocation$X+MagneticRadius*0.707,nowLocation$Y-MagneticRadius*0.707,magneticDatabase)#;gradLowerRight = LowerRight - localValue
  UpperLeft = fanjulichazhi(nowLocation$X-MagneticRadius*0.707,nowLocation$Y+MagneticRadius*0.707,magneticDatabase)#;gradLowerleft = LowerLeft - localValue
  UpperRight = fanjulichazhi(nowLocation$X+MagneticRadius*0.707,nowLocation$Y+MagneticRadius*0.707,magneticDatabase)#;gradLowerRight = LowerRight - localValue
  #1右，2左，3上，4下，5左下，6右下，7左上，8右上
  allDirectionVec = c(RightValue ,LeftValue ,UpValue ,DownValue,       LowerLeft, LowerRight ,UpperLeft ,UpperRight )
  allDirectionGrad = allDirectionVec - localValue
  allDirectionGradExp = exp(allDirectionGrad)
  prob.original =  allDirectionGradExp**stabilityIndex
  if(OnlySouthward){prob.original[c(3,7,8)]=0}
  if(OnlyNorthward){prob.original[4:6]=0}
  # if(OnlySouthwest){prob.original[c(1,3,8)]=0}
  # if(OnlyNortheast){prob.original[c(2,5,4)]=0}
  prob = sum1(prob.original)*c(co.horizontal,co.horizontal,1,1,1,1,1,1)
  return(prob)
  }










# example
# prob = probCompute( nowLocation = readRDS('FirstSource.rds')
#                    ,magneticDatabase = readRDS('magneticDatabase.rds'))






















