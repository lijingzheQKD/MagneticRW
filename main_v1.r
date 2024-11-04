encoding = 'UTF-8'
nowdir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(nowdir)
# Load necessary packages and functions
source_here = function(filename, nowdir = dirname(rstudioapi::getSourceEditorContext()$path)) {
  fulldir = paste(nowdir, '/', filename, sep = '')
  source(fulldir, encoding = encoding)
}
source_here('Function random_walk.R')
source_here("Function Build_magneticDatabase.R")
source_here("Function rotation.R")
source_here('Function addchannel.R')
source_here('Function UpperLeftShift and LowerRightShift.R')
library(dplyr)

# Create magneticDatabase and drwMapRot
myPts = read.csv('supplement1.csv')  # 0 is non-channel, 10 is oil well, 20 is water well

set.seed(2)

# Set parameters
N.max = 100  # Maximum steps
rotationAngleInRadiansAnti_clockwise = 0  # Rotation angle
StepLength = 25  # Step length
co.horizontal = 1/500  # Horizontal bias coefficient
stabilityIndex = 1  # Stability coefficient (reduce randomness)
safetyDistance = 30  # Minimum distance from non-channel wells
safetydistance = 30  # Oil-water protection
DistanceForAvoidAlreadyTouch = 35
AlreadyChanneloilScattersXYH = NULL
AlreadyChannelwaterScattersXYH = NULL
PointsOfThisPeriodall = NULL
nowzht = NULL
initialpoint = 1

# Preprocessing and observation
col.plot = myPts$isChannel
col.plot[col.plot == 0] = "grey"
col.plot[col.plot == 20] = "blue"
col.plot[col.plot == 10] = "red"
plot(myPts$X, myPts$Y, col = col.plot, pch = 19, asp = 1)

myPtsRot = myPts
myPtsRot[, 2:3] = rotation(myPtsRot[, 2:3], rotationAngleInRadiansAnti_clockwise)
tmyPtsRot = myPtsRot
plot(myPtsRot$X, myPtsRot$Y, col = col.plot, pch = 19, asp = 1)

# Elevation base map information
library(ggplot2)
library(SpatialExtremes)
data.now = myPtsRot
index.nulls = which(is.na(data.now$Y))
data.now = data.now[!is.na(data.now$Y), ]
x = data.now$X
y = data.now$Y
z = data.now$H
Well.identifier = data.now$Well.identifier
original.xyz.pts = data.frame(x, y, z, Well.identifier)
xo = seq(min(x), max(x), length = 100)
yo = seq(min(y), max(y), length = 100)
xlim = range(6000, 9500)
ylim = range(5500, 8000)

# Use akima for interpolation
inter_lin = akima::interp(x, y, z, xo = xo, yo = yo, linear = FALSE, extrap = TRUE, duplicate = TRUE)
xygrid1 = expand.grid(x = xo, y = yo)
xygrid1$z = as.vector(inter_lin$z)
xygrid1$z[xygrid1$z < 0] = 0
inter_lin$z[inter_lin$z < 0] = 0
all(inter_lin$z == matrix(xygrid1$z, nrow = length(yo)))
contour(xo, yo, inter_lin$z, add = TRUE)

# Create drwMapRot
drwMapRot = list()
drwMapRot[['baseWellsXY']] = myPtsRot
drwMapRot[['channelWellsXY']] = myPtsRot[myPtsRot$isChannel == 20 | myPtsRot$isChannel == 10, ]
drwMapRot[['non_channelWellsXY']] = myPtsRot[myPtsRot$isChannel == 0, ]
basechannel = drwMapRot$channelWellsXY

# Create magneticDatabase
x.min = min(myPtsRot$X)
y.min = min(myPtsRot$Y)
x.max = max(myPtsRot$X)
y.max = max(myPtsRot$Y)
grid_size = round((x.max - x.min) / 50)
x.seq = seq(x.min, x.max, length.out = (x.max - x.min) / grid_size)
y.seq = seq(y.min, y.max, length.out = (y.max - y.min) / grid_size)
magneticDatabase = Build_magneticDatabase(myPtsRot = myPtsRot,
                                          x.min = x.min,
                                          y.min = y.min,
                                          x.max = x.max,
                                          y.max = y.max,
                                          grid_size = grid_size)
tmagneticDatabase = magneticDatabase

# Prepare for plotting
rr = tmagneticDatabase$X
tt = tmagneticDatabase$Y
gg = tmagneticDatabase$Z

drawmyRot <- subset(myPtsRot, select = -Well.identifier)
drawmyRot <- subset(drawmyRot, select = -H)
names(drawmyRot) <- c("X", "Y", "Z")
drawmyRot1 <- subset(drawmyRot, drawmyRot$Z == "0")
drawmyRot2 <- subset(drawmyRot, drawmyRot$Z == "10")
drawmyRot3 <- subset(drawmyRot, drawmyRot$Z == "20")

date = substring(Sys.time(), 1, 10)

v <- ggplot(tmagneticDatabase, aes(X, Y, z = Z))

p11 = v + geom_contour_filled(bins = 9, global.breaks = TRUE) + coord_equal() +
  geom_point(data = drawmyRot1, aes(X, Y), colour = 'grey', size = 1.5) + 
  geom_point(data = drawmyRot2, aes(X, Y), colour = 'red', size = 1.5) + 
  geom_point(data = drawmyRot3, aes(X, Y), colour = 'blue', size = 1.5)

# Random walk (south and north directions)
PointsOfThisPeriod = list()
PointsOfAlreadyChannelScattersXYH = list()
for (i in 1:length(drwMapRot$channelWellsXY$Y)) {
  Southresult = random_walk_2d(drwMapRot$channelWellsXY[i, ],
                               N = N.max,
                               magneticDatabase = tmagneticDatabase,
                               drwMapRot = drwMapRot,
                               StepLength = StepLength,
                               co.horizontal = co.horizontal,
                               stabilityIndex = stabilityIndex,
                               safetyDistance = safetyDistance,
                               DistanceForAvoidAlreadyTouch = DistanceForAvoidAlreadyTouch,
                               OnlySouthward = TRUE,
                               MagneticRadius = 30,
                               tmyPtsRot = tmyPtsRot)
  Northresult = random_walk_2d(drwMapRot$channelWellsXY[i, ],
                               N = N.max,
                               magneticDatabase = tmagneticDatabase,
                               drwMapRot = drwMapRot,
                               StepLength = StepLength,
                               co.horizontal = co.horizontal,
                               stabilityIndex = stabilityIndex,
                               safetyDistance = safetyDistance,
                               DistanceForAvoidAlreadyTouch = DistanceForAvoidAlreadyTouch,
                               OnlyNorthward = TRUE,
                               MagneticRadius = 25,
                               tmyPtsRot = tmyPtsRot)
  num5 = i * 1000
  PointsOfThisPeriod[[i]] = rbind(Northresult[length(Northresult$x):1, ], Southresult)
  PointsOfThisPeriod2 = Hchannel(addchannel = PointsOfThisPeriod[[i]],
                                 xygrid1 = xygrid1,
                                 basechannel = basechannel,
                                 num5 = num5)
  PointsOfThisPeriodall = rbind(PointsOfThisPeriodall, PointsOfThisPeriod2)
  tmyPtsRot <- rbind(tmyPtsRot, PointsOfThisPeriodall)
  num7 = dim(tmyPtsRot)[1]
  tmyPtsRot$Well.identifier <- c(1:num7)
  
  drwMapRot = list()
  drwMapRot[['baseWellsXY']] = tmyPtsRot
  drwMapRot[['channelWellsXY']] = tmyPtsRot[tmyPtsRot$isChannel == 20 | tmyPtsRot$isChannel == 10, ]
  drwMapRot[['non_channelWellsXY']] = tmyPtsRot[tmyPtsRot$isChannel == 0, ]
  
  tmagneticDatabase = magneticDatabase
  points(rbind(Northresult, Southresult)[, c(1, 2)], pch = 19)
}

# Post-processing for already completed but independent points

# Post-processing
contour(xo, yo, matrix(xygrid1$z, nrow = length(yo)), asp = 1)
points(myPtsRot$X, myPtsRot$Y, col = col.plot, pch = 19)
text(myPtsRot$X,myPtsRot$Y,
     labels = myPtsRot$Well.identifier,
     cex = 1)

for (i in 1:length(PointsOfThisPeriod)) {
  points(x = PointsOfThisPeriod[[i]]$x, 
         y = PointsOfThisPeriod[[i]]$y,
         col = 'black',
         cex = 0.25,
         lwd = 8)
}

for (i in 1:length(PointsOfThisPeriod)) {
  lines(x = PointsOfThisPeriod[[i]]$x, 
        y = PointsOfThisPeriod[[i]]$y,
        col = 'purple',
        cex = 0.5,
        lwd = 8)
}

points(myPtsRot$X, myPtsRot$Y, col = col.plot, pch = 19)

# Smoothing the lines
library(smoothr)
for (i in 1:length(PointsOfThisPeriod)) {
  line.now = as.matrix(PointsOfThisPeriod[[i]])
  line.smooth = smooth_ksmooth(line.now, smoothness = 2)
  lines(x = line.smooth[, 1], 
        y = line.smooth[, 2],
        col = 'orange',
        cex = 0.25,
        lwd = 19.8)
}
points(myPtsRot$X, myPtsRot$Y, col = col.plot, pch = 19)


