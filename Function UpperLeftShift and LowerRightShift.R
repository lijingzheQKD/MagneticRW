encoding = 'UTF-8'
source_here = function(filename,nowdir = dirname(rstudioapi::getSourceEditorContext()$path)){
  fulldir = paste(nowdir,'/',filename,sep = '');  source(fulldir, encoding = encoding)}
source_here('Function zhongchuidian.R')
# this function is used to build upperleft and lowerright shifted curve of a curve.
# the input xs and ys are coordinates of the original curve.
# the output is the upperleft or lowerright curve points, matrix.
UpperLeftShift = function(xs,ys,dh=1){
  outputX = c()
  outputY = c()
  for (i in 2:length(xs)) {
    xA = xs[(i-1)]
    yA = ys[(i-1)]
    xB = xs[i]
    yB = ys[i]
    zcd = zhongchuidian(xA,yA,xB,yB,dh = dh)
    outputX = c(outputX,zcd[1,1])
    outputY = c(outputY,zcd[1,2])
  }
    output = cbind(outputX,outputY)
}
LowerRightShift = function(xs,ys,dh=1){
  outputX = c()
  outputY = c()
  for (i in 2:length(xs)) {
    xA = xs[(i-1)]
    yA = ys[(i-1)]
    xB = xs[i]
    yB = ys[i]
    zcd = zhongchuidian(xA,yA,xB,yB,dh = dh)
    outputX = c(outputX,zcd[2,1])
    outputY = c(outputY,zcd[2,2])
  }
  output = cbind(outputX,outputY)
}



















# example
if(0){
  xs = 1:99/30
  ys = sin(xs)
  plot(xs,ys,xlim = c(-5,5),ylim = c(-5,5),asp = 1)
  upperleftcurve = UpperLeftShift(xs,ys,dh=0.1)
  fanupperleftcurve = LowerRightShift(xs,ys,dh=0.1)
  points(upperleftcurve,col=6);points(fanupperleftcurve,col=6)
}