# 和是1的概率值变换
# The input is a vector
# The output is also a vector
sum1 = function(vec){
  return(vec/sum(vec))
}
#example
if(0){
  vec = 1:5
  output = sum1(vec)
  print(output)
}