# HW 6
# Ben Reichert

###
### 1
vocab <- read.csv("Vocab.csv")
n <- length(vocab[,4])
# a)
xy <- as.matrix(vocab[,4:5])
X <- vocab[,4]
Y <- vocab[,5]
plot(X,Y)

# b)
xbar = sum(X)/n
ybar = sum(Y)/n
xybar = sum(X*Y)/n
xsqbar = sum(X*X)/n
b = (ybar*xsqbar-xbar*xybar) / (xsqbar - xbar*xbar)
a = (ybar - b)/xbar
cat("α:",a,", and β:",b,"\n")

# c)
abline(b,a)
# It does appear that people with more education tend to have larger vocabularies, as demonstrated by the slope (α) being positive

# d)
cat("On average, an extra year of education improves the score on this particular vocabulary test by",a,"\n\n")

###
### 2
dat = read.csv("ais.csv",stringsAsFactors=FALSE, sep=",")

# a)
X = as.matrix(dat[,3:12])
Y = as.vector(dat[,2])

n = nrow(X);
for (d in 3:12) {
  X = as.matrix(dat[,d])
  xbar = sum(X)/n
  ybar = sum(Y)/n
  xybar = sum(X*Y)/n
  xsqbar = sum(X*X)/n
  b = (ybar*xsqbar-xbar*xybar) / (xsqbar - xbar*xbar)
  a = (ybar - b)/xbar
  cat("with", colnames(dat[d]), "as variables we get α of",a,", and β of",b,"\n")
}
cat("\n")

# b)
for (d in 3:12) {
  X = as.matrix(dat[,d]) 
  a = solve(t(X) %*% X, t(X) %*% Y)  
  yhat = X %*% a	 
  error = Y - yhat
  sse = sum(error*error)
  cat("with", colnames(dat[d]), "as variables we get average yhat of", ave(yhat)[1],"\n",
      "average error of",ave(error)[1],"\n",
      "and sse of",sse,"\n");
}
cat("\n")

# c)
ting <- 3:12;
for (d in 3:12) {
  X = as.matrix(dat[,ting[!ting %in% d]])
  a = solve(t(X) %*% X, t(X) %*% Y)
  yhat = X %*% a	 
  error = Y - yhat
  sse = sum(error*error)
  cat("with", colnames(dat[d]), "missing we get sse of",sse,"\n")
}
cat("\n")
# ommission of the hc variable results in greatest increase of SSE. Therefore, hc seems to be the most important in
# predicting rcc.

###
### 3
data(nottem)
y = nottem
n = length(y)
x = 1:n;

# a)
#for(i in 0:19) {
#  plot(x[(1+i*12):(12+i*12)],y[(1+i*12):(12+i*12)],type="b")
#}
plot(x,y,type="b")

# b)
Y <- as.matrix(y)

xbar = sum(x)/n;
ybar = sum(y)/n;
xybar = sum(x*y)/n
xsqbar = sum(x*x)/n

b = (ybar*xsqbar-xbar*xybar)/ (xsqbar - xbar*xbar)
a = (ybar - b)/xbar
abline(b,a)

# c)
Xc = cbind(rep(1,n),cos(pi*x/6),sin(pi*x/6))
ac = solve(t(Xc) %*% Xc, t(Xc) %*% Y)
yhat = Xc %*% ac
error = Y - yhat
sse = sum(error*error)
cat("a =",ac[2],", b =",ac[3],", c =",ac[1])
lines(x,yhat,col=2)

# d)
Xd = cbind(rep(1,n),cos(pi*x/6),sin(pi*x/6),x)
ad = solve(t(Xd) %*% Xd, t(Xd) %*% Y)
yhat = Xd %*% ad
error = Y - yhat
sse = sum(error*error)
plot(x,y,type="b")
abline(b,a)
lines(x,yhat,col=3)
# Yes, the results suggest that the company seems to be experiencing a growth in sales. 

###
### 4
data("AirPassengers")
y = AirPassengers
n = length(y)
x = 1:n;
Y <- as.matrix(y)

# a)
#for(i in 0:11) {
#  plot(x[(1+i*12):(12+i*12)],log(y[(1+i*12):(12+i*12)]),type="b")
#}
plot(x,log(y),type="b")
# Proposed model: trigonometric linear because the data is seasonal but increasing at a linear rate.

# b)
X = cbind(rep(1,n),cos(pi*x/6),sin(pi*x/6),x)
a = solve(t(X) %*% X, t(X) %*% log(Y))
yhat = X %*% a
lines(x,yhat,col=4)

# c)
plot(x,y,type="b")
X = cbind(rep(1,n),cos(pi*x/6),sin(pi*x/6),x)
a = solve(t(X) %*% X, t(X) %*% Y)
yhat = X %*% a
lines(x,yhat,col=5)
