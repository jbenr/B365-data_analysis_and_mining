# Ben Reichert
# B365 Final

## 1 ##
# a)
# P(no,no,no,no) = count["no","no","no","no"]/1000

# b)
# P(yes,yes,,) = count["yes","yes",,]/1000
# P(yes,no,,) = count["yes","no",,]/1000
# P(no,yes,,) = count["no","yes",,]/1000
# P(no,no,,) = count["no","no",,]/1000

# c)
# P(first yes | last two yes) = count["yes",,,]/count[,,"yes","yes"]
# P(first no | last two yes) = count["no",,,]/count[,,"yes","yes"]

# d)
# Yes, i would say that it is reasonable to assume both samples came from the same population. Answering no to all four
# questions, assuming each question has a 50/50 probability of being yes/no, answering no to four straight questions is
# a 1/16 chance. For 50% of the sample population to choose a 1/16 chance is pretty bizzare, and for 60% of another sample
# population to do the same thing is even more bizarre. So one of two things could be happening. 1) the questions could be
# incredibly "no" oriented, like "have you ever shot yourself in the eyeball". Oviously everyone would answer no to that
# question. But 2) if the questions are designed to be relatively evenly distributed between yes/no it is more likely
# that the two sample populations were drawn from the same population because people from the same population tend to be
# similarly minded. 


## 2 ##
xy <- read.csv("x_and_y.csv")
# a)
y = xy[,3]
n = length(y)
x = xy[,2]
Y <- as.matrix(y)
X = cbind(rep(1,n),x,x^2,x^3)
a = solve(t(X) %*% X, t(X) %*% Y)
yhat = X %*% a
# a = 2.0030348, b = -1.0277663, c = 0.5290213, d = 1.0562995

# b)
plot(x,y)
points(x,yhat,col="red")

# c)
error = Y - yhat
sse = sum(error*error)
# sse = 99.98896


## 3 ##
# a)
# I do not think that it is possible to solve this system of four equations. Typically, any system of three equations
# with three variables can be solved without an issue, but adding a fourth equation, which is like adding a fourth parameter
# throws the system off. There is no solution that satisfies all 4 equations. 
# After graphing them out, 

# b) 
#      X       a    y
# {4,  2,  2} {a1} {5}
# {5, -3, -4} {a2} {6}
# {7, -4, -2} {a3} {2}
# {2,  2, -2}      {10}

# c)
x <- matrix(c(4,5,7,2,-3,-4,2,-4,-2), ncol=3, nrow=3)
y <- matrix(c(5,6,2))
sol1 <- solve(x, y)

x <- matrix(c(4,5,2,2,-3,2,2,-4,-2), ncol=3, nrow=3)
y <- matrix(c(5,6,10))
sol2 <- solve(x, y)

x <- matrix(c(5,7,2,-3,-4,2,-4,-2,-2), ncol=3, nrow=3)
y <- matrix(c(6,2,10))
sol3 <- solve(x,y)

x <- matrix(c(4,7,2,2,-4,2,2,-2,-2), ncol=3, nrow=3)
y <- matrix(c(6,2,10))
sol4 <- solve(x,y)

a1 <- sum(sol1[1],sol2[1],sol3[1],sol4[1])/4
a2 <- sum(sol1[2],sol2[2],sol3[2],sol4[2])/4
a3 <- sum(sol1[3],sol2[3],sol3[3],sol4[3])/4
# a1 = 1.056911, a2 = 2.218279, a3 = -1.788999

# d)
# I think my a1, a2, and a3 are the best choice because it approximates the solution in the scope of the system.
# I took out one of the equations to make the system contain only 3, making it solvable, and solved for that. Then
# I did the same with the second equation, solving for a1, a2, and a3 for that system, and so on. So I solved for 4
# different systems of 3 equations comprised of the combinations of the 4 equations we were given and averaged the
# results. This gives the best approximate solution. 


## 4 ##
# a) b) c) png's attached.

# d) I would take the new collection and determine which α value, 0, 0,02, or 1 results in the lowest error. As
# it is with the collection above, an α of 1.0 will most likely result in the highest error, α of 0.02 the next highest,
# and α of 0.0 as the lowest. Depending on the error values with pruning in optimal locations (resulting in lowest errors),
# the new collection will probably give fairly similar results to the data given in the first part of this problem. As for
# finding optimal pruning locations, I would simply start from the bottom of the tree and determine whether the combined
# error of the terminal nodes plus the split penalty of α is higher than its parent node's error. If the parent node k's
# error is < [(k*2)'s error + (k*2 + 1)'s error +  α] then I would prune at that parent node and work this algorithm through
# the entirety of the tree. However, a α of 0.0 will consistently provide the lowest error rate because the tree's error
# rate would not be getting penalized at all on splits, so the tree with α = 0.0 will probably be the most optimal, as above. 


## 5 ##
# a)
# In choosing a variable using forward selection, we would choose the one that gives us the smallest SSE. 
# On scanning the graphs that have rcc as an input, it is quite clear that the hc variable produces graphs
# that are the most linear, which would produce the lowest SSE. 

# b)
# If the response variable (y) is rcc and the predictor variable (x) is hc, according to the graph, rise is 2
# and run is 20 so α would be 2/20 = 0.1.


## 6 ##
# a)
# This problem does not give enough info to calculate the naive bayes classifier. The bayes theorem is as follows:
# P(L|O) = P(L)*P(O|L) / P(L)*P(O|L) + P(!L)*P(O|!L) for P(life given oxygen content of planet) and
# P(L|T) = P(L)*P(T|L) / P(L)*P(T|L) + P(!L)*P(T|!L) for P(life given temperature of planet). 
# We have enough of these inputs to use Bayes' theorem to calculate each of the above calculations, but do not have
# actual data. We need the data of whether or not there is life on a selection of planets given their
# environmental conditions to determine the bayes' classifiers of those conditions. If we had the data, 
# we could easily count the number of planets that have life on them for each of the conditions and arrive
# at definitive classifiers for each condition. 

# b)
p.l <- 0.01
p.nl <- 1-p.l

p.ol <- 0.9
p.nol <- 1-p.ol

p.onl <- 0.01
p.nonl <- 1-p.onl

p.tl <- 0.95
p.ntl <- 1-p.tl

p.tnl <- 0.3
p.ntnl <- 1-p.tnl

p.lo <- p.l*p.ol / ((p.l*p.ol) + (p.nl*p.onl))
# P(L|O) = 0.4761905
p.lt <- p.l*p.tl / ((p.l*p.tl) + (p.nl*p.tnl))
# P(L|T) = 0.03099511
p.lot <- p.lo+p.lt
# So P(L|O&T) = 0.5071856 which is > 0.5 so this would classify as a planet with life.

p.lno <- p.l*p.nol / ((p.l*p.nol) + (p.nl*p.nonl))
# P(L|!O) = 0.001019264
p.lnot <- p.lno+p.lt
# So P(L|!O&T) = 0.03201437 which is < 0.5 so this would classify as a planet without life. 

p.lnt <- p.l*p.ntl / ((p.l*p.ntl) + (p.nl*p.ntnl))
# P(L|!T) = 0.0007209805
p.lont <- p.lo+p.lnt
# So P(L|O&T) = 0.4769115 which is < 0.5 so this would classify as a planet without life. 

p.lnont <- p.lno+p.lnt
# So P(L|!O&!T) = 0.001740245 which is < 0.5 so this would classify as a planet without life.

