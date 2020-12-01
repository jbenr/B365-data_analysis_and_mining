### HW 5
# Ben Reichert
library(rpart)

tree <- scan("tree_data.dat")
X = matrix(tree,byrow=T,ncol = 3)

traverse <- function(i) {
  print(i);
  if (X[i,3] == 0) {
    traverse(2*i)
    traverse(2*i+1)
    }
}

n = X[1,1] + X[1,2]
risk <- function(i) {
  if (X[i,3]) return(min(X[i,1],X[i,2])/n)
  return(risk(2*i)+risk(2*i+1))
}

## 1
# Attached picture


## 2


#a)
alpha <- 0.03
penrisk <- function(i) {
  if (X[i,3]) return(min(X[i,1],X[i,2])/n)
  return(penrisk(2*i)+penrisk(2*i+1)+alpha)
}

#b)
optrisk <- function(i) {
  if (X[i,3]) return(risk(i))
  return(min(min(X[2*i,1],X[2*i,2])/n,penrisk(2*i))+min(min(X[2*i+1,1],X[2*i+1,2])/n,penrisk(2*i+1)))
}
# subtract one from optrisk(1). Answer is 0.32, but optrisk(1) gives 0.33 because
# it adds the 0.01 from node #19.

#c)
optnodes <- function(i) {
  if (!X[i,3])
    if(((min(X[2*i,1],X[2*i,2])+min(X[2*i+1,1],X[2*i+1,2]))/n)+0.03 <= min(X[i,1],X[i,2]))
    print(i)
    return(optnodes(2*i))
    return(optnodes(2*i+1))
}

#d)
# Attached

#e)
# Attached


## 3

#a)
# The 0 in the 23rd row is the error rate associated with that tree outlined in the 23rd row.
# It is measuring the error rate on the training set.

#b)
# I believe that the tree on the 23rd row will perform relatively well on data not represented
# in the tree. The true error rate is 0.046602, the second best of the proposed complexity parameters.

#c)
# The tree that makes 5 splits has a relative error rate of 0.1611650, so it will perform reasonably
# well with new data from the same population. 

#d)
# 0.00064725 seems to be the most optimal complexity parameter because the xerror or true
# error rate (the generalization error) is the lowest (best) at this complexity parameter.
