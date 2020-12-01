cat("Ben Reichert\n")
cat("B365 - Homework 3\n\n")

# 1
pa1 = .2
pb1 = .3
pc1 = .5
paf1 = .3
pbf1 = .5
pcf1 = .9
nf1 = (1-paf1)*pa1 + (1-pbf1)*pb1 + (1-pcf1)*pc1

a1 = "P(A|not favor) = P(A)*(1-P(favor|A)) / (1 - (P(favor|A) + P(favor|B) + P(favor|C)))"
b1 = pa1*(1-paf1)/nf1
c1 = pb1*(1-pbf1)/nf1
d1 = pc1*(1-pcf1)/nf1

n1=10000
party1 = rep("",n1)
favor1 = rep(0,n1)
for(i in 1:n1){
  temp = runif(1,0,100)
  if(temp <= 20) {
    party1[i] <- "A"
    if(runif(1,0,100) <= 30) {favor1[i] <- 1}
  } else if(temp <= 50 && temp > 20){
    party1[i] <- "B"
    if(runif(1,0,100) <= 50) {favor1[i] <- 1}
  } else {
    party1[i] <- "C"
    if(runif(1,0,100) <= 90) {favor1[i] <- 1}
  }
}
favora1 = 0
counta1 = 0
for(i in 1:n1){
  if(party1[i] == "A"){
    counta1 = counta1 + 1
    favora1 = favora1 + favor1[i]
  }
}
e1 = favora1/counta1

cat("1.\n",
    "a)",a1,"\n",
    "b)",b1,"\n",
    "c)",c1,"\n",
    "d)",d1,"\n",
    "e)",e1,"\n",
    "f) The probabilities computed above absolutely apply to this scenario. This scenario is what\n",
    "those probabilities are meant for. They are the probabilities that an individual who doesn't\n",
    "favor the measure is in either party A, B, or C. So for this individual, those probabilities apply.","\n\n")

# 2
cat("2.\n","a)")

data("UCBAdmissions"); # import the data
ucb = UCBAdmissions; # abbreviate "UCBAdmissions"
print(apply(ucb,c("Dept","Admit"),sum));  # 2-way table of Department x Admit
mosaicplot(apply(ucb,c("Dept","Admit"),sum));  # mosaic plot clearer ...
cat("Department and admit status do not seem independent. Different departments have drastically different \nadmit status percentages, thus department affects the chance of admittance. Not independent.",
    "\n","b)")

print(apply(ucb,c("Dept","Gender"),sum));  # 2-way table of Department x Gender
mosaicplot(apply(ucb,c("Dept","Gender"),sum));  # mosaic plot clearer ...
cat("Department and gender do not seem independent. Different departments have much different gender \npercentages, thus department affects likelihood of gender. Not independent.",
    "\n","c)")

print(t(ucb[,,"F"]));
mosaicplot(t(ucb[,,"F"]));
cat("For the applicants to department F, gender and admit status do seem to be independent. Regardless \nof gender, the applicants to department F seem to have a universal admittance rate.",
    "\n","d) Rejected\n")

print(t(apply(ucb,c("Gender","Admit"),sum))["Rejected",])
cat("\n")

# 3
cat("3.","\n","a) Graphs attached.")

data(iris)
n = nrow(iris)
type = rep(0,n)
type[iris[,5] == "setosa"] = "s"
type[iris[,5] == "versicolor"] = "c"
type[iris[,5] == "virginica"] = "v"
#pairs(iris[,1:4],pch=type,cex=2)
cat("\n","b) I think that Petal.Width would be optimal as a bayes classifier for identifying iris type\n",
    "because it seems to differentiate between values most effectively. As a bayes classifier is\n",
    "supposed to be the most consistent data predictor, the Petal.Width characteristic, by the pair\n",
    "graphs, seems to offer the biggest distinction between the 4 attributes.","\n\n")

# 4
cat("4.")

da <- c(1,2,3,4,5,6)
db <- c(1,1,2,3,3,4,5,5,6)
dc <- c(1,2,3,4,4,5,5,6,6)
n4 = 1000
m4 = matrix(0, 3, n4)
for(i in 1:n4){
  temp = sample(0:100, 1)
  for(j in 1:3) {
    if(temp <= 50) {
      m4[j,i] <- sample(da, 1, replace=T)
      m4[j,i] <- sample(da, 1, replace=T)
      m4[j,i] <- sample(da, 1, replace=T)
    } else if(temp > 50 && temp <= 75) {
      m4[j,i] <- sample(db, 1, replace=T)
      m4[j,i] <- sample(db, 1, replace=T)
      m4[j,i] <- sample(db, 1, replace=T)
    } else {
      m4[j,i] <- sample(dc, 1, replace=T)
      m4[j,i] <- sample(dc, 1, replace=T)
      m4[j,i] <- sample(dc, 1, replace=T)
    }
  }
}

ba4 = (1/6)^3
bb4 = (1/9)*(1/9)*(2/9)
bc4 = (1/9)*(2/9)*(2/9)
sumb4 = ba4+bb4+bc4
probx = (ba4*0.5) + (bb4*0.25) + (bc4*0.25)

cat("\n","a) Experiment in code.","\n",
    "b) If x1=2, x2=4, and x3=5,\n",
    "P(A|x1, x2, x3) =",0.5*ba4/probx,"\n",
    "P(B|x1, x2, x3) =",0.25*bb4/probx,"\n",
    "P(C|x1, x2, x3) =",0.25*bc4/probx)

ca4 = "(P(A) * P(x1, x2, x3|A)) / P(x1, x2, x3)"
ca44 = (0.5 * ba4) / probx
cb4 = "(P(B) * P(x1, x2, x3|B)) / P(x1, x2, x3)"
cb44 = (0.25 * bb4) / probx
cc4 = "(P(C) * P(x1, x2, x3|C)) / P(x1, x2, x3)"
cc44 = (0.25 * bc4) / probx

cat("\n","c)",
    "Bayes Classifiers if x1=2, x2=4, and x3=5,\n",
    "P(A|x1, x2, x3) =",ca4,"=",ca44,"\n",
    "P(B|x1, x2, x3) =",cb4,"=",cb44,"\n",
    "P(C|x1, x2, x3) =",cc4,"=",cc44)
d4actual <- matrix("",3,n4)
d4bayes <- matrix("",3,n4)
for(i in 1:n4) {
  for(j in 1:3) {
    temp = runif(1,0,1)
    if(temp < (0.5*ba4/probx)) {
      d4actual[j,i] <- "A"
    } else if(temp >= (0.5*ba4/probx) && temp < ((0.5*ba4)+(0.25*bb4)/probx)) {
      d4actual[j,i] <- "B"
    } else {
      d4actual[j,i] <- "C"
    }
    if(temp < (ca44)) {
      d4bayes[j,i] <- "A"
    } else if(temp >= (ca44) && temp < (ca44+cb44)) {
      d4bayes[j,i] <- "B"
    } else {
      d4bayes[j,i] <- "C"
    }
  }
}
error4 = 0
for(i in 1:n4) {
  if(d4actual[i] != d4bayes[i]){
    error4 = error4 +1
  }
}
cat("\n","d) Proportion of true result =",(1-error4/n4),"\n\n")

# 5
X <- read.csv("three_related_vars.csv")
a5 <- X$A
b5 <- X$B
c5 <- X$C
mosaic_ab <- table(a5,b5)
mosaic_bc <- table(b5,c5)
mosaic_ac <- table(a5,c5)
mosaic_abc <- table(a5,b5,c5)
mosaicplot(mosaic_ab)
mosaicplot(mosaic_bc)
mosaicplot(mosaic_ac)
mosaicplot(mosaic_abc)

cat("5.","\n","a) According to the mosaics, when C is a 1, A and B tend to also be 1. Thus, the pairs A&C and B&C\n",
    "do not seem independent. The pair A&B seems slightly more independent, but I see too much of a\n",
    "correlation between A&B being the same... so I don't think any are independent of one another.\n",
    "b) According to the mosaics, it seems that when A is 0, B is relatively random, thus hinting at\n",
    "a conditional independence between A&B when A is 0. I see the same trend when B is 0 with B&C, but\n",
    "am slightly less confident about that conditional independence, and am yet less confident with the\n",
    "conditional independence of A&C when A is 0. Although they show signs of conditional independence,\n",
    "I do not think there is enough evidence to say that is the case for any of them.\n",
    "c) ")
a1sum = 0
a1b1= 0
a1c1 = 0
for(i in 1:10000){
  if(a5[i] == 1) {
    a1sum = a1sum + 1
    if(b5[i] == 1){
      a1b1 = a1b1+1
    }
    if(c5[i] == 1){
      a1c1 = a1c1+1
    }
  }
}
cat("P(B1 | A1) =",a1b1/a1sum,"\n",
    "P(C1 | A1) =",a1c1/a1sum,"\n",
    "As we can clearly see, the probability of B being 1 given A is 1 and C being 1 given A is 1\n",
    "is heavily skewed. Therefore, A&B and A&C are not independent.\n",
    "P(")


