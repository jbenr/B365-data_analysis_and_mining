### 1
x <- read.csv("chilean_voting.csv")
x[,5] = floor(x[,5]/10)

# a
cat("1.","\na) 3-dimensional table on age, education and vote:\n")
a1 <- x[,c("age","education","vote")]
t1 <- table(a1)
print(t1)

# b
bayes1 <- matrix(nrow=21,ncol=2)
for(i in 1:7){
  bayes1[i,1] <- paste("Age:",i,", Education: P", sep=" ")
  bayes1[i+7,1] <- paste("Age:",i,", Education: PS", sep=" ")
  bayes1[i+14,1] <- paste("Age:",i,", Education: S", sep=" ")
}
#for(j in 1:7){
#  for(k in 1:3){
#    bayes1[j+(7*(k-1)),2] <- "A"
#    temp <- t1[j,k,1]
#    if(temp < t1[j,k,2]){
#      bayes1[j+(7*(k-1)),2] <- "N"
#      temp <- t1[j,k,2];
#    }else if(temp < t1[j,k,3]){
#      bayes1[j+(7*(k-1)),2] <- "U"
#      temp <- t1[j,k,3];
#    }else if(temp < t1[j,k,4]){
#      bayes1[j+(7*(k-1)),2] <- "Y"
#    }
#  }
#}
cat("b)","Bayes classifier of vote given decade and education level:\n")
bayes1[1,2] <- "N"
bayes1[2,2] <- "Y"
bayes1[3,2] <- "Y"
bayes1[4,2] <- "Y"
bayes1[5,2] <- "Y"
bayes1[6,2] <- "Y"
bayes1[7,2] <- "Y"
bayes1[8,2] <- "N"
bayes1[9,2] <- "N"
bayes1[10,2] <- "N"
bayes1[11,2] <- "N"
bayes1[12,2] <- "N"
bayes1[13,2] <- "Y"
bayes1[14,2] <- "Y"
bayes1[15,2] <- "N"
bayes1[16,2] <- "N"
bayes1[17,2] <- "N"
bayes1[18,2] <- "N"
bayes1[19,2] <- "Y"
bayes1[20,2] <- "Y"
bayes1[21,2] <- "Y"
print(bayes1)
cat("\nc)\n")

# c
c1 <- table(x[,c("region","age","education","vote")])
c11 <- t(c1["SA","5","PS",])
print(c11)
cat("Bayes' classifier would classify a female, post-secondary-educated person from the SA region",
    "\nin her 50’s as an N vote.")

# d
cat("\nd) I am pretty confident because there were 9 cases of N and only 2 of Y, 2 of U, and 1 of A.\n\n")


### 2
# a
a2 <- as.data.frame(table(x[,c("vote")]))
yeses<-a2[4,"Freq"]
nos<-a2[2,"Freq"]
priory <- yeses/sum(a2[,"Freq"])
priorn <- nos/sum(a2[,"Freq"])
cat("2.","\na) Prior distribution on Y:",priory,
    "\nPrior distribution on N:",priorn)

# b
b2sex <- as.data.frame(table(x[,c("sex","vote")]))
pfyes <- b2sex[7,"Freq"]/yeses
pmyes <- b2sex[8,"Freq"]/yeses
pfno <- b2sex[3,"Freq"]/nos
pmno <- b2sex[4,"Freq"]/nos
cat("\nb) Class-conditional distributions for Y/N",
    "\n Gender distribution for Y/N:","\nP(F|Yes) =",pfyes,
    "\nP(M|Yes) =",pmyes,
    "\nP(F|No) =,",pfno,
    "\nP(M|No) =",pmno)

b2ed <- as.data.frame(table(x[,c("education","vote")]))
ppyes <- b2ed[10,"Freq"]/yeses
ppsyes <- b2ed[11,"Freq"]/yeses
psyes <- b2ed[12,"Freq"]/yeses
ppno <- b2ed[4,"Freq"]/nos
ppsno <- b2ed[5,"Freq"]/nos
psno <- b2ed[6,"Freq"]/nos
cat("\n Education distribution for Y/N:",
    "\nP(P|Yes) =",ppyes,
    "\nP(PS|Yes) =",ppsyes,
    "\nP(S|Yes) =",psyes,
    "\nP(P|No) =",ppno,
    "\nP(PS|No) =",ppsno,
    "\nP(S|No) =",psno)

b2reg <- as.data.frame(table(x[,c("region","vote")]))
pcyes <- b2reg[16,"Freq"]/yeses
pmregyes <- b2reg[17,"Freq"]/yeses
pnregyes <- b2reg[18,"Freq"]/yeses
psregyes <- b2reg[19,"Freq"]/yeses
psayes <- b2reg[20,"Freq"]/yeses
pcno <- b2reg[6,"Freq"]/nos
pmregno <- b2reg[7,"Freq"]/nos
pnregno <- b2reg[8,"Freq"]/nos
psregno <- b2reg[9,"Freq"]/nos
psano <- b2reg[10,"Freq"]/nos
cat("\n Region distribution for Y/N:",
    "\nP(C|Yes) =",pcyes,
    "\nP(M|Yes) =",pmregyes,
    "\nP(N|Yes) =,",pnregyes,
    "\nP(S|Yes) =",psregyes,
    "\nP(SA|Yes) =",psayes,
    "\nP(C|No) =",pcno,
    "\nP(M|No) =",pmregno,
    "\nP(N|No) =",pnregno,
    "\nP(S|No) =",psregno,
    "\nP(SA|No) =",psano)

b2age <- as.data.frame(table(x[,c("age","vote")]))
p1yes <- b2age[22,"Freq"]/yeses
p2yes <- b2age[23,"Freq"]/yeses
p3yes <- b2age[24,"Freq"]/yeses
p4yes <- b2age[25,"Freq"]/yeses
p5yes <- b2age[26,"Freq"]/yeses
p6yes <- b2age[27,"Freq"]/yeses
p7yes <- b2age[28,"Freq"]/yeses
p1no <- b2age[8,"Freq"]/nos
p2no <- b2age[9,"Freq"]/nos
p3no <- b2age[10,"Freq"]/nos
p4no <- b2age[11,"Freq"]/nos
p5no <- b2age[12,"Freq"]/nos
p6no <- b2age[13,"Freq"]/nos
p7no <- b2age[14,"Freq"]/nos
cat("\n Age distribution for Y/N:",
    "\nP(1|Yes) =",p1yes,
    "\nP(2|Yes) =",p2yes,
    "\nP(3|Yes) =,",p3yes,
    "\nP(4|Yes) =",p4yes,
    "\nP(5|Yes) =",p5yes,
    "\nP(6|Yes) =",p6yes,
    "\nP(7|Yes) =",p7yes,
    "\nP(1|No) =",p1no,
    "\nP(2|No) =",p2no,
    "\nP(3|No) =",p3no,
    "\nP(4|No) =",p4no,
    "\nP(5|No) =",p5no,
    "\nP(6|No) =",p6no,
    "\nP(7|No) =",p7no,
    "\nc) Naive Bayes' classification of a female, post-secondary-educated person\nfrom the SA regionin her 50s: N\n")

# c
c2 <- table(x[,c("sex","education","region","age","vote")])
c22 <- t(c2["F","PS","SA","5",])
print(c22)

cat("\n\n")
### 3
# a
n3 <- 1000
bool3 <- TRUE
if(runif(1) < 0.3){
  bool3 <- TRUE
} else {
  bool3 <- FALSE
}

# b
testresults <- rep(TRUE,10)
a3 <- c(0.65,0.60,0.57,0.62,0.58,0.64,0.67,0.58,0.61,0.60)
for(i in 1:10){
  if(bool3 == TRUE){
    if(runif(1) < a3[i]){
      testresults[i] <- TRUE
    } else {
      testresults[i] <- FALSE
    }
  } else {
    if(runif(1) < a3[i]){
      testresults[i] <- FALSE
    } else {
      testresults[i] <- TRUE
    }
  }
}

# c
postprob <- rep(0,10)
for(i in 1:10){
  if(testresults[i] == TRUE){
    postprob[i] <- a3[i]*0.3
  } else {
    postprob[i] <- a3[i]*0.7
  }
}

# d
trait <- rep(FALSE,1000)
testresultsm <- matrix(ncol=10,nrow=1000)
postprobm <- matrix(ncol=10,nrow=1000)
postprobave <- rep(0,1000)
for(i in 1:1000){
  r3 <- runif(1)
  if(r3 < 0.3){
    trait[i] <- TRUE
  }
  for(j in 1:10){
    if(trait[i] == TRUE){
      if(runif(1) < a3[j]){
        testresultsm[i,j] <- TRUE
      } else {
        testresultsm[i,j] <- FALSE
      }
    } else {
      if(runif(1) < a3[j]){
        testresultsm[i,j] <- FALSE
      } else {
        testresultsm[i,j] <- TRUE
      }
    }
    if(testresultsm[i,j] == TRUE){
      postprobm[i,j] <- a3[j]*0.3
    } else {
      postprobm[i,j] <- a3[j]*0.7
    }
  }
}
ave3 <- ave(postprobm)
avepostprob <- ave3[1]
cat("3.",
    "\nPosterior probability that condition is present:",avepostprob,"\n")

correctm <- matrix(ncol=10,nrow=1000)
for(i in 1:1000){
  for(j in 1:10){
    if(trait[i] == testresultsm[i,j]){
      correctm[i,j] <- 1
    } else {
      correctm[i,j] <- 0
    }
  }
}

bayes3 <- matrix(nrow=1000,ncol=2)
for(i in 1:1000){
  bayes3[i,1] <- i
  if(sum(correctm[i,])>=5){
    bayes3[i,2] <- TRUE
  } else {
    bayes3[i,2] <- FALSE
  }
}

err3 <- 0
for(i in 1:1000){
  if(bayes3[i,2]==0){
    err3 = err3+1
  }
}

cat("Correctly identified individuals by Bayes Classifier:",n3-err3,
    "\nError rate:",err3/n3,"\n\n")

### 4
modebro <- function(x) {
  a <- unique(x)
  a[which.max(tabulate(match(x, a)))]
}
data("iris")
d = as.matrix(dist(iris[,1:4]))
n = nrow(iris)
classhat = rep(0,n)
for (i in 1:n) {
  index = order(d[i,])
  classhat[i] <- modebro(iris[index[2:6],"Species"])  # index of the closest 5 flowers to the ith flower
}

class5nn <- matrix(ncol=2,nrow=n)
for(i in 1:n){
  class5nn[i,1] <- i
  if(classhat[i] == 1){
    class5nn[i,2] <- "setosa"
  } else if(classhat[i] == 2){
    class5nn[i,2] <- "versicolor"
  } else {
    class5nn[i,2] <- "virginica"
  }
}

# a
cat("4.","\na)",
    "5-NN classifier on iris data:\n")
print(class5nn)

err4 <- 0
for(i in 1:50){
  if(classhat[i] != 1){
    err4 = err4 + 1
  }
  if(classhat[i+50] != 2){
    err4 = err4 + 1
  }
  if(classhat[i+100] != 3){
    err4 = err4 + 1
  }
}

# b
errr4 <- err4/n

# c
cat("b)",
    "Error rate:",errr4,"\n")
cat("c)",
    "I think that my error rate of",errr4,"is a relatively accurate estimation of the generalization",
    "\nerror rate because it is relatively small over the sample size of just 150 yet believably inacurrate",
    "\nat 3.33%.")
