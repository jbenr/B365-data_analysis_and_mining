cat("B365 Homework 1\n")
cat("Ben Reichert\n\n")

### 1
n = 10000
wins = rep(0,n)
a=rep(0,n)
b=rep(0,n)
c=rep(0,n)
for(i in 1:n){
  repeat{
    a <- runif(1)>.5
    b <- runif(1)>.5
    c <- runif(1)>.5
    if(!(a==b & b==c)){break}
  }
  if (a==b & c!=a) {
    wins[i] <- "c"
  } else if (b==c & a!=b) {
    wins[i] <- "a"
  } else if (a==c & b!=c) {
    wins[i] <- "b"
  }
}
sum = 0;
for(j in 1:n){
  index <- wins[j]
  if(index == "a"){
    sum = sum+1
  }
}
n1b = 2500
n1bool = 0.02 == 1/sqrt(n1b)

cat("1.\n a.) P(A) =", sum/n,"\n")
cat(" b.)", n1b,"trials necessary for 1/sqrt(n) == 0.02 to be",n1bool,"\n")
cat(" c.) We can assume that the confidence interval containing P(A wins) is reliable 95% of the time. \n")
cat(" d.) True Probability = ",1/3, "because there is a 25% percent chance of \n",
    "each outcome, including rerolling. But the experiment specifies that we reroll \n",
    "if they all are the same. So we take the 25% chance of reroll out and are left \n",
    "with 25% / 75% for A winning. So the true prob is 25/75, or 1/3.\n\n")

### 2
n2=40000
deck=(1:52)
p1=rep(0,n2);
p2=rep(0,n2);
winner = ""
for(j in 1:n2){
  deck=(1:52)
for(i in 1:40){
  draw = sample(deck, 1, replace = FALSE, prob = NULL)
  if(draw <= 28){
    if( i%%2 != 0 ){ p1[j]=1; }
    else { p2[j]=1; } 
    break }
  else { deck = deck[deck != draw] }
}}
n2a = sum(p1) / n2
n2an = 1 / sqrt(n2)

cat("2.\n", "a.) (",n2a-n2an,",", n2a+n2an, ")\n b.)","True probability =",round(n2a,digits=2),
    "because this is the amount of times P1 wins over n,\n which is the number of times experiment is tested.","\n\n")

### 3
cat("3.\n", "a.) Sample space = 52 cards \n     |Omega| or # of elements in Omega =", 52*51/2,
    "\n", "b.) Elements of Omega where both cards are same rank =", 52*3/2,"\n",
    "c.) P(Drawing a pair) =", 3/51, "= 5.88%","\n\n")

### 4
n4=300
w4=rep(0,300)
for(i in 1:n4){
  num4 = sample(1:10, 1, replace=TRUE)
  if(num4 == 1){ w4[i] = 1 }
}
phat4 = sum(w4) / n4
conf = 1.96 * sqrt(phat4*(1-phat4)/n4)

pf4 = rep(0,n4)
phf4 = rep(0,n4)
conf4 = rep(0,n4)
for(i in 1:n4){
  if( sample(1:10,1,replace=TRUE) == 1) { 
    pf4[i] = 1
    phf4[i] = sum(pf4) / i
    conf4[i] = 1.96 * sqrt(phf4[i]*(1-phf4[i])/i)
  } else {
    phf4[i] = sum(pf4) / i
    conf4[i] = 1.96 * sqrt(phf4[i]*(1-phf4[i])/i)
  }
}
hi4 = phf4 + conf4
lo4 = phf4 - conf4
index = 1:n4

phat4c = rep(0,1000)
conf4c = rep(0,1000)
contru = rep(0,1000)
for(j in 1:1000){
  w4c=rep(0,300)
  for(i in 1:300){
    if(sample(1:10, 1, replace=TRUE) == 1){ 
      w4c[i] = 1
    }
  }
  phat4c[j] = sum(w4c) / 300
  conf4c[j] = 1.96 * sqrt(phat4c[j]*(1-phat4c[j])/300)
}
for(i in 1:1000) {
  if(phat4c[i]+conf4c[i] > 0.1 && phat4c[i]-conf4c[i] < 0.1){ contru[i] = 1 }
}
prob4c = sum(contru) / 1000

cat("4.\n", "a.) Confidence interval: (",phat4-conf,",",phat4+conf,")",
    "\n b.) Sample plot attached.",plot(index, hi4,main="Confidence Interval over 300 Points",col="blue",pch="+", ylim=c(0,1)),
             points(index, lo4, col="red", pch="o"),"\n c.)",prob4c,"\n\n")
### 5
cat("5.\n I think that the exact probability of A's number being larger than B's number is 1/2.\n",
    "If you think about it case by case... the first case we have is two numbers, let's say\n",
    "1 and 2 in the bag. There's obviously a 50% probability that person A chooses 2, and\n",
    "therefore winning the game. Moving on to three numbers, we have 1, 2 and 3 in the bag.\n",
    "Here, we have a 1/3 chance of picking each number, but picking 2 gives us another 50/50\n",
    "scenario. So we can really break this down by only looking at the chances of picking,\n",
    "1 or 3 which would also be a 50/50 scenario. Moving on to four numbers in the bag, a clear\n",
    "pattern begins to reveal itself. Every scenario has a 50/50 win rate for each player,\n",
    "no matter who picks first.","\n\n")

### 6
f=function(t){
  sample(c(1,2,3,4,5,6),size=t,replace=TRUE,prob=c(.1,.2,.3,.35,.02,0.03))
}
cat("6.\n Number from 1-6:", f(1),"\n\n")

### 7
cat("7.\n a.) P(Both questions No) =", 0.4, "\n")
cat(" b.) P(Either question Yes) =", 0.2+0.1+0.3, "\n")
cat(" c.) P(1 question Yes) =", 0.3/0.4, "\n")
cat(" d.) The two are not mutually exclusive because given the fact that Question 1 \n",
    "is Yes, Question 2 has a", 0.3/0.5, "probability of being Yes and a", 0.2/0.5, "probability of\n",
    "being No. Yet, given that Question 1 being No, the probability of Question 2\n",
    "has a", 0.1/0.5, "probability of being Yes and a", 0.4/0.5, "probability of being No.\n")
