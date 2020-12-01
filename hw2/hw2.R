cat("Homework 2","\n")
cat("Ben Reichert\n\n")

### 1 ###
pop1 = 100020
a1 = "P(Diabetes|Kale) of all Massachusetts adults cannot be computed because we do not have
data for the entire population of MA, we only have our small sample population of 100,020. 
We can make assumptions about the population that we gathered data on, but not all of MA."
pDK = 801/pop1
pK = (9192+801)/pop1
phatDK = pDK/pK
pDNK = 9905/pop1
pNK = (9905+80122)/pop1
phatDNK = pDNK/pNK
confDK = 1.96*sqrt(phatDK*(1-phatDK)/(9192+801))
confDNK = 1.96*sqrt(phatDNK*(1-phatDNK)/(9905+80122))
d1 = "I can conclude that the kale eaters are less likely to have diabetes because the
confidence interval of having diabetes and eating kale does not overlap with the confidence
interval of having diabetes and not eating kale. The projected proportions are roughly 3%
apart, leading me to conclude that kale eaters are statistically less likely to have diabetes."
e1 = "Based off of this study I do not think it is possible to draw the conclusion that eating
kale causes a lower diabetes rate. This is an observational study where kale consumption in the
participants is essentially hearsay. Kale is not necessarily the reason for lower diabetes rates."
f1 = "A more likely inference would be that people who eat kale also generally have a healthier
diet, resulting in a lower diabetes rate."
cat("1.\n","a.)",a1,"\n b.) Phat(Diabetes|Kale) =",phatDK,
    "\n c.) Confidence interval kale eaters: (",phatDK-confDK,",",phatDK+confDK,")\n",
    "Confidence interval non kale eaters: (",phatDNK-confDNK,",",phatDNK+confDNK,")","\n d.)",d1,"\n",
    "e.)",e1,"\n f.)",f1,"\n\n")

### 2 ###
a2 = "Given this scenario, I do not think that there is evidence that kale causes a lower
diabetes rate. People from different zip codes may have different diets. It is likely that
other factors are at play in determining diabetes rates from different zip codes."
b2 = "Given this scenario, I do not think there is evidence that kale is the cause for lower
diabetes rate. I think it would produce interesting results, but whether or not people consider
themselves health concious is a very vague and inaccurate way to pick participants. People 
could just blatantly lie and just say they consider themselves health concious to seem normal."
c2 = "I think this scenario would lead to sufficient evidence that kale is a cause of lower
diabetes rates. Randomly selecting the participants and groups would lead to a statistically
proper sample population for making inferences such as these."
cat("2.\n","a.)",a2,"\n b.)",b2,"\n c.)",c2,"\n\n")

### 3 ###
x3 <- runif(1000, 0, 1)
y3 <- runif(1000, 0, 1)
boolx3 <- 1:1000
booly3 <- 1:1000
for (i in 1:1000){
  if ((x3[i] + y3[i]) < 1) {
    boolx3[i] <- "T"
  } else {
    boolx3[i] <- "F"
  }
  if ((x3[i] - y3[i]) < 0) {
    booly3[i] <- "T"
  } else {
    booly3[i] <- "F"
  }
}
plot(x3, y3, main="Question 3", pch=1, col=ifelse(x3+y3<1 & x3-y3<0, "green",
                                            ifelse(x3+y3<1 & !(x3-y3<0), "blue",
                                                   ifelse(!(x3+y3<1) & x3-y3<0, "purple","red"))))
a3 = "According to the graph, the events A and B are independent. The event of A being True does
not have an impact on the probability of B being true."
a3w = 0
for(i in 1:1000){
  if(x3[i]+y3[i] < 1){
    a3w = a3w + 1
  }
}
phata3 = a3w/1000
conf3a = 1.96*sqrt(phata3*(1-phata3)/1000)

b3w = 0
ab3w = 0
for(i in 1:1000){
  if(x3[i]-y3[i] < 0){
    b3w = b3w + 1
  }
  if((x3[i]-y3[i] < 0) && (x3[i]+y3[i] < 1)){
    ab3w = ab3w + 1
  }
}
phatab3 = ab3w/b3w
conf3ab = 1.96*sqrt(phatab3*(1-phatab3)/b3w)
b3 = "Yes, these confidence intervals are consistent with A and B being independent. Given that
B is true, the probability of A is still around 50%.\n\n"

cat("3.\n","a.) ",a3,"\n b.) Confidence interval P(A): (",phata3-conf3a,",",phata3+conf3a,")\n",
    "Confidence interval P(A|B): (",phatab3-conf3ab,",",phatab3+conf3ab,")\n",b3)

### 4 ###
n4 = 1000000
a4 = 0
b4 = 0
ab4 = 0
for(i in 1:n4){
  x = runif(1)
  if(x<.5){
    a4 = a4 + 1
  }
  if(((2*x) %% 1) < .5){
    b4 = b4 + 1
  }
  if((((2*x) %% 1) < .5) && (x<.5)){
    ab4 = ab4+1
  }
}
phata4 = a4/n4
confa4 = 1.96*sqrt(phata4*(1-phata4)/n4)
phatb4 = b4/n4
confb4 = 1.96*sqrt(phatb4*(1-phatb4)/n4)
phatab4 = ab4/b4
confab4 = 1.96*sqrt(phatab4*(1-phatab4)/b4)

cat("4.\n","a.) Confidence interval P(A): (",phata4-confa4,",",phata4+confa4,")\n",
    "Confidence interval P(B): (",phatb4-confb4,",",phatb4+confb4,")\n",
    "Confidence interval P(A|B): (",phatab4-confab4,",",phatab4+confab4,")\n",
    "b.) A and B do appear to be independent events.\n\n")

### 5 ###
a5 = "These events should be modeled as dependent because I would assume people generally feel
similarly about government officials as a whole. If you feel good about the mayor of Bloomington
you feel good about the government as a whole and in turn also the police chief. The two events
seem correlated."
b5 = "These events should be modeled as independent because one random guy liking the mayer will
not affect what another random guy thinks about the mayor. These events do not seem correlated."
c5 = "These events are entirely dependent. If the coin lands on heads, it is definitely not tails."
d5 = "I think these events are dependent. Generally, if somebody likes a movie they are going to
also be a fan of the sequel, with the same being true about not liking a movie. Thus, the initial
condition of liking Incredibles 1 affects the likelihood of liking Incredibles 2."
cat("5.\n","a.) ",a5,"\n","b.) ",b5,"\n","c.) ",c5,"\n","d.) ",d5,"\n\n")

### 6 ###
a6 = "T and S are not independent because the event of a person having trait T drastically
increases their likelihood of possessing trait S."
b6 = (.9*.3)/((.9*.3)+.3)
n6 = 10000
vt6 = rep(0, n6)
vs6 = rep(0, n6)
for(i in 1:n6){
  if(runif(1,0,100) < 30) {
    vt6[i] <- 1
    if(runif(1,0,100) < 90) {
      vs6[i] <- 1
    }
  } else if (runif(1,0,70) < 30) {
    vs6[i] <- 1
  }
}
ts6 = 0
for(i in 1:n6) {
  if((vt6[i] + vs6[i]) == 2) {
    ts6 = ts6 + 1
  }
}
s6 = sum(vs6)
t6 = sum(vt6)
phatts6 = ts6/s6
confts6 = 1.96*sqrt(phatts6*(1-phatts6)/s6)

cat("6.\n","a.) ",a6,"\n","b.) P(T|S) =",b6,"\n","c.)\n",
    "d.) Confidence interval P(T|S): (",phatts6-confts6,",",phatts6+confts6,")\n")

