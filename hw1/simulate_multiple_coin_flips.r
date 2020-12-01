# Suppose Y is # of H's in 10 coin flips.
# We know P(Y = 5) = 10!/(5!)(5!)(2^10)
# but what if we did't know this?
# Estimate P(Y = 5) by simulating the experiment



n = 100            # number of times we repeat experiment
Y = rep(0,n)	   # will hold number of heads (out of 10 flips) here

for (i in 1:n) {
  Y[i] = sum(runif(10)<.5);  # number heads out of n flips
}

#print(Y)
cat("Est. prob = ",  sum(Y==5)/n, "\n");


truep = factorial(10)/(factorial(5)*factorial(5)*2^{10})
cat("True prob = ",truep, "\n");

