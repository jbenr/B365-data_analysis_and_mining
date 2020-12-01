# simulate the process of flipping a coin using runif.
# We know that P(H) = 1/2, but how to get an experiment that occurs half the time?

# if runif(1) < .5 we say H
# otherwise say T

M = 100  # number of times we replicate experiment
outcome = (runif(M) < .5)
#print(outcome)

# looking at the output it appears we get a H (TRUE) half the time, but let's count to see ...

estimated_prob = sum(outcome)/M
print(estimated_prob)
cat("error =", abs(estimated_prob-.5), "\n")
