# Demonstration of Simpson's paradox on the famous UCB graduate admissions data.

data("UCBAdmissions"); # import the data
ucb = UCBAdmissions;   # abbreviate "UCBAdmissions"
print(dimnames(ucb));
print(apply(ucb,c("Gender","Admit"),sum));  # 2-way table of Gender x Admit
mosaicplot(apply(ucb,c("Gender","Admit"),sum));  # mosaic plot clearer ...


# table shows clear gender bias against Female students






























# ... or does it ...?  



mosaicplot(t(ucb[,,"A"]));  # department A seems to *favor* Females
mosaicplot(t(ucb[,,"B"]));  # B shows slight bias in favor of Females but nearly  neutral  
mosaicplot(t(ucb[,,"C"]));  # so are others ...
mosaicplot(t(ucb[,,"D"]));  
mosaicplot(t(ucb[,,"E"]));  
mosaicplot(t(ucb[,,"F"]));  

# what is going on !?!?
# Every department either favors Females or is neutral, though, put together, women are admitted *less* often

mosaicplot(apply(ucb,c("Dept","Admit"),sum));  # depts accept rates differ

# to facilitate understanding departments are ordered A ... F from easier to harder

mosaicplot(apply(ucb,c("Dept","Gender"),sum));  # men apply more to easier depts A and B

# moral:  summing out (marginalizing) over variables can give misleading results




