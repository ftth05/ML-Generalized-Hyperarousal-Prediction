# example data
set.seed(1)
DF <- data.frame(x=sample(c("Y","N"),100,T),y=sample(c("Y","N"),100,T))
DF

# how to get correlation
DF <- lapply(DF, as.integer)
DF
cor(DF)
#            x          y
# x  1.0000000 -0.0369479
# y -0.0369479  1.0000000

# visualize it
library(corrplot)
corrplot(cor(DF))


DF
