
# generate_bar_plots()



# get_coocur_test <- function(V){      # single vector as an input
#   VxV <- outer(V, V)                 # computes the outer product
#   VxV[lower.tri(VxV, diag = F)] <- 0 #  keep only upper diagonal matrix
#   Dp <- VxV/sum(VxV)             # normalizes it by the sum of elements
#   f_Dp <- sum(Dp[upper.tri(VxV, diag = F)]) # calculates the sum of the elements in the upper triangle of the resulting matrix
#   
#   # print(V)
#   # print(VxV)
#   # print(Dp)
#   # print(f_Dp)
#   
#   round(f_Dp, 4) # single value as output
# }



# get_coocur_test(c(0.7, 0.8, 0.9, 0.5, 0.3, 0.6, 0.2, 0.1))
# get_coocur_test(c(7, 8, 9, 5, 3, 6, 2, 1))
# get_coocur_test(c(700, 800, 900, 500, 300, 600, 200, 100))

# get_coocur_test(c(0, 0, 0, 0, 0, 2, 0, 0, 0))
# get_coocur_test(c(1, 1, 1, 1, 1, 2, 1, 1, 1))
# get_coocur_test(c(7, 7, 7, 7, 7, 2, 7, 7, 7))
# get_coocur_test(c(7, 8, 9, 5, 3, 6, 2, 1, 0))


# get_coocur_test(c(0.7, 0.8, 0.9, 0.5, 0.3, 0.6, 0.2, 0.1, 0.7, 0.8, 0.9, 0.5, 0.3, 0.6, 0.2, 0.1))
# get_coocur_test(c(0.7, 0.7, 0.7, 0.7))
# get_coocur_test(c(0.9, 0.9, 0.9, 0.9))
# get_coocur_test(c(0.7, 0.7, 0.7, 0.7, 0.7))
