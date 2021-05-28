# This script computes the optimal portfolio allocation using four different
# combinations (2 distribution assumptions and 2 optimization goals)
# 
# distribution assumptions:
# 1. returns on different dates are I.I.D.
# 2. each day's return depend linearly on previous returns (auto-regression)
#
# optimization goals:
# 1. minimize variance of the portfolio (target return = 0.001)
# 2. maximize the probability of getting beyond-acceptable return (>=0.001)
# note that 0.001 is pretty high for daily return: 1.001^253 = 1.28772...
# (if our portfolio grows by 0.001 per trading day, it will have grown by 0.29
# per year, given that there are on average 253 trading days in a year)
# ("0.001" also happens to be less than A_2/C_2 - see page 7 of paper)
# 
# Note that:
# * we allow short selling
# * the last day's returns (5/21/2021) are used to test the portfolio's 
#   performance and thus not used in training (we simulate setting up the
#   portfolio on 5/20/2021 and evaluating it on 5/21/2021)
#
# disclaimer: the  formula used in this script heavily reference the paper
# "Portfolio risk management via interval probability" by Eugene Demidenko
#
# code author: Shengsong Gao
# latest update: 5/27/2021

# read in the data (it contains the adjusted prices for all SP500 stocks
# from 5/17/2016 to 5/21/2021 but we only consider 5 of the stocks)
# (note that the last trading day in the date range fetched is 5/21/2021)
stock_picks = c("AAPL", "TSLA", "GE", "C", "CCL")
price_data = read.csv("./stocks_adjusted_SP500_2016-05-17_2021-05-23.csv", row.names=1)[, stock_picks]

# start by setting up the training data and the evaluation data: we compute 
# the simple returns from the stock data.
# note that we cannot use log returns because the linear combination
# given the weights only makes sense with the simple returns

num_prices = nrow(price_data)
return_data = (price_data[2:num_prices,]-price_data[1:(num_prices-1),])/
              price_data[1:(num_prices-1),]
num_training_data = nrow(return_data) - 1 # last row excluded from training
training_data = return_data[1:num_training_data,]
# use as.numeric() to convert the dataframe row into a vector.
# note that  as.vector() does NOT work (it does nothing for a dataframe row)
evaluation_data = as.numeric(return_data[num_training_data+1,])

# compute the mean vector and covariance matrix for distribution assumption 1
# note that we exclude the last return (it's used for evaluation
mean_1 = colMeans(training_data)
cov_1 = cov(training_data)

# compute the mean vector and covariance matrix for distribution assumption 2

# we use auto-regression with default maxlag=10 for each stock.
# the return prediction will be the "mean" (expected return) and the
# residual vectors will be used to compute the covariance matrix
# auto_regression() takes in a stock's returns and returns the linear model
auto_regression = function(return_vector, maxlag) {
  prediction_data = return_vector[(maxlag+1):num_training_data]
  AR_matrix = matrix(nrow=(num_training_data-maxlag), ncol=maxlag)
  # we fill out the auto-regression matrix column-wise.
  for(j in 1:maxlag) 
    AR_matrix[, j] = return_vector[(maxlag-j+1):(num_training_data-j)]
  lm_output = lm(prediction_data~AR_matrix)
  return(lm_output)
}

# set up placeholders we will fill in after calling auto_regression()
maxlag = 10
num_stocks = length(stock_picks)
mean_2 = rep(0, num_stocks)
residual_matrix = matrix(nrow=(num_training_data-maxlag), ncol=num_stocks)

# we assume that mean_1&mean_2 and cov_1&cov_2 are populated in the same order
# as the vector `stock_picks` (the uncertainty is around `mean_1` and `cov_1`)
for (i in 1:num_stocks) {
  return_vector = training_data[, stock_picks[i]]
  lm_output = auto_regression(return_vector, maxlag)
  predictors = return_vector[(num_training_data-maxlag+1):num_training_data]
  lm_coefficients = lm_output$coefficients
  # note that the first coefficient is the intercept
  prediction = lm_coefficients[1]
  for (j in 1:maxlag) {
   prediction = prediction + lm_coefficients[j+1] * predictors[j] 
  }
  mean_2[i] = prediction
  residual_matrix[, i] = lm_output$residuals
}

cov_2 = cov(residual_matrix)

# now, onto the optimal portfolio allocation strategies
# =======================================================================
# note that the variables are as they appear in the paper
# (`A`, `B`, `C`, `D`, `a` are defined on page 3; the subscripts denote
# the distribution assumption they follow)
ones = rep(1, num_stocks)
r = 0.001

# note that we must use `drop()` to explicitly convert 1X1 matrix into a scalar
# for various reason:
# 1. to avoid having to use matrix product in reverse order compared with
#    conventional mathematical denotation
# 2. treating 1X1 matrix as a scalar implicitly makes R complain (for example,
#    if we try to multiply it with another vector using regular product).
cov_1_inv = solve(cov_1)
A_1 = drop(t(mean_1) %*% cov_1_inv %*% ones)
B_1 = drop(t(mean_1) %*% cov_1_inv %*% mean_1)
C_1 = drop(ones %*% cov_1_inv %*% ones)
D_1 = B_1 * C_1 - A_1 ^ 2

cov_2_inv = solve(cov_2)
A_2 = drop(t(mean_2) %*% cov_2_inv %*% ones)
B_2 = drop(t(mean_2) %*% cov_2_inv %*% mean_2)
C_2 = drop(ones %*% cov_2_inv %*% ones)
D_2 = B_2 * C_2 - A_2 ^ 2

# the optimal weights are calculated for each combination is calculated below
# note how `p` for goal 2 is the same as `a` for goal 1
# having done the algebra, `a` for goal 2 has formula `1/(A-r*C)`
# (whereas for goal 1, `a` has formula `(r*C-A)/D`. Why both formula
# fit into the general optimal weight formula need further investigation

# combination 1-1
# ==========================================
a_1_1 = (r*C_1 - A_1) / D_1
optimal_weights_1_1 = cov_1_inv %*% (a_1_1 * mean_1 + 
                      ((1 - a_1_1 * A_1) / C_1) * ones)
print(optimal_weights_1_1)
print(t(optimal_weights_1_1) %*% evaluation_data)

# combination 1-2
# ==========================================
Q_1 = 1 / D_1
p_1 = a_1_1
a_1_2 = -Q_1 / p_1
optimal_weights_1_2 = cov_1_inv %*% (a_1_2 * mean_1 + 
                      ((1 - a_1_2 * A_1) / C_1) * ones)
print(optimal_weights_1_2)
print(t(optimal_weights_1_2) %*% evaluation_data)

# combination 2-1
# ==========================================
a_2_1 = (r*C_2 - A_2) / D_2
optimal_weights_2_1 = cov_2_inv %*% (a_2_1 * mean_2 + 
                      ((1 - a_2_1 * A_2) / C_2) * ones)
print(optimal_weights_2_1)
print(t(optimal_weights_2_1) %*% evaluation_data)

# combination 2-2
# ==========================================
Q_2 = 1 / D_2
p_2 = a_2_1 # note how p for goal 2 is the same as a for goal 1
a_2_2 = -Q_2 / p_2
optimal_weights_2_2 = cov_2_inv %*% (a_2_2 * mean_2 + 
                      ((1 - a_2_2 * A_2) / C_2) * ones)
print(optimal_weights_2_2)
print(t(optimal_weights_2_2) %*% evaluation_data)
