##### ACTL30007 Assignment 2 (929715)

# load packages
# install.packages('astsa')
# install.packages('readxl')
library(astsa)
library(readxl)

# import data
dataset <- read_excel('C:/Unimelb/Sem6/AM3/Assignment 2/china_vs_australia_covid-19.xlsx')
# assign the vector variables
time = time(dataset$Date)
market_aus = dataset$`S&P/ASX 200`
market_chn = dataset$`Shanghai Composite`
cases_aus = dataset$`Australia New Daily Cases`
cases_chn = dataset$`China New Daily Cases`
# visual analysis
month_vec = c(2, 33, 62, 93, 123)
par(mfrow = c(4,1))
plot(market_aus~time, xaxt = 'n', type = 'o', col = 'red', ylim = c(4000, 7500), main = 'Australian Market Index (S&P/ASX 200)', xlab = 'Time', ylab = 'Index')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
plot(market_chn~time, xaxt = 'n', type = 'o', col = 'blue', ylim = c(2600, 3200), main = 'Chinese Market Index (Shanghai Composite)', xlab = 'Time', ylab = 'Index')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
plot(cases_aus~time, xaxt = 'n', type = 'o', col = 'orange', ylim = c(0, 700), main = 'Australia Daily New Cases', xlab = 'Time', ylab = 'New cases')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
plot(cases_chn~time, xaxt = 'n', type = 'o', col = 'green', ylim = c(0, 16000), main = 'China Daily New Cases', xlab = 'Time', ylab = 'New cases')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))


## detrending via differencing
det_market_aus = diff(market_aus)
# visualising the difference plots
par(mfrow = c(2, 1))
plot(det_market_aus~time[-1], xaxt = 'n', type = 'o', main = 'Australian Market Index (S&P/ASX 200) \ndetrended via first difference', xlab = 'Time', ylab = 'Difference')
axis(1, at = month_vec-1, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(det_market_aus, f = 2/3), lwd = 2, lty = 2, col = 'red')
abline(0, 0, lty = 2)
legend('bottomright', legend = 'Lowess with f = 2/3', fill = 'red')
# acf plot
acf(det_market_aus, lag.max = 50, main = 'ACF Plot of Australian Market Index (S&P/ASX 200) \ndetrended via first difference')

## fitting regression model 1
# model fit
model1 = lm(market_aus~time+market_chn+cases_aus+cases_chn)
summary(model1)
# plots to check stationarity
# residual plot
par(mfrow = c(2, 1))
plot(residuals(model1)~time, xaxt = 'n', type = 'o', col = 'chocolate4', main = 'Residual Plot (Model 1)', xlab = 'Time', ylab = 'Residual')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(residuals(model1), f = 2/3), lwd = 2, lty = 2, col = 'red')
abline(0, 0, lty = 2)
legend('bottomright', legend = 'Lowess with f = 2/3', fill = 'red')
# acf plot of residuals
acf(residuals(model1), lag.max =  50, col = 'chocolate4', main = 'ACF Plot of Residuals (Model 1)')
# goodness-of-fit plot
par(mfrow = c(1, 1))
plot(market_aus~time, xaxt = 'n', type = 'o', ylim = c(4000, 7500), main = 'Australian Market Index (S&P/ASX 200)', xlab = 'Time', ylab = 'Index')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(market_aus, f = 1/3), lwd = 2, lty = 2, col = 'red')
lines(model1$coefficients[1]+model1$coefficients[2]*time+model1$coefficients[3]*market_chn+model1$coefficients[4]*cases_aus+model1$coefficients[5]*cases_aus, lwd = 2, col = 'chocolate4')
legend("topright", legend = c('Lowess with f = 1/3', 'Model 1'), fill = c('red', 'chocolate4'))


## scatterplot matrix
panel <- function(x, y){
  usr <- par('usr')
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits = 2)
  corr <- bquote(rho == .(r))
  text(0.5, 0.5, corr, col = 'violetred4')
}
pairs(cbind(Australian_Market = market_aus, Chinese_Market = market_chn, Australia_New_Cases = cases_aus, China_New_Cases = cases_chn), lower.panel = panel)

## fitting regression model 2
# square vectors
cases_aus_sq <- cases_aus^2
cases_chn_sq <- cases_chn^2
# model fit
model2 <- lm(market_aus~time+market_chn+cases_aus+cases_aus_sq+cases_chn+cases_chn_sq)
summary(model2)
# plots to check stationarity
par(mfrow = c(2, 1))
# residual plot
plot(residuals(model2)~time, xaxt = 'n', type = 'o', col = 'turquoise4', main = 'Residual Plot (Model 2)', xlab = 'Time', ylab = 'Residual')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(residuals(model2), f = 2/3), lwd = 2, lty = 2, col = 'red')
abline(0, 0, lty = 2)
legend('bottomright', legend = 'Lowess with f = 2/3', fill = 'red')
# acf plot of residuals
acf(residuals(model2), lag.max =  50, col = 'turquoise4', main = 'ACF Plot of Residuals (Model 2)')
# goodness-of-fit plot
par(mfrow = c(1, 1))
plot(market_aus~time, xaxt = 'n', type = 'o', ylim = c(4000, 7500), main = 'Australian Market Index (S&P/ASX 200)', xlab = 'Time', ylab = 'Index')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(market_aus, f = 1/3), lwd = 2, lty = 2, col = 'red')
lines(model2$coefficients[1]+model2$coefficients[2]*time+model2$coefficients[3]*market_chn+model2$coefficients[4]*cases_aus+model2$coefficients[5]*cases_aus_sq+model2$coefficients[6]*cases_chn+model2$coefficients[7]*cases_chn_sq, lwd = 2, col = 'turquoise4')
legend("topright", legend = c('Lowess with f = 1/3', 'Model 2'), fill = c('red', 'turquoise4'))


## cross-correlation and lag plots
par(mfrow = c(4, 1))
acf(market_aus, lag.max = 30, main = 'ACF Plot of Australian Market Index (S&P/ASX 200)')
ccf(market_aus, market_chn, lag.max = 30, main = 'CCF Plot of Australian Market Index (S&P/ASX 200) \nand Chinese Market Index (Shanghai Composite)')
ccf(market_aus, cases_aus, lag.max = 30, main = 'CCF Plot of Australian Market Index (S&P/ASX 200) \nand Australia Daily New Cases')
ccf(market_aus, cases_chn, lag.max = 30, main = 'CCF Plot of Australian Market Index (S&P/ASX 200) \nand China Daily New Cases')
lag2.plot(market_aus, cases_aus, max.lag = 11)
lag2.plot(cases_chn, market_aus, max.lag = 11)

## fitting regression model 3
# lead and lag functions
func_lag <- function(x, lag){
  stop <- length(x) - lag
  c(rep(NA,lag), x[(1:stop)])
}
func_lead <- function(x, lead){
  c(x[-(1:lead)], rep(NA, lead))
}
# lead and lag vectors
cases_aus_lead2 <- func_lead(cases_aus, 2)
cases_aus_lead3 <- func_lead(cases_aus, 3)
cases_aus_lead4 <- func_lead(cases_aus, 4)
cases_chn_lag5 <- func_lag(cases_chn, 5)
cases_chn_lag6 <- func_lag(cases_chn, 6)
cases_chn_lag7 <- func_lag(cases_chn, 7)
# model fit
model3 <- lm(market_aus~time+market_chn+cases_aus_lead2+cases_aus_lead3+cases_aus_lead4+cases_chn_lag5+cases_chn_lag6+cases_chn_lag7)
summary(model3)
# plots to check stationarity
par(mfrow = c(2, 1))
# residual plot
plot(residuals(model3)~time[-c(1:7, 147:150)], xaxt = 'n', type = 'o', col = 'chartreuse4', main = 'Residual Plot (Model 3)', xlab = 'Time', ylab = 'Residual')
axis(1, at = month_vec-8, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(residuals(model3), f = 2/3), lwd = 2, lty = 2, col = 'red')
abline(0, 0, lty = 2)
legend('bottomright', legend = 'Lowess with f = 2/3', fill = 'red')
# acf plot of residuals
acf(residuals(model3), lag.max =  50, col = 'chartreuse4', main = 'ACF Plot of Residuals (Model 3)')
# goodness-of-fit plot
par(mfrow = c(1, 1))
plot(market_aus~time, xaxt = 'n', type = 'o', ylim = c(4000, 7500), main = 'Australian Market Index (S&P/ASX 200)', xlab = 'Time', ylab = 'Index')
axis(1, at = month_vec, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May'))
lines(lowess(market_aus, f = 1/3), lwd = 2, lty = 2, col = 'red')
lines(model3$coefficients[1]+model3$coefficients[2]*time+model3$coefficients[3]*market_chn+model3$coefficients[4]*cases_aus_lead2+model3$coefficients[5]*cases_aus_lead3+model3$coefficients[6]*cases_aus_lead4+model3$coefficients[7]*cases_chn_lag5+model3$coefficients[8]*cases_chn_lag6+model3$coefficients[9]*cases_chn_lag7, lwd = 2, col = 'chartreuse4')
legend("topright", legend = c('Lowess with f = 1/3', 'Model 3'), fill = c('red', 'chartreuse4'))


