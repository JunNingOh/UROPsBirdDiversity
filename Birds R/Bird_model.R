#Note: Requires data from file:Datas


###Packages##
library(sars)
############

testfit_avg=sar_average(data = Bird_data_BC)
outlier_Bird_bc = Bird_data_BC[-15,]
testfit_avg_2=sar_average(data=outlier_Bird_bc) ##best logistic

finalbirdfit=sar_logistic(data=outlier_Bird_bc)
summary(finalbirdfit)
#Call: S==d/(1+exp(-z*A+c))
#converge:true
#d = 67.709
#z = 0.021256
#c = 3.6780