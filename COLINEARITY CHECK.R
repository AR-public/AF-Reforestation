#CORRELATION MATRIX
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software


#Compute correlation matrix
res<- cor(mydata[c('lnd_dgr', 'lnd_prt', 'lnd_crp', 'lnd_grz', 'edctn_n', 'edctn_s', 'edctn_h', 'illtrcy', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'incm_nn')])
round(res,2)




#Correlation matrix with significance levels (p-value)
library(Hmisc)
rcorr(res, type = c("pearson"))

res2 <- rcorr(as.matrix(mydata[c('lnd_dgr', 'lnd_prt', 'lnd_crp', 'lnd_grz', 'edctn_n', 'edctn_s', 'edctn_h', 'illtrcy', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'incm_nn')]))
#res2




#Use corrplot() function: Draw a correlogram
# THIS CREATES A DIAGONAL PLOT WITH CIRCLES AS INDICATORS
#install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90, insig='blank')
  

#SQUARE PLOT AND MOST VISUALLY APPEALING IMO
corrplot(res, method='color', tl.col = "black", order = "hclust")
