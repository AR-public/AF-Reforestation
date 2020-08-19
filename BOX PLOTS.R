#BOX PLOTS#


#boxplot(mydata$agrfrst_sc) # boxplot of scaled agroforestry across municipalities
#boxplot(agrfrst~abbrv_s, mydata) # boxplot of agroforestry by state. Normally ordered alphabetically


library(RColorBrewer)

#PROTECTED LAND
bop0.2 <-ggplot(data = mydata[c('abbrv_s', 'lnd_prt', 'lnd_dgr', 'lnd_crp', 'lnd_grz')],
              aes(x=abbrv_s, y=lnd_prt)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 30) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'PROTECTED LAND',
       y='Percentage',x='States')
bop0.2


#DEGRADED LAND
bop0.4 <-ggplot(data = mydata[c('abbrv_s', 'lnd_prt', 'lnd_dgr', 'lnd_crp', 'lnd_grz')],
              aes(x=abbrv_s, y=lnd_dgr)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 0.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'DEGRADED LAND',
       y='Percentage',x='States')
bop0.4


#CROP LAND
bop0.6 <-ggplot(data = mydata[c('abbrv_s', 'lnd_prt', 'lnd_dgr', 'lnd_crp', 'lnd_grz')],
              aes(x=abbrv_s, y=lnd_crp)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'CROP LAND',
       y='Percentage',x='States')
bop0.6

#GRAZING LAND
bop0.8 <-ggplot(data = mydata[c('abbrv_s', 'lnd_prt', 'lnd_dgr', 'lnd_crp', 'lnd_grz')],
              aes(x=abbrv_s, y=lnd_grz)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 35) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'GRAZING LAND',
       y='Percentage',x='States')
bop0.8


library(gridExtra)
grid.arrange(bop0.2, bop0.4, bop0.6, bop0.8)






#NO EDUCATION
bop1 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=edctn_n)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 35) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'NO EDUCATION',
       y='Percentage',x='States')



#SECONDARY EDUCATION
bop2 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=edctn_s)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 10) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'SECONDARY EDUCATION',
       y='Percentage',x='States')



#HIGHER EDUCATION
bop3 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=edctn_h)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.005, color = "#00AFBB")+
  labs(title = 'HIGHER EDUCATION',
       y='Percentage',x='States')



#ILLITERACY
bop4 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=illtrcy)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'ILLITERACY',
       y='Percentage',x='States')



#ROOM DENSITY
bop5 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=dnsty_r)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'ROOM DENSITY >3',
       y='Percentage',x='States')



#PROPERTY DENSITY
bop6 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=dnsty_pr)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 1.5) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'PROPERTY DENSITY',
       y='Property / ha',x='States')



#POPULATION DENSITY
bop7 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=dnsty_pp)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 5) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'POPULATION DENSITY',
       y='Population / ha',x='States')



#AGROFORESTRY
bop8 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=agrfrst)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'AGROFORESTRY',
       y='Percentage',x='States')



#RURAL INCOME PLO
bop9 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=incm_rrPLO)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'RURAL INCOME',
       y='Reais',x='States')



#URBAN INCOME PLO
bop10 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=incm_rbPLO)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  ylim(0, 2000) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'URBAN INCOME',
       y='Reais',x='States')



#MEDIAN INCOME PLO
bop11 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=incm_mPLO)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'MEDIAN INCOME',
       y='Reais',x='States')



#BELOW MIN WAGE
bop12 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=incm_b_)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'BELOW MIN WAGE',
       y='Percentage',x='States')




#NO INCOME
bop13 <-ggplot(data = mydata[c('abbrv_s','edctn_n', 'edctn_nPLO', 'edctn_s', 'edctn_sPLO', 'edctn_h', 'edctn_hPLO', 'illtrcy', 'illtPLO', 'dnsty_r', 'dnsty_pr', 'dnsty_pp', 'agrfrst', 'agrfPLO', 'incm_rrPLO', 'incm_rbPLO', 'incm_mPLO', 'incm_b_', 'in__PLO', 'incm_nn', 'incm_nPLO')],
       aes(x=abbrv_s, y=incm_nn)) +
  geom_boxplot(outlier.shape = NA, outlier.color = 'red')+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  guides(fill = "none") +
  geom_jitter(shape=20, position=position_jitter(0.2), size=0.5, color = "#00AFBB")+
  labs(title = 'NO INCOME',
       y='Percentage',x='States')



library(gridExtra)
grid.arrange(bop1, bop2, bop3, bop4, bop6, bop7, bop8, bop9, bop10, bop11, bop12, bop13) #bop5
