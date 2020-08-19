#BAR PLOTS#

library(ggplot2)


#PROTECTED LAND lnd_prt
bap0.2 <-ggplot(data=mydata,aes(x=reorder(name_id, lnd_prt),y=lnd_prt)) + 
  geom_bar(stat ='identity',aes(fill=lnd_prt))+
  ylim(0, 40) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'PROTECTED LAND',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$lnd_prt),size = 1, color = 'blue')
bap0.2


#DEGRADED LAND
bap0.4 <-ggplot(data=mydata,aes(x=reorder(name_id, lnd_dgr),y=lnd_dgr)) + 
  geom_bar(stat ='identity',aes(fill=lnd_dgr))+
  ylim(0, 1.5) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'DEGRADED LAND',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$lnd_dgr),size = 1, color = 'blue')
bap0.4


#CROP LAND
bap0.6 <-ggplot(data=mydata,aes(x=reorder(name_id, lnd_crp),y=lnd_crp)) + 
  geom_bar(stat ='identity',aes(fill=lnd_crp))+
  ylim(0, 100) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'CROP LAND',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$lnd_crp),size = 1, color = 'blue')
bap0.6


#GRAZING LAND
bap0.8 <-ggplot(data=mydata,aes(x=reorder(name_id, lnd_grz),y=lnd_grz)) + 
  geom_bar(stat ='identity',aes(fill=lnd_grz))+
  ylim(0, 100) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'GRAZING LAND',
       y='Percentage',x='Municipalities')+
  geom_hline(yintercept = mean(mydata$lnd_grz),size = 1, color = 'blue')
bap0.8



library(gridExtra)
grid.arrange(bap0.2, bap0.4, bap0.6, bap0.8)










#NO EDUCATION
bap1 <-ggplot(data=mydata,aes(x=reorder(name_id, edctn_n),y=edctn_n)) + 
  geom_bar(stat ='identity',aes(fill=edctn_n))+
  ylim(0, 30) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'NO EDUCATION',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$edctn_n),size = 1, color = 'blue')



#SECONDARY EDUCATION
bap2 <-ggplot(data=mydata,aes(x=reorder(name_id, edctn_s),y=edctn_s)) + 
  geom_bar(stat ='identity',aes(fill=edctn_s))+
  ylim(0, 10) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'SECONDARY EDUCATION',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$edctn_s),size = 1, color = 'blue')


#HIGHER EDUCATION
bap3 <-ggplot(data=mydata,aes(x=reorder(name_id, edctn_h),y=edctn_h)) + 
  geom_bar(stat ='identity',aes(fill=edctn_h))+
  ylim(0, 15) +
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'HIGHER EDUCATION',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$edctn_h),size = 1, color = 'blue')


#ILLITERACY
bap4 <-ggplot(data=mydata,aes(x=reorder(name_id, illtrcy),y=illtrcy)) + 
  geom_bar(stat ='identity',aes(fill=illtrcy))+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'ILLITERACY',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$illtrcy),size = 1, color = 'blue')


#ROOM DENSITY >3
bap5 <-ggplot(data=mydata,aes(x=reorder(name_id, dnsty_r),y=dnsty_r)) + 
  geom_bar(stat ='identity',aes(fill=dnsty_r))+
  ylim(0, 40) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'ROOM DENSITY >3',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$dnsty_r),size = 1, color = 'blue')




##################### THE PROP/POP DENSITIES HAVE EXTREME VALUES (0.00Y - 140) WHICH MEANS THEIR BAR PLOTS AREN'T AS NEAT ###################
# LOW VALUES CAN BE EXPLAINED DUE TO HIGH FOREST COVER IN SOME MUNS, AND OTHER MORE RESIDENTIAL MUNS #


#PROPERTY DENSITY
bap6 <-ggplot(data=mydata,aes(x=reorder(name_id, dnsty_pr),y=dnsty_pr)) + 
  geom_bar(stat ='identity',aes(fill=dnsty_pr))+
  ylim(0, 2) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Density Scale")+
  labs(title = 'PROPERTY DENSITY',
       y='Properties per hectare',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$dnsty_pr),size = 1, color = 'blue')



#POPULATION DENSITY
bap7 <-ggplot(data=mydata,aes(x=reorder(name_id, dnsty_pp),y=dnsty_pp)) + 
  geom_bar(stat ='identity',aes(fill=dnsty_pp))+
  ylim(0, 10) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Density Scale")+
  labs(title = 'POPULATION DENSITY',
       y='Population per hectare',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$dnsty_pp),size = 1, color = 'blue')




#AGROFORESTRY
bap8 <-ggplot(data=mydata,aes(x=reorder(name_id, agrfrst),y=agrfrst)) + 
  geom_bar(stat ='identity',aes(fill=agrfrst))+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'AGROFORESTRY',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$agrfrst),size = 1, color = 'blue')



#RURAL INCOME
bap9 <-ggplot(data=mydata,aes(x=reorder(name_id, incm_rrPLO),y=incm_rrPLO)) + 
  geom_bar(stat ='identity',aes(fill=incm_rrPLO))+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Income Scale")+
  labs(title = 'RURAL INCOME',
       y='Reais',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$incm_rrPLO),size = 1, color = 'blue')



#URBAN INCOME
bap10 <-ggplot(data=mydata,aes(x=reorder(name_id, incm_rbPLO),y=incm_rbPLO)) + 
  geom_bar(stat ='identity',aes(fill=incm_rbPLO))+
  ylim(0, 2200) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Income Scale")+
  labs(title = 'URBAN INCOME',
       y='Reais',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$incm_rbPLO),size = 1, color = 'blue')



#MEDIAN INCOME
bap11 <-ggplot(data=mydata,aes(x=reorder(name_id, incm_mPLO),y=incm_mPLO)) + 
  geom_bar(stat ='identity',aes(fill=incm_mPLO))+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Income Scale")+
  labs(title = 'MEDIAN INCOME',
       y='Reais',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$incm_mPLO),size = 1, color = 'blue')



#BELOW MIN WAGE
bap12 <-ggplot(data=mydata,aes(x=reorder(name_id, incm_b_),y=incm_b_)) + 
  geom_bar(stat ='identity',aes(fill=incm_b_))+
  #ylim(0, 15) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'BELOW MIN WAGE',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$incm_b_),size = 1, color = 'blue')



#NO INCOME
bap13 <-ggplot(data=mydata,aes(x=reorder(name_id, incm_nn),y=incm_nn)) + 
  geom_bar(stat ='identity',aes(fill=incm_nn))+
  ylim(0, 50) +                                                 #Don't include this line for full view of outliers
  coord_flip() + 
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_gradient(name="Percentage Scale")+
  labs(title = 'NO INCOME',
       y='Percentage',x='Municipalities')+ 
  geom_hline(yintercept = mean(mydata$incm_nn),size = 1, color = 'blue')
bap13

library(gridExtra)
grid.arrange(bap1, bap2, bap3, bap4, bap6, bap7, bap8, bap9, bap10, bap11, bap12, bap13) #bap5
