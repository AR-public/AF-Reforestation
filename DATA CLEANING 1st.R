rm(list=ls())
#https://github.com/ipeaGIT/geobr
setwd("C:/Users/khayy/OneDrive/Desktop/R files/Anna shared") ## SET YOUR Working directory 

############################
# Chunk: libraries
#############################
library(rgdal)
library(maptools)
library(sp)
library(plyr)
library(geobr)
library(sf)
library(ggplot2)
library(tmap) #can use ggplot if necessary
library(INLA)
library(raster)
library(spdep)
library(SpatialEpi)
library(stringr)
library(rgeos)

library(Rgraphviz)

############################
# Chunk: loading data and shapefile
#############################
#myshp=readOGR(dsn = ".", layer = "gadm36_BRA_2")
load('BrazilData.RData')
#View(myshp)
ls()
#class(myshp)
#dim(myshp)

#?SpatialPolygonsDataFrame 
#help.search('spatial polygon')


############################
#Chunk:    creating area Id AND REMOVING STATE PI      
############################
mydata=myshp@data
mydata=mydata[-c(2330), ]                   #Removing the single entry from state 'PI'
mydata=cbind(mydata, ID=1:nrow(mydata))
#View(mydata)
#structure(mydata)


#put these into the beginning to correct for the rfrsttn_ch error
mydata$rfrsttn_ch=ifelse(mydata$rfrsttn_ch==11,1,mydata$rfrsttn_ch)
table(mydata$rfrsttn_ch)
##### ALSO NEED TO REMOVE 22 MUNICIPALITIES FOR WHICH THERE ARE 0 LAND PARCELS ######


#myshp@data
#list.files()
#myshp - this gives the CRS (coordinate reference system) Error description
#plot(myshp) Plots basic map of municipalities




############################
# Chunk:  neighb str - Encodes the spatial structure of the seemingly random data which we are going to introduce
#############################
temp <- poly2nb(myshp) #polygon to neighbour
temp # tells you how many links there are, how many with no links
nb2INLA("Brazil.graph", temp) # neighbour to INLA. This takes the temp data into INLA 
Brz.adj <- paste(getwd(),"/Brazil.graph",sep="") # copies graphical information into the working directory
""
H <- inla.read.graph(filename="Brazil.graph") #creates the structure of the temp graph with all the municipalities as nodes and their neighbouring nodes
#image(inla.graph2matrix(H),xlab="",ylab="")  
#plot(H) 




################################################
############## 1. char2numeric #################
################################################


mydata$edctn_n=as.numeric(mydata$edctn_n) #myshp@data$edctn_n=as.numeric(myshp@data$edctn_n)
#summary(mydata$edctn_n) # to make sue it's been converted to a numeric format
# You will have to do this for all of them. some of them you want them to stay as characters, like the names of municipalites and states.

#GENERAL
mydata$lnd_pr_=as.numeric(mydata$lnd_pr_)
mydata$rfrsttn_ch=as.numeric(mydata$rfrsttn_ch)
mydata$rfrsttn_cn=as.numeric(mydata$rfrsttn_cn)
mydata$ppltn_t=as.numeric(mydata$ppltn_t)
mydata$ppl_PLO=as.numeric(mydata$ppl_PLO)
mydata$Area=as.numeric(mydata$Area)
#CONTEXT
mydata$lnd_prt=as.numeric(mydata$lnd_prt) #protected land
mydata$lnd_dgr=as.numeric(mydata$lnd_dgr) #degraded land
mydata$lnd_crp=as.numeric(mydata$lnd_crp) #crop land
mydata$lnd_grz=as.numeric(mydata$lnd_grz) #grazing land
#ADOPTER
mydata$edctn_n=as.numeric(mydata$edctn_n)
mydata$edctn_nPLO=as.numeric(mydata$edctn_nPLO)
mydata$edctn_s=as.numeric(mydata$edctn_s)
mydata$edctn_sPLO=as.numeric(mydata$edctn_sPLO)
mydata$edctn_h=as.numeric(mydata$edctn_h)
mydata$edctn_hPLO=as.numeric(mydata$edctn_hPLO)
mydata$illtrcy=as.numeric(mydata$illtrcy)
mydata$illtPLO=as.numeric(mydata$illtPLO)
mydata$dnsty_r=as.numeric(mydata$dnsty_r)    #room density
mydata$dnsty_pr=as.numeric(mydata$dnsty_pr)  #property density
mydata$dnsty_pp=as.numeric(mydata$dnsty_pp)  #population density
mydata$agrfrst=as.numeric(mydata$agrfrst)
mydata$agrfPLO=as.numeric(mydata$agrfPLO)
mydata$incm_rrPLO=as.numeric(mydata$incm_rrPLO)
mydata$incm_rbPLO=as.numeric(mydata$incm_rbPLO)
mydata$incm_mPLO=as.numeric(mydata$incm_mPLO)
mydata$incm_b_=as.numeric(mydata$incm_b_)     #There is an underscore at the end 
mydata$in__PLO=as.numeric(mydata$in__PLO)     #There is a double underscore here
mydata$incm_nn=as.numeric(mydata$incm_nn)
mydata$incm_nPLO=as.numeric(mydata$incm_nPLO)


####################### Removing %'s >100 ##########################

library(tidyverse)
#CONTEXT OUTLIER REMOVAL 
#mydata$lnd_prt[mydata$lnd_prt>100] <- 'na'
#mydata$lnd_dgr[mydata$lnd_dgr>100] <- 'na'
#mydata$lnd_crp[mydata$lnd_crp>70] <- 'na'
#mydata$lnd_grz[mydata$lnd_grz>70] <- 'na'

#ADOPTER OUTLIER REMOVAL

                                                                                            #####NO EDUCATION#####
mydata$edctn_n[mydata$edctn_n>20 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_n=as.numeric(mydata$edctn_n)
mydata$edctn_n=ifelse(is.na(mydata$edctn_n), mean(mydata$edctn_n,na.rm=T), mydata$edctn_n)

mydata$edctn_n[mydata$edctn_n<4 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_n=as.numeric(mydata$edctn_n)
mydata$edctn_n=ifelse(is.na(mydata$edctn_n), mean(mydata$edctn_n,na.rm=T), mydata$edctn_n)

mydata$edctn_n[mydata$edctn_n>22 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_n=as.numeric(mydata$edctn_n)
mydata$edctn_n=ifelse(is.na(mydata$edctn_n), mean(mydata$edctn_n,na.rm=T), mydata$edctn_n)

mydata$edctn_n[mydata$edctn_n<3.2 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_n=as.numeric(mydata$edctn_n)
mydata$edctn_n=ifelse(is.na(mydata$edctn_n), mean(mydata$edctn_n,na.rm=T), mydata$edctn_n)


                                                                                       ####SECONDARY EDUCATION#####
mydata$edctn_s[mydata$edctn_s>6.1 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_s=as.numeric(mydata$edctn_s)
mydata$edctn_s=ifelse(is.na(mydata$edctn_s), mean(mydata$edctn_s,na.rm=T), mydata$edctn_s)

mydata$edctn_s[mydata$edctn_s<2.5 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_s=as.numeric(mydata$edctn_s)
mydata$edctn_s=ifelse(is.na(mydata$edctn_s), mean(mydata$edctn_s,na.rm=T), mydata$edctn_s)

mydata$edctn_s[mydata$edctn_s>6.6 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_s=as.numeric(mydata$edctn_s)
mydata$edctn_s=ifelse(is.na(mydata$edctn_s), mean(mydata$edctn_s,na.rm=T), mydata$edctn_s)

mydata$edctn_s[mydata$edctn_s<2 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_s=as.numeric(mydata$edctn_s)
mydata$edctn_s=ifelse(is.na(mydata$edctn_s), mean(mydata$edctn_s,na.rm=T), mydata$edctn_s)


                                                                                             ####HIGHER EDUCATION####
mydata$edctn_h[mydata$edctn_h>11 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_h=as.numeric(mydata$edctn_h)
mydata$edctn_h=ifelse(is.na(mydata$edctn_h), mean(mydata$edctn_h,na.rm=T), mydata$edctn_h)

mydata$edctn_h[mydata$edctn_h<0.4 & mydata$rfrsttn_ch==1] <- 'na'
mydata$edctn_h=as.numeric(mydata$edctn_h)
mydata$edctn_h=ifelse(is.na(mydata$edctn_h), mean(mydata$edctn_h,na.rm=T), mydata$edctn_h)

mydata$edctn_h[mydata$edctn_h>15 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_h=as.numeric(mydata$edctn_h)
mydata$edctn_h=ifelse(is.na(mydata$edctn_h), mean(mydata$edctn_h,na.rm=T), mydata$edctn_h)

mydata$edctn_h[mydata$edctn_h<0.4 & mydata$rfrsttn_ch==0] <- 'na'
mydata$edctn_h=as.numeric(mydata$edctn_h)
mydata$edctn_h=ifelse(is.na(mydata$edctn_h), mean(mydata$edctn_h,na.rm=T), mydata$edctn_h)




                                                                           ####ILLITERACY####
mydata$illtrcy[mydata$illtrcy>34 & mydata$rfrsttn_ch==1] <- 'na'
mydata$illtrcy=as.numeric(mydata$illtrcy)
mydata$illtrcy=ifelse(is.na(mydata$illtrcy), mean(mydata$illtrcy,na.rm=T), mydata$illtrcy)

mydata$illtrcy[mydata$illtrcy<2 & mydata$rfrsttn_ch==1] <- 'na'
mydata$illtrcy=as.numeric(mydata$illtrcy)
mydata$illtrcy=ifelse(is.na(mydata$illtrcy), mean(mydata$illtrcy,na.rm=T), mydata$illtrcy)

mydata$illtrcy[mydata$illtrcy>40 & mydata$rfrsttn_ch==0] <- 'na'
mydata$illtrcy=as.numeric(mydata$illtrcy)
mydata$illtrcy=ifelse(is.na(mydata$illtrcy), mean(mydata$illtrcy,na.rm=T), mydata$illtrcy)

mydata$illtrcy[mydata$illtrcy<3 & mydata$rfrsttn_ch==0] <- 'na'
mydata$illtrcy=as.numeric(mydata$illtrcy)
mydata$illtrcy=ifelse(is.na(mydata$illtrcy), mean(mydata$illtrcy,na.rm=T), mydata$illtrcy)



                                                                              ####PROPERTY DENSITY###
mydata$dnsty_pr[mydata$dnsty_pr>0.60 & mydata$rfrsttn_ch==1] <- 'na'
mydata$dnsty_pr=as.numeric(mydata$dnsty_pr)
mydata$dnsty_pr=ifelse(is.na(mydata$dnsty_pr), mean(mydata$dnsty_pr,na.rm=T), mydata$dnsty_pr)

mydata$dnsty_pr[mydata$dnsty_pr<0 & mydata$rfrsttn_ch==1] <- 'na'
mydata$dnsty_pr=as.numeric(mydata$dnsty_pr)
mydata$dnsty_pr=ifelse(is.na(mydata$dnsty_pr), mean(mydata$dnsty_pr,na.rm=T), mydata$dnsty_pr)

mydata$dnsty_pr[mydata$dnsty_pr>1.5 & mydata$rfrsttn_ch==0] <- 'na'
mydata$dnsty_pr=as.numeric(mydata$dnsty_pr)
mydata$dnsty_pr=ifelse(is.na(mydata$dnsty_pr), mean(mydata$dnsty_pr,na.rm=T), mydata$dnsty_pr)

mydata$dnsty_pr[mydata$dnsty_pr<0 & mydata$rfrsttn_ch==0] <- 'na'
mydata$dnsty_pr=as.numeric(mydata$dnsty_pr)
mydata$dnsty_pr=ifelse(is.na(mydata$dnsty_pr), mean(mydata$dnsty_pr,na.rm=T), mydata$dnsty_pr)


                                                                              ###POPULATION DENSITY####

mydata$dnsty_pp[mydata$dnsty_pp>1.9 & mydata$rfrsttn_ch==1] <- 'na'
mydata$dnsty_pp=as.numeric(mydata$dnsty_pp)
mydata$dnsty_pp=ifelse(is.na(mydata$dnsty_pp), mean(mydata$dnsty_pp,na.rm=T), mydata$dnsty_pp)

mydata$dnsty_pp[mydata$dnsty_pp<0 & mydata$rfrsttn_ch==1] <- 'na'
mydata$dnsty_pp=as.numeric(mydata$dnsty_pp)
mydata$dnsty_pp=ifelse(is.na(mydata$dnsty_pp), mean(mydata$dnsty_pp,na.rm=T), mydata$dnsty_pp)

mydata$dnsty_pp[mydata$dnsty_pp>4.8 & mydata$rfrsttn_ch==0] <- 'na'
mydata$dnsty_pp=as.numeric(mydata$dnsty_pp)
mydata$dnsty_pp=ifelse(is.na(mydata$dnsty_pp), mean(mydata$dnsty_pp,na.rm=T), mydata$dnsty_pp)

mydata$dnsty_pp[mydata$dnsty_pp<0 & mydata$rfrsttn_ch==0] <- 'na'
mydata$dnsty_pp=as.numeric(mydata$dnsty_pp)
mydata$dnsty_pp=ifelse(is.na(mydata$dnsty_pp), mean(mydata$dnsty_pp,na.rm=T), mydata$dnsty_pp)


                                                                                ###AGROFORESTRY###
mydata$agrfrst[mydata$agrfrst>41 & mydata$rfrsttn_ch==1] <- 'na'
mydata$agrfrst=as.numeric(mydata$agrfrst)
mydata$agrfrst=ifelse(is.na(mydata$agrfrst), mean(mydata$agrfrst,na.rm=T), mydata$agrfrst)

mydata$agrfrst[mydata$agrfrst<0 & mydata$rfrsttn_ch==1] <- 'na'
mydata$agrfrst=as.numeric(mydata$agrfrst)
mydata$agrfrst=ifelse(is.na(mydata$agrfrst), mean(mydata$agrfrst,na.rm=T), mydata$agrfrst)

mydata$agrfrst[mydata$agrfrst>54 & mydata$rfrsttn_ch==0] <- 'na'
mydata$agrfrst=as.numeric(mydata$agrfrst)
mydata$agrfrst=ifelse(is.na(mydata$agrfrst), mean(mydata$agrfrst,na.rm=T), mydata$agrfrst)

mydata$agrfrst[mydata$agrfrst<0 & mydata$rfrsttn_ch==0] <- 'na'
mydata$agrfrst=as.numeric(mydata$agrfrst)
mydata$agrfrst=ifelse(is.na(mydata$agrfrst), mean(mydata$agrfrst,na.rm=T), mydata$agrfrst)


                                                                               #####RURAL INCOME#####

mydata$incm_rrPLO[mydata$incm_rrPLO>1100 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_rrPLO=as.numeric(mydata$incm_rrPLO)
mydata$incm_rrPLO=ifelse(is.na(mydata$incm_rrPLO), mean(mydata$incm_rrPLO,na.rm=T), mydata$incm_rrPLO)

mydata$incm_rrPLO[mydata$incm_rrPLO<10 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_rrPLO=as.numeric(mydata$incm_rrPLO)
mydata$incm_rrPLO=ifelse(is.na(mydata$incm_rrPLO), mean(mydata$incm_rrPLO,na.rm=T), mydata$incm_rrPLO)

mydata$incm_rrPLO[mydata$incm_rrPLO>1500 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_rrPLO=as.numeric(mydata$incm_rrPLO)
mydata$incm_rrPLO=ifelse(is.na(mydata$incm_rrPLO), mean(mydata$incm_rrPLO,na.rm=T), mydata$incm_rrPLO)

mydata$incm_rrPLO[mydata$incm_rrPLO<100 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_rrPLO=as.numeric(mydata$incm_rrPLO)
mydata$incm_rrPLO=ifelse(is.na(mydata$incm_rrPLO), mean(mydata$incm_rrPLO,na.rm=T), mydata$incm_rrPLO)


    
                                                                                          ####URBAN INCOME####
mydata$incm_rbPLO[mydata$incm_rbPLO>1350 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_rbPLO=as.numeric(mydata$incm_rbPLO)
mydata$incm_rbPLO=ifelse(is.na(mydata$incm_rbPLO), mean(mydata$incm_rbPLO,na.rm=T), mydata$incm_rbPLO)

mydata$incm_rbPLO[mydata$incm_rbPLO<0 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_rbPLO=as.numeric(mydata$incm_rbPLO)
mydata$incm_rbPLO=ifelse(is.na(mydata$incm_rbPLO), mean(mydata$incm_rbPLO,na.rm=T), mydata$incm_rbPLO)

mydata$incm_rbPLO[mydata$incm_rbPLO>1400 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_rbPLO=as.numeric(mydata$incm_rbPLO)
mydata$incm_rbPLO=ifelse(is.na(mydata$incm_rbPLO), mean(mydata$incm_rbPLO,na.rm=T), mydata$incm_rbPLO)

mydata$incm_rbPLO[mydata$incm_rbPLO<0 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_rbPLO=as.numeric(mydata$incm_rbPLO)
mydata$incm_rbPLO=ifelse(is.na(mydata$incm_rbPLO), mean(mydata$incm_rbPLO,na.rm=T), mydata$incm_rbPLO)



                                                                                           ###MEDIAN INCOME####

mydata$incm_mPLO[mydata$incm_mPLO>900 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_mPLO=as.numeric(mydata$incm_mPLO)
mydata$incm_mPLO=ifelse(is.na(mydata$incm_mPLO), mean(mydata$incm_mPLO,na.rm=T), mydata$incm_mPLO)

mydata$incm_mPLO[mydata$incm_mPLO<0 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_mPLO=as.numeric(mydata$incm_mPLO)
mydata$incm_mPLO=ifelse(is.na(mydata$incm_mPLO), mean(mydata$incm_mPLO,na.rm=T), mydata$incm_mPLO)

mydata$incm_mPLO[mydata$incm_mPLO>950 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_mPLO=as.numeric(mydata$incm_mPLO)
mydata$incm_mPLO=ifelse(is.na(mydata$incm_mPLO), mean(mydata$incm_mPLO,na.rm=T), mydata$incm_mPLO)

mydata$incm_mPLO[mydata$incm_mPLO<0 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_mPLO=as.numeric(mydata$incm_mPLO)
mydata$incm_mPLO=ifelse(is.na(mydata$incm_mPLO), mean(mydata$incm_mPLO,na.rm=T), mydata$incm_mPLO)



                                                                                      ####BELOW MIN WAGE INCOME####
mydata$incm_b_[mydata$incm_b_>46 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_b_=as.numeric(mydata$incm_b_)
mydata$incm_b_=ifelse(is.na(mydata$incm_b_), mean(mydata$incm_b_,na.rm=T), mydata$incm_b_)

mydata$incm_b_[mydata$incm_b_<11 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_b_=as.numeric(mydata$incm_b_)
mydata$incm_b_=ifelse(is.na(mydata$incm_b_), mean(mydata$incm_b_,na.rm=T), mydata$incm_b_)

mydata$incm_b_[mydata$incm_b_>48 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_b_=as.numeric(mydata$incm_b_)
mydata$incm_b_=ifelse(is.na(mydata$incm_b_), mean(mydata$incm_b_,na.rm=T), mydata$incm_b_)

mydata$incm_b_[mydata$incm_b_<9 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_b_=as.numeric(mydata$incm_b_)
mydata$incm_b_=ifelse(is.na(mydata$incm_b_), mean(mydata$incm_b_,na.rm=T), mydata$incm_b_)


                                                                                        ####NO INCOME####

mydata$incm_nn[mydata$incm_nn>40 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_nn=as.numeric(mydata$incm_nn)
mydata$incm_nn=ifelse(is.na(mydata$incm_nn), mean(mydata$incm_nn,na.rm=T), mydata$incm_nn)

mydata$incm_nn[mydata$incm_nn<13.5 & mydata$rfrsttn_ch==1] <- 'na'
mydata$incm_nn=as.numeric(mydata$incm_nn)
mydata$incm_nn=ifelse(is.na(mydata$incm_nn), mean(mydata$incm_nn,na.rm=T), mydata$incm_nn)

mydata$incm_nn[mydata$incm_nn>42 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_nn=as.numeric(mydata$incm_nn)
mydata$incm_nn=ifelse(is.na(mydata$incm_nn), mean(mydata$incm_nn,na.rm=T), mydata$incm_nn)

mydata$incm_nn[mydata$incm_nn<9.5 & mydata$rfrsttn_ch==0] <- 'na'
mydata$incm_nn=as.numeric(mydata$incm_nn)
mydata$incm_nn=ifelse(is.na(mydata$incm_nn), mean(mydata$incm_nn,na.rm=T), mydata$incm_nn)










#################################
####### 3. STANDARDISING ########
#################################

#Use mydata to  standardize  the variables for the model (using scale function) because INLA deals better with scale 
#quantities 
mydata$edctn_n_sc=scale(mydata$edctn_n) #myshp@data$edctn_n_sc=scale(myshp@data$edctn_n)
#Scaling removes the general mean from each observation and divides by standard deviation to bring the new general mean to 0 and new standard deviation close to 1, or at least much smaller.
#The range of values is scaled to a narrower range
summary(mydata$edctn_n_sc)
#?scale #gives more information on how it works and even gives an example at the bottom


mydata$edctn_n_sc=scale(mydata$edctn_n)
mydata$edctn_nPLO_sc=scale(mydata$edctn_nPLO)
mydata$edctn_s_sc=scale(mydata$edctn_s)
mydata$edctn_sPLO_sc=scale(mydata$edctn_sPLO)
mydata$edctn_h_sc=scale(mydata$edctn_h)
mydata$edctn_hPLO_sc=scale(mydata$edctn_hPLO)
mydata$illtrcy_sc=scale(mydata$illtrcy)
mydata$illtPLO_sc=scale(mydata$illtPLO)
mydata$dnsty_r_sc=scale(mydata$dnsty_r)
mydata$dnsty_pr_sc=scale(mydata$dnsty_pr)
mydata$dnsty_pp_sc=scale(mydata$dnsty_pp)
mydata$agrfrst_sc=scale(mydata$agrfrst)
mydata$agrfPLO_sc=scale(mydata$agrfPLO)
mydata$incm_rrPLO_sc=scale(mydata$incm_rrPLO)
mydata$incm_rbPLO_sc=scale(mydata$incm_rbPLO)
mydata$incm_mPLO_sc=scale(mydata$incm_mPLO)
mydata$incm_b__sc=scale(mydata$incm_b_)
mydata$in__PLO_sc=scale(mydata$in__PLO)
mydata$incm_nn_sc=scale(mydata$incm_nn)
mydata$incm_nPLO_sc=scale(mydata$incm_nPLO)

mydata$avg_incm = (mydata$incm_rrPLO + mydata$incm_rbPLO)/2

###########################################################
#Chunk:   SPLITTING SCALED VARIABLES INTO QUANTILES########
###########################################################

#mydata$agrfrst_sc=as.numeric(mydata$agrfrst_sc)
#mydata$agrfrst1=ifelse(is.na(mydata$agrfrst_sc), mean(mydata$agrfrst_sc,na.rm=T), mydata$agrfrst_sc)

#mydata$edctn_n_cat=cut(mydata$edctn_n_sc, breaks=c(quantile(mydata$edctn_n_sc,
 #                                                           probs = seq(0, 1, by = 0.25))))

#mydata$edctn_s_cat=cut(mydata$edctn_s_sc, breaks=c(quantile(mydata$edctn_s_sc,
  #                                                          probs = seq(0, 1, by = 0.25))))

#mydata$edctn_h_cat=cut(mydata$edctn_h_sc, breaks=c(quantile(mydata$edctn_h_sc,
 #                                                           probs = seq(0, 1, by = 0.25))))

#mydata$illtrcy_cat=cut(mydata$illtrcy_sc, breaks=c(quantile(mydata$illtrcy_sc,
 #                                                           probs = seq(0, 1, by = 0.25))))

#mydata$dnsty_pr_cat=cut(mydata$dnsty_pr_sc, breaks=c(quantile(mydata$dnsty_pr_sc,
#                                                              probs = seq(0, 1, by = 0.25))))

#mydata$dnsty_pp_cat=cut(mydata$dnsty_pp_sc, breaks=c(quantile(mydata$dnsty_pp_sc,
 #                                                             probs = seq(0, 1, by = 0.25))))

#mydata$agrfrst_cat=cut(mydata$agrfrst_sc, breaks=c(quantile(mydata$agrfrst_sc,
 #                                                           probs = seq(0, 1, by = 0.25))))

#mydata$incm_rrPLO_cat=cut(mydata$incm_rrPLO_sc, breaks=c(quantile(mydata$incm_rrPLO_sc,
 #                                                                 probs = seq(0, 1, by = 0.25))))

#mydata$incm_rbPLO_cat=cut(mydata$incm_rbPLO_sc, breaks=c(quantile(mydata$incm_rbPLO_sc,
 #                                                                 probs = seq(0, 1, by = 0.25))))

#mydata$incm_mPLO_cat=cut(mydata$incm_mPLO_sc, breaks=c(quantile(mydata$incm_mPLO_sc,
 #                                                               probs = seq(0, 1, by = 0.25))))

#mydata$incm_b__cat=cut(mydata$incm_b__sc, breaks=c(quantile(mydata$incm_b__sc,
 #                                                           probs = seq(0, 1, by = 0.25))))

#mydata$incm_nn_cat=cut(mydata$incm_nn_sc, breaks=c(quantile(mydata$incm_nn_sc,
 #                                                           probs = seq(0, 1, by = 0.25))))




mydata$avg_incm_cat = cut(mydata$avg_incm, breaks=c(quantile(mydata$avg_incm,
                                                             probs = seq(0, 1, by = 0.33333333333))))

mydata$edctn_s_cat=cut(mydata$edctn_s, breaks=c(quantile(mydata$edctn_s,
                                                            probs = seq(0, 1, by = 0.33333333333))))

mydata$dnsty_pp_cat=cut(mydata$dnsty_pp, breaks=c(quantile(mydata$dnsty_pp,
                                                              probs = seq(0, 1, by = 0.25))))
mydata$dnsty_pp_cat = cut(mydata$dnsty_pp, breaks=c(0,0.3,0.6,5))

summary(mydata$dnsty_pp_cat)
