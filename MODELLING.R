                        ##########################################################################
                        ########################### Modelling ####################################
                        ##########################################################################

                        if(!require(ggregplot)) devtools::install_github("gfalbery/ggregplot") # Installing Greg's package for plotting functions!
                        
                        library(INLA); library(ggplot2); library(ggregplot)
                        library(tidyverse)
                        library(RColorBrewer)
                        
                        
                        
                        
                        
# LINEAR MODEL # NOT A PROPER MODEL. WOULD HARM YOUR THESIS TO MENTION THIS WAS EVEN CONSIDERED.
                        #options(scipen=999)
                        #reg.eq1 <- rfrsttn_ch ~ edctn_n_sc + edctn_s_sc + edctn_h_sc + 
                        #  dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                        #  incm_b__sc + incm_nn_sc
                        #reg1 <- lm(reg.eq1, data = mydata)
                        #summary(reg1)
                        # Multiple R^2 is very small (0.01663).
                        # I believe this means it's not an ideal fit for what's being observed
                        # Therefore, spatial effects should be taken into consideration  
                      
                        
                        
# GLM LOGISTIC REGRESSION
                        
                        logistic <- glm(rfrsttn_ch ~ edctn_n + edctn_s + edctn_h + 
                                          dnsty_pp + agrfrst + incm_rrPLO + incm_rbPLO +
                                          incm_b_ + incm_nn, data=mydata, family="binomial")
                        summary(logistic)
                        
                        table <- round(coef(summary(logistic)), 4)
                        #View(table)
                        
                        ## Now calculate the overall "Pseudo R-squared" and its p-value
                        ll.null <- logistic$null.deviance/-2
                        ll.proposed <- logistic$deviance/-2
                        
                        ## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
                        (ll.null - ll.proposed) / ll.null
                        
                        ## The p-value for the R^2
                        1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
                        
                        library(ggplot2)
                        library(cowplot)
                        theme_set(theme_cowplot())
                        ## now we can plot the data
                        predicted.data <- data.frame(probability.of.rfrsttn_ch=logistic$fitted.values, rfrsttn_ch=mydata$rfrsttn_ch)
                        #View(predicted.data)
                        
                        predicted.data <- predicted.data[
                          order(predicted.data$probability.of.rfrsttn_ch, decreasing=FALSE),]
                        predicted.data$rank <- 1:nrow(predicted.data)
                        
                        ## Lastly, we can plot the predicted probabilities for each sample having
                        ## heart disease and color by whether or not they actually had heart disease
                        ggplot(data=predicted.data, aes(x=rank, y=probability.of.rfrsttn_ch)) +
                          geom_point(aes(color=rfrsttn_ch), alpha=1, shape=4, stroke=2) +
                          xlab("Municipalities") +
                          ylab("Predicted probability of reforestation")
                        
                        #ggsave("Reforestation GLM.pdf") #SAVES GGPLOT AS A PDF
                        
                        #Comment on the poor fit (Low R^2, high AIC, which marginally improves, even as the model is simplified (i.e. reducing the number of variables))
                        #The AIC reduced from 1984.9 to 1982.1 after removing all but the incomes as they had the only p values <0.05
                        
                        #The model is too poor a fit to reliably comment on          
                        
                        
                        
#INLA (iid)
                       #iid9Vars
                         formula.iid9 = rfrsttn_ch ~ 1 + edctn_n_sc + edctn_s_sc + edctn_h_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          incm_b__sc + incm_nn_sc + f(ID, model='iid', 
                                                      graph='Brazil.graph', adjust.for.con.comp=TRUE)
                         result.iid9= inla(formula.iid9, family='binomial',
                                        data=mydata,
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        
                        summary(result.iid9)
                        
                        
                        
                        #iid5Vars
                        formula2 = rfrsttn_ch ~ 1 + edctn_n_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          f(ID, model='bym', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        result2= inla(formula2, family='binomial',
                                        data=mydata,
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        
                        summary(result2)
                        
                        
                        
                        
                        
#INLA (bym)
                        #bym9Vars
                        formula2.5 = rfrsttn_ch ~ 1 + edctn_n_sc + edctn_s_sc + edctn_h_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          incm_b__sc + incm_nn_sc + f(ID, model='bym', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        
                        result2.5= inla(formula2.5,
                                        family='binomial', # based on the distribution of your outcome, it could be 'poisson' 'binomial' or 'zero inflated poisson'
                                        # if the outcome is  a rate (then the total lnd pr should be  accounted )
                                        data=mydata, # myshp@data
                                        control.predictor = list(compute=TRUE),
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        summary(result2.5)  
                        
                        
                      
                        
                        #bym5Vars
                        formula3 = rfrsttn_ch ~ 1 + edctn_n_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          f(ID, model='bym', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        
                        result3= inla(formula3,
                                        family='binomial', # based on the distribution of your outcome, it could be 'poisson' 'binomial' or 'zero inflated poisson'
                                        # if the outcome is  a rate (then the total lnd pr should be  accounted )
                                        data=mydata, # myshp@data
                                        control.predictor = list(compute=TRUE),
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        summary(result3)  
                        
                        #plot_inla_residuals(result3)
                        
                        
                        
###COMPARING the 9Variable iid AND bym MODELS###                        

#Forest Plot                                                
#install.packages('MCMCglmm')                        
library(MCMCglmm)

                        Efxplot(list(result1.5, result2.5))                        
                        


#Checking which Covariates to remove (if any) 
                        
                        resp <- 'rfrsttn_ch'
                        
                        covar <- c('edctn_n_sc',
                                   'edctn_s_sc',
                                   'edctn_h_sc',
                                   'dnsty_pp_sc',
                                   'dnsty_pp_sc',
                                   'agrfrst_sc',
                                   'incm_rrPLO_sc',
                                   'incm_rbPLO_sc',
                                   'incm_b__sc',
                                   'incm_nn_sc')
                        
                        
                        #HostModelSel <- INLAModelSel(resp, covar, "ID", "iid", "binomial", mydata)
                        
                        #Finalcovar <- HostModelSel$Removed[[length(HostModelSel$Removed)]]
                        View(Finalcovar)
                    
                        #FINAL COVARIATES:
                        #1	edctn_n_sc
                        #2	dnsty_pp_sc
                        #3	agrfrst_sc
                        #4	incm_rrPLO_sc
                        #5	incm_rbPLO_sc
                        
                        
                        #ggField(result3, myshp, Groups = 1) +     #ATTEMPT AT PLOTTING THE SPATIAL FIELD TO VISUALLY COMPARE SPATIAL EFFECTS ON VARIATION
                        #  scale_fill_brewer(palette = "Blues")
                        #Maxrange = 40
                        #INLARange(list(result3), MaxRange = Maxrange, Mesh=myshp)   
                        
                        #Creating list of models to compare
                        SpatialHostList <- list(result1.5, result2, result2.5, result3)
                        #Comparing DICs between different models
                        INLADICFig(SpatialHostList, ModelNames = c("IID9", "IID5", "BYM9", "BYM5"))
                        #Comparing 95% CI between models
                        Efxplot(SpatialHostList, ModelNames = c("IID9", "IID5", "BYM9", "BYM5"))


                        
                        
                        #######################################
                        #PLOTTING MARGINALS for bym5
                        ######################################
                        
                                                
                        library(ggplot2)
                        marginal <- inla.smarginal(result3$marginals.fixed$edctn_n_sc)
                        marginal <- data.frame(marginal)
                        ggplot(marginal, aes(x = x, y = y)) + geom_line() +
                          labs(x = expression(beta[1]), y = "Density") +
                          ggtitle("NO EDUCATION") +
                          geom_vline(xintercept = 0, col = "black") + theme_bw()
                        
                        marginal <- inla.smarginal(result3$marginals.fixed$dnsty_pp_sc)
                        marginal <- data.frame(marginal)
                        ggplot(marginal, aes(x = x, y = y)) + geom_line() +
                          labs(x = expression(beta[1]), y = "Density") +
                          ggtitle("POPULATIN DENSITY") +
                          geom_vline(xintercept = 0, col = "black") + theme_bw()
                        
                        marginal <- inla.smarginal(result3$marginals.fixed$agrfrst_sc)
                        marginal <- data.frame(marginal)
                        ggplot(marginal, aes(x = x, y = y)) + geom_line() +
                          labs(x = expression(beta[1]), y = "Density") +
                          ggtitle("AGROFORESTRY") +
                          geom_vline(xintercept = 0, col = "black") + theme_bw()
                        
                        marginal <- inla.smarginal(result3$marginals.fixed$incm_rrPLO_sc)
                        marginal <- data.frame(marginal)
                        ggplot(marginal, aes(x = x, y = y)) + geom_line() +
                          labs(x = expression(beta[1]), y = "Density") +
                          ggtitle("RURAL INCOME") +
                          geom_vline(xintercept = 0, col = "black") + theme_bw()
                        
                        marginal <- inla.smarginal(result3$marginals.fixed$incm_rbPLO_sc)
                        marginal <- data.frame(marginal)
                        ggplot(marginal, aes(x = x, y = y)) + geom_line() +
                          labs(x = expression(beta[1]), y = "Density") +
                          ggtitle("URBAN INCOME") +
                          geom_vline(xintercept = 0, col = "black") + theme_bw()
                     
                        
                        
                        
                        ##########################################################################
                        # Anna's Recommended Model Parameters (Categorical education & popDensity)
                        ##########################################################################
                        
                        formula4 = rfrsttn_ch ~ 1 + edctn_s_cat + 
                          dnsty_pp_cat + agrfrst_sc + avg_incm_cat +
                          f(ID, model='bym', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        
                        result4= inla(formula4,
                                      family='binomial', # based on the distribution of your outcome, it could be 'poisson' 'binomial' or 'zero inflated poisson'
                                      # if the outcome is  a rate (then the total lnd pr should be  accounted )
                                      data=mydata, # myshp@data
                                      control.predictor = list(compute=TRUE),
                                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        summary(result4)  
                        Efxplot(list(result4))  
                        
                        
                        
                        ########## AN IDEA TO TAKE THIS FURTHER COULD BE TO BREAK DOWN THE INCOMES WHICH APPEAR MOST INFLUENTIAL INTO CATEGORIES TO IDENTIFY A POTENTIAL CUT-OFF AFTER WHICH POINT THE EFFECTS ARE SIGNIFICANT
                        
                        
                        
                        
      #INLA (bym2)   
                        
                        #bym2-9Vars
                        formula.bym29 = rfrsttn_ch ~ 1 + edctn_n_sc + edctn_s_sc + edctn_h_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          incm_b__sc + incm_nn_sc + f(ID, model='bym2', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        
                        result.bym29= inla(formula.bym29,
                                        family='binomial', # based on the distribution of your outcome, it could be 'poisson' 'binomial' or 'zero inflated poisson'
                                        # if the outcome is  a rate (then the total lnd pr should be  accounted )
                                        data=mydata, # myshp@data
                                        control.predictor = list(compute=TRUE),
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        summary(result.bym29)  
                        
                        
                        
                        
                        #bym5Vars
                        formula.bym25 = rfrsttn_ch ~ 1 + edctn_n_sc + 
                          dnsty_pp_sc + agrfrst_sc + incm_rrPLO_sc + incm_rbPLO_sc +
                          f(ID, model='bym2', scale.model=TRUE, graph='Brazil.graph', adjust.for.con.comp=TRUE)
                        
                        result.bym25= inla(formula.bym25,
                                      family='binomial', # based on the distribution of your outcome, it could be 'poisson' 'binomial' or 'zero inflated poisson'
                                      # if the outcome is  a rate (then the total lnd pr should be  accounted )
                                      data=mydata, # myshp@data
                                      control.predictor = list(compute=TRUE),
                                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
                        
                        summary(result.bym25)
                        
#                        plot_inla_residuals(result.bym25)