##
##    Programme:  Money Demand Analytical Set.r
##
##    Objective:  
##
##
##    https://medium.com/@marc.jacobs012/cointegration-of-time-series-in-r-a6543dacf66e
##    https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
##
##
##
##
##
##
##
##
##    Author:     James Hogan, started 20 August 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions and colour palattes
   ##
      source("R/themes.r")

   ##
   ##    Load data from somewhere  
   ##
      load("Data_Output/Money_Demand_Analytical_Set.rda")

   ##
   ##    Restrict to Housing
   ##
      Money_Demand_Analytical_Set <- Money_Demand_Analytical_Set[Money_Demand_Analytical_Set$Industry == "Households - Housing",]

   ##
   ## tau3, phi2, and phi3 are trend, drift and none
   ##  significantly different from the critical value means that the data IS stationary 
   ##

      Unit_Root <-ur.df(Money_Demand_Analytical_Set$Real_Mortgages, 
                        lags = 24, 
                        selectlags = "AIC", 
                        type = "trend")      
      summary(Unit_Root)
      plot.ts(Unit_Root@res, ylab = "Residuals")
      abline(h = 0, col = "red")
      tsm::ac(Unit_Root@res)

      Unit_Root <-ur.df(Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate, 
                        lags = 24, 
                        selectlags = "AIC", 
                        type = "trend")      
      summary(Unit_Root)
      plot.ts(Unit_Root@res, ylab = "Residuals")
      abline(h = 0, col = "red")
      tsm::ac(Unit_Root@res)


      Unit_Root <-ur.df(Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked, 
                        lags = 24, 
                        selectlags = "AIC", 
                        type = "trend")      
      summary(Unit_Root)
      plot.ts(Unit_Root@res, ylab = "Residuals")
      abline(h = 0, col = "red")
      tsm::ac(Unit_Root@res)


      Unit_Root <-ur.df(Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices, 
                        lags = 24, 
                        selectlags = "AIC", 
                        type = "trend")      
      summary(Unit_Root)
      plot.ts(Unit_Root@res, ylab = "Residuals")
      abline(h = 0, col = "red")
      tsm::ac(Unit_Root@res)

      Unit_Root <-ur.df(Money_Demand_Analytical_Set$Log_GDP, 
                        lags = 24, 
                        selectlags = "AIC", 
                        type = "trend")      
      summary(Unit_Root)
      plot.ts(Unit_Root@res, ylab = "Residuals")
      abline(h = 0, col = "red")
      tsm::ac(Unit_Root@res)

##
##    Real_Mortgages is non stationary without trend, is lag 6
##    House_Mortgage_Interest_Rate is nonstationary without trend, is lag 7
##    Labour_Paid_Hours_Worked is nonstationary with trend, is lag 17
##    Log_Fisher_Ideal_House_Prices is nonstationary without trend, is lag 5
##
      Money_Demand_Analytical_Set

      VARselect(data.frame(Money_Demand_Analytical_Set$Real_Mortgages,
                           Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate,
                           Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices,
                           Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked), 
                lag.max = 17, 
                type = c("const"))    
      ##
      ##    Do some causality tests
      ##
      
        VAR_Model <- VAR(data.frame(Real_Mortgages = Money_Demand_Analytical_Set$Real_Mortgages,
                                    Log_Fisher_Ideal_House_Prices = Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices),
                                  p = 17,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Real_Mortgages")                # mortgage demand causes house prices to change.
        causality(VAR_Model,cause = "Log_Fisher_Ideal_House_Prices") # House prices absolutely cause mortgage demand to change.
      
      
        VAR_Model <- VAR(data.frame(Real_Mortgages = Money_Demand_Analytical_Set$Real_Mortgages,
                                    Labour_Paid_Hours_Worked = Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked),
                                  p = 17,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Real_Mortgages")            # Mortgage demand causes Labour_Paid_Hours_Worked to change??
        causality(VAR_Model,cause = "Labour_Paid_Hours_Worked")  # Labour_Paid_Hours_Worked also causes mortgage demand to change
      

        VAR_Model <- VAR(data.frame(Real_Mortgages = Money_Demand_Analytical_Set$Real_Mortgages,
                                    House_Mortgage_Interest_Rate = Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate),
                                  p = 17,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Real_Mortgages")            # Mortgage demand causes House_Mortgage_Interest_Rate to change??
        causality(VAR_Model,cause = "House_Mortgage_Interest_Rate")  # But House_Mortgage_Interest_Rate do not cause mortgage demand to change!


        VAR_Model <- VAR(data.frame(Log_Fisher_Ideal_House_Prices = Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices,
                                    House_Mortgage_Interest_Rate  = Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate),
                                  p = 17,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Log_Fisher_Ideal_House_Prices") # House prices do not cause House_Mortgage_Interest_Rate to change
        causality(VAR_Model,cause = "House_Mortgage_Interest_Rate")  # But House_Mortgage_Interest_Rate do cause house prices to change
      
      
   ##
   ##    Derive the speed of adjustment, and test its significance
   ##       Taken from here: https://stats.stackexchange.com/questions/96645/finding-significance-levels-for-cointegrating-coefficients-in-cajorls
   ##
   ##    The coefficients on ECT1 are the speeds of adjustment of the regression variable to disequilibrium in the long run position.
   ##
      
      jotest=ca.jo(data.frame(Money_Demand_Analytical_Set$Real_Mortgages,
                              Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate,
                              Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked,
                              Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices), 
                              type  = "trace", 
                              K     = 8, 
                              ecdet = "trend", 
                              spec  = "longrun")
      summary(jotest)

      vecm <- cajorls(jotest,r=1)
      coeftest(vecm$rlm)
      coef(summary(vecm$rlm))

      dynamic_bit <- alphaols(jotest)
      summary(dynamic_bit)
       
      cajo_beta_create <- function(cajo_o, cajorls_o) {
            alfa <- coef(cajorls_o$rlm)[1, ]
            residuals <- resid(cajorls_o$rlm)
            N <- nrow(residuals)
            sigma <- crossprod(residuals) / N
            beta <- cajorls_o$beta
            # standard errors
            beta.se <- sqrt(diag(kronecker(solve(crossprod(cajo_o@RK[, -1])), solve(t(alfa) %*% solve(sigma) %*% alfa))))
            beta.se2 <- c(NA, beta.se)
            beta.t <- c(NA, beta[-1] / beta.se)
            beta.pvalue <- dt(beta.t, df=cajorls_o$rlm$df.residual)     # p values

            tr <- createTexreg(coef.names = as.character(rownames(beta)), coef = (-1)*as.numeric(beta), se = beta.se2, pvalues=beta.pvalue,
            gof.names = c('Dummy'), gof=c(1), gof.decimal=c(FALSE))
            return(tr)
       }
      cajo_beta_create(jotest, vecm)
      ##
      ##    Long term model
      ##
      screenreg(cajo_beta_create(jotest, vecm))
      
##
##    Make a linear combination of the eigenvectors
##
   Coefs <- as.data.frame(jotest@V)
   Coefs$Model <- rownames(Coefs)
   rownames(Coefs) <- NULL

   Money_Demand_Analytical_Set$Estimate <- NA
   Money_Demand_Analytical_Set$Trend <- 1:nrow(Money_Demand_Analytical_Set)
   
   
   for(i in 1:nrow(Money_Demand_Analytical_Set))
   {
#      Money_Demand_Analytical_Set$Estimate[i] <-exp( -(Coefs[2,1] * Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate[i]) +
#                                                     -(Coefs[3,1] * Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked[i]) +
#                                                     -(Coefs[4,1] * Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices[i]) +
#                                                     -(Coefs[5,1] * 1))
      Money_Demand_Analytical_Set$Estimate[i] <-exp( -(Coefs[2,1] * Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate[i]) +
                                                     -(Coefs[3,1] * Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked[i]) +
                                                     -(Coefs[4,1] * Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices[i])+
                                                     -(Coefs[5,1] * Money_Demand_Analytical_Set$Trend[i]))
   }

   Plot_Me <- data.table::melt(Money_Demand_Analytical_Set,
                               id.var = "Period",
                               measure.vars = c("Real_Loans_By_Industry","Estimate"))
                               

      ggplot(Plot_Me, 
             aes(x = Period, 
                 y = value, 
                 colour=variable))     +
             geom_line(size =1) +
             geom_point(size =1) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~variable, scales="free") +
             labs(x = "\nTime Period", 
                  y = "%\n", 
                  title="Real Loans by Industry and \n",
                  caption = "Stats NZ QES: Total Paid Hours by Industry") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")                      



   Money_Demand_Analytical_Set$Nominal_Estimate <- Money_Demand_Analytical_Set$Estimate * Money_Demand_Analytical_Set$Fisher_Ideal_House_Prices
   
   Plot_Me <- data.table::melt(Money_Demand_Analytical_Set,
                               id.var = "Period",
                               measure.vars = c("Loans_By_Industry","Nominal_Estimate"))
                               

      ggplot(Plot_Me, 
             aes(x = Period, 
                 y = value, 
                 colour=variable))     +
             geom_line(size =1) +
             geom_point(size =1) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~variable, scales="free") +
             labs(x = "\nTime Period", 
                  y = "%\n", 
                  #title="Paid Hours Worked\n",
                  caption = "Stats NZ QES: Total Paid Hours by Industry") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")                      

##
##    Do we have short-term estimates from the Johansen method?
##

jotest <- cajolst(data.frame(Money_Demand_Analytical_Set$Real_Mortgages,
                             Money_Demand_Analytical_Set$House_Mortgage_Interest_Rate,
                             Money_Demand_Analytical_Set$Labour_Paid_Hours_Worked,
                             Money_Demand_Analytical_Set$Log_Fisher_Ideal_House_Prices), 
             trend=TRUE, 
             K=8)
        
summary(jotest)

        
        