##
##    Programme:  Econometrics_PhillipsCurve.r
##
##
##       Author:  James Hogan, NZIER, 15 November 2018
##
   rm(list=ls(all=TRUE))
   
   ##
   ## Load the source data frames
   ##
      load("Final_Output/Actual_Expected.rda")
      load("Data_Intermediate/CPI.rda")
      load("Data_Intermediate/General_Price.rda")
      load("Final_Output/Output_Gap.rda")
  
         
    ##
    ##     Which Prices?  Implicit_Price_Deflator and the CPI
    ##
      General_Price$Inflation <- NA
      General_Price$Change_in_Inflation <- NA
      for(i in 2:nrow(General_Price))
         {  
            General_Price$Inflation[i] <- ((General_Price$value[i] / General_Price$value[(i-1)])-1)*100
         }
      for(i in 2:nrow(General_Price))
         {  
            General_Price$Change_in_Inflation[i] <- (General_Price$Inflation[i] - General_Price$Inflation[(i-1)])
         }
         
      CPI$CPIInflation <- NA
      CPI$Change_in_CPIInflation <- NA
      for(i in 2:nrow(CPI))
         {  
            CPI$CPIInflation[i] <- ((CPI$value[i] / CPI$value[(i-1)])-1)*100
         }
      for(i in 2:nrow(CPI))
         {  
            CPI$Change_in_CPIInflation[i] <- (CPI$CPIInflation[i] - CPI$CPIInflation[(i-1)])
         }         
         
      Output_Gap <- merge(Output_Gap,
                          General_Price[General_Price$Measure == "Gross Domestic Product - expenditure measure", 
                                        c("TimePeriod", "Inflation", "Change_in_Inflation")] ,
                          by = c("TimePeriod"))
      Output_Gap <- merge(Output_Gap,
                          CPI ,
                          by = c("TimePeriod"))
                          
                          
    ##
    ##     Is there an Unemployment_Rate / Inflation trade off? - yes
    ##
      GLS_UnemploymentPrices <- gls(Unemployment_Rate ~ Inflation, 
                                   data=Output_Gap,
                                   correlation = corAR1(form = ~ TimePeriod))
                               
      summary(GLS_UnemploymentPrices)   
      
      GLS_UnemploymentPrices <- gls(Unemployment_Rate ~ Change_in_Inflation, 
                               data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                               correlation = corAR1(form = ~ TimePeriod))
                               
      summary(GLS_UnemploymentPrices)                             
    ##
    ##     Is there an Output Gap / General Price Inflation trade off? - no.
    ##
      GLS_Output_GapPrices <- gls(Inflation ~ Output_Gap, 
                                   data=Output_Gap,
                                   correlation = corAR1(form = ~ TimePeriod))
                               
      summary(GLS_Output_GapPrices)   
       ##
       ##     Is there an Output Gap / Accelerating Inflation ? - no.
       ##
      
      GLS_Output_GapAccelPrices <- gls(Change_in_Inflation ~ Output_Gap, 
                                   data=Output_Gap,
                                   correlation = corAR1(form = ~ TimePeriod))
      summary(GLS_Output_GapAccelPrices)                             
      
    ##
    ##     Is there an Output Gap / CPI Price Inflation trade off? - no.
    ##
         
      GLS_CPIPrices <- gls(CPIInflation ~ Output_Gap, 
                                   data=Output_Gap,
                                   correlation = corAR1(form = ~ TimePeriod))
                               
      summary(GLS_CPIPrices)              
       ##
       ##     Is there an Output Gap / Accelerating CPI Price Inflation trade off? - YES!
       ##
     
      GLS_CPIAccelPrices <- gls(Change_in_CPIInflation    ~ Output_Gap, 
                                data=Output_Gap,
                                correlation = corAR1(form = ~ TimePeriod))
      summary(GLS_CPIAccelPrices)

      OLS <- lm(Change_in_CPIInflation    ~ Output_Gap, 
                data=Output_Gap)
      summary(OLS)
      
 
     ##
     ##  Test for Autocorrelation:  Extract the residuals and check out their autocorrelation function
     ##        Autocorrelation in errors, looks like a AR(1) process
     ##
     ##     Autocorrelation underestimates the true variance of the estimates:  t-values are overstated 
     ##
         acf(OLS$residuals)
         pacf(OLS$residuals)
         dwtest(OLS)

     ##
     ##  Test for Hetroskedasticity:  Breusch-Pagan test.  Yep, heaps of hetroskedasticity
     ##     Hetroskedasticity overestimates the true variance of the estimates:  t-values are understated 
     ##
         bptest(OLS)           
     
     ##
     ##  Test for misspecification: Ramsey Reset test and test of structural break
     ##
         sctest(OLS)
         reset(OLS)
         
         ocus <- efp(Change_in_CPIInflation    ~ Output_Gap, type = "OLS-CUSUM", 
                     data = Output_Gap)
         bound.ocus <- boundary(ocus, alpha = 0.05)
         plot(ocus)
         
         bp.inf <- breakpoints(Change_in_CPIInflation    ~ Output_Gap, 
                     data =Output_Gap)
         summary(bp.inf)
         confint(bp.inf)
          ##
          ##    The specific dates are...
          ##
           Output_Gap$TimePeriod[confint(bp.inf)[[1]][,2]]               

    ##
    ##     Lets see how it worked
    ##
      Actual_Expected <- data.frame(TimePeriod = as.Date(Output_Gap$TimePeriod,"%Y-%m-%d"),
                                    Change_in_CPIInflation = as.numeric(Output_Gap$Change_in_CPIInflation),
                                    Estimated_Change_in_CPIInflation = as.numeric(OLS$fitted))
                         
      Actual_Expected <- melt(Actual_Expected,
                              id.vars = c("TimePeriod"),
                              measure.vars = c("Change_in_CPIInflation", "Estimated_Change_in_CPIInflation"))
                   
      ggplot(Actual_Expected, aes(x=TimePeriod, y=value, colour=variable))     +
             geom_line(size =.7) +
             geom_point(size =.5) +
             labs(title="Change in CPI Inflation - Estimated Change in CPI Inflation\n") +
             theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                   strip.text  = element_text(angle=00, vjust=0.5, size=8),
                   legend.position="right")
