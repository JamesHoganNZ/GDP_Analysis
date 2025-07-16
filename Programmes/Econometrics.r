##
##    Programme:  Create_Analytical_Set.r
##
##    Objective:  This programme is a port from something I'd written at MBIE.  I'm
##                modularising it now because I don't have TRED, so must read in the
##                data separately from InfoShare.  The following spreadsheets have been
##                downloaded from InfoShare, opened in Excel, and saved as Excel spread-
##                sheets.  This programme reads them in and tidy's there structure.
##
##       Author:  James Hogan, NZIER, 13 November 2018
##
   rm(list=ls(all=TRUE))
   
   ##
   ## Load the source data frames
   ##
      load("Data_Intermediate/Capital_Input.rda")
      load("Data_Intermediate/CPI.rda")
      load("Data_Intermediate/General_Price.rda")
      load("Data_Intermediate/Gross_Output.rda")
      load("Data_Intermediate/Labour_Hours.rda")
      load("Data_Intermediate/Labour_Qty.rda")
      load("Data_Intermediate/PPI_Output.rda")
      load("Data_Intermediate/Unemployed.rda")
      load("Data_Intermediate/Analytical_Set.rda")

     ##
     ##  Take the logs
     ##
         Analytical_Set$Output  <- log(Analytical_Set$Output)
         Analytical_Set$Capital <- log(Analytical_Set$Capital)
         Analytical_Set$Labour  <- log(Analytical_Set$Labour)
         Analytical_Set$Year    <- year(Analytical_Set$TimePeriod)
      
     ##
     ##  Estimate the production function
     ##
     ##  
        ##
        ##  First, the baseline:  bog standard OLS
        ##
            OLS <- lm(Output ~ Year + Labour + Capital, data=Analytical_Set)
            summary(OLS)
            plot(OLS)
         
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
               
               ocus <- efp(Output ~ Year + Labour + Capital, type = "OLS-CUSUM", 
                           data = Analytical_Set)
               bound.ocus <- boundary(ocus, alpha = 0.05)
               plot(ocus)
               
               bp.inf <- breakpoints(Output ~ Year + Labour + Capital, 
                           data =Analytical_Set)
               summary(bp.inf)
               confint(bp.inf)
               
          ##
          ##    The specific dates are...
          ##
            Analytical_Set$TimePeriod[confint(bp.inf)[[1]][,2]]
               

          ##
          ##  Everything's wrong with it - Autocorrelation / Hetroskedasticity / Structural Breaks!
          ##
          
          ##
          ##  Lets fix up the autocorrelation
          ##
            GLS_OLS <- gls(Output ~ Year + Labour + Capital + variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ as.numeric(variable)),
                           correlation = corAR1(form = ~ TimePeriod + variable))   
            summary(GLS_OLS)
            anova(OLS, GLS_OLS)
            
            acf(GLS_OLS$residuals)
            pacf(GLS_OLS$residuals)
          
        ##
        ##  Secondly, a dummy variable model capturing the industry variantion
        ##
            Dummy_OLS <- gls(Output ~ (Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ as.numeric(variable)),
                           correlation = corAR1(form = ~ TimePeriod + variable))  
            summary(Dummy_OLS)              
            ##
            ##    Do the tests
            ##
            acf(Dummy_OLS$residuals)
            pacf(Dummy_OLS$residuals)
            dwtest(Dummy_OLS)

            bptest(Dummy_OLS)           
            sctest(Dummy_OLS)
            reset(Dummy_OLS)
            
            ocus <- efp(Output ~ (Year + Labour + Capital)*variable, type = "OLS-CUSUM", 
                        data = Analytical_Set)
            bound.ocus <- boundary(ocus, alpha = 0.05)
            plot(ocus)
            
            bp.inf <- breakpoints(Output ~ (Year + Labour + Capital)*variable, 
                        data =Analytical_Set)
            summary(bp.inf)
            confint(bp.inf)
            
            Actual_Expected <- data.frame(TimePeriod = as.Date(Analytical_Set$TimePeriod,"%Y-%m-%d"),
                                          Industry   = str_replace_all(Analytical_Set$variable, "\\.", " "),
                                          Actual_Gross_Output = as.numeric(exp(Analytical_Set$Output)),
                                          Estimates = as.numeric(exp(GLS_OLS$fitted)))          
                               
            Actual_Expected <- melt(Actual_Expected,
                                    id.vars = c("TimePeriod", "Industry"),
                                    measure.vars = c("Actual_Gross_Output", "Estimates"))
                         
            ggplot(Actual_Expected, aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.7) +
                   geom_point(size =.5) +
                   labs(title="New Zealand Production\nCobb-Douglas Function, Estimated with Multi-Level Model\n") +
                   ylab("Gross Output\n$(Mill)") +
                   theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                         strip.text  = element_text(angle=00, vjust=0.5, size=8),
                         legend.position="right")+
                   facet_grid(~Industry, scales="free")            
            

         Output_Gap <- with(Actual_Expected,
                         aggregate(list(value = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = variable),
                                   sum, 
                                   na.rm = FALSE)
                                   )
         Output_Gap <- dcast(Output_Gap,
                             TimePeriod ~ variable)
         Output_Gap$Output_Gap <- with(Output_Gap, ((Actual_Gross_Output / Estimates)-1)*100)
         
        ##
        ##     Grab some Unemployment and Inflation measures
        ##
         Output_Gap <- merge(Output_Gap,
                             Unemployed[((Unemployed$Age == "Total All Ages") &
                                         (Unemployed$Gender == "Total Both Sexes") &
                                         (Unemployed$Measure == "Unemployment Rate") 
                                         ),],
                             by = c("TimePeriod"))
         names(Output_Gap)[names(Output_Gap) == 'value'] = "Unemployment_Rate"
         ##
         ##    Quite a clear cyclical pattern showing up when you look at the points of a 
         ##       scattergraph, by year
         ##
         ggplot(Output_Gap, aes(x=Output_Gap/100, y=Unemployment_Rate/100))     +
                geom_smooth(method=lm) +
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = "green") +
                geom_point(size = 1, colour = "blue") +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=2, nudge_x = 0.0025) +
                scale_x_continuous(labels = percent) +
                scale_y_continuous(labels = percent) +                
                ylab("Unemployment Rate\n") +
                xlab("Economic Production Output Gap\n(Actual Output / Expected Output)") +                
                labs(title="The New Zealand Business Cycle\n")  +         
                theme(axis.title.y = element_text(angle=90, vjust=0.5, size=8),
                      axis.title.x = element_text(angle=00, vjust=0.5, size=8),
                      axis.text.x  = element_text(angle=00, vjust=0.5, size=6),
                      axis.text.y  = element_text(angle=00, vjust=0.5, size=6),
                      strip.text   = element_text(angle=00, vjust=0.5, size=5),
                      legend.title = element_text(angle=00, vjust=0.5, size=5),
                      legend.text  = element_text(angle=00, vjust=0.5, size=5),
                      #plot.background= element_rect(fill="red"),
                      #legend.margin  = unit(-0.6,"cm"),
                      plot.margin=unit(c(1,1,1,1),"mm"),
                      legend.position  = "bottom") 
                                  
            
            
            
            
            
            
            
            
            
            
        ##
        ##  Third, a system of equations
        ##
            Analytical_Set <- pdata.frame(Analytical_Set, index = c("variable", "TimePeriod"))
            Model <- Output ~ Year + Labour + Capital
            
            System_Of_Equations <- systemfit(Model, "SUR",
                                             data = Analytical_Set)
            summary(System_Of_Equations)
         
        ##
        ##  Third, As a mixed-multilevel model.  This model assumes random coefficients that
        ##     vary by industry.  
        ##
            ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (-1 + Capital + Labour ) |variable)
                                        
            summary(ML_Production_LabCap)
            random.effects(ML_Production_LabCap)            
            ##
            ##    Re-estimate with just labour
            ##
            ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                            data = Analytical_Set, 
                            random = ~ (-1 + Labour ) |variable)
                            
            summary(ML_Production_LabCap)
            random.effects(ML_Production_LabCap)    
            
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Analytical_Set$TimePeriod,"%Y-%m-%d"),
                                          Industry   = str_replace_all(Analytical_Set$variable, "\\.", " "),
                                          Actual_Gross_Output = as.numeric(exp(Analytical_Set$Output)),
                                          Estimates = as.numeric(exp(ML_Production_LabCap$fitted[,2])))          
                               
            Actual_Expected <- melt(Actual_Expected,
                                    id.vars = c("TimePeriod", "Industry"),
                                    measure.vars = c("Actual_Gross_Output", "Estimates"))
                         
            ggplot(Actual_Expected, aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.7) +
                   geom_point(size =.5) +
                   labs(title="New Zealand Production\nCobb-Douglas Function, Estimated with Multi-Level Model\n") +
                   ylab("Gross Output\n$(Mill)") +
                   theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                         strip.text  = element_text(angle=00, vjust=0.5, size=8),
                         legend.position="right")+
                   facet_grid(~Industry, scales="free")
            ggsave("New Zealand Production - Cobb Douglas2017.png",width = 16.5, height = 11.7, dpi=600) 
        ##
        ##    Extensions:  What's the relationship between the output gap and unemployment:  Okun's Law
        ##
         Output_Gap <- with(Actual_Expected,
                         aggregate(list(value = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = variable),
                                   sum, 
                                   na.rm = FALSE)
                                   )
         Output_Gap <- dcast(Output_Gap,
                             TimePeriod ~ variable)
         Output_Gap$Output_Gap <- with(Output_Gap, ((Actual_Gross_Output / Estimates)-1)*100)
         
        ##
        ##     Grab some Unemployment and Inflation measures
        ##
         Output_Gap <- merge(Output_Gap,
                             Unemployed[((Unemployed$Age == "Total All Ages") &
                                         (Unemployed$Gender == "Total Both Sexes") &
                                         (Unemployed$Measure == "Unemployment Rate") 
                                         ),],
                             by = c("TimePeriod"))
         names(Output_Gap)[names(Output_Gap) == 'value'] = "Unemployment_Rate"
         ##
         ##    Quite a clear cyclical pattern showing up when you look at the points of a 
         ##       scattergraph, by year
         ##
         ggplot(Output_Gap, aes(x=Output_Gap/100, y=Unemployment_Rate/100))     +
                geom_smooth(method=lm) +
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = "green") +
                geom_point(size = 1, colour = "blue") +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=2, nudge_x = 0.001) +
                scale_x_continuous(labels = percent) +
                scale_y_continuous(labels = percent) +                
                ylab("Unemployment Rate\n") +
                xlab("Economic Production Output Gap\n(Actual Output / Expected Output)") +                
                labs(title="The New Zealand Business Cycle\n")  +         
                theme(axis.title.y = element_text(angle=90, vjust=0.5, size=8),
                      axis.title.x = element_text(angle=00, vjust=0.5, size=8),
                      axis.text.x  = element_text(angle=00, vjust=0.5, size=6),
                      axis.text.y  = element_text(angle=00, vjust=0.5, size=6),
                      strip.text   = element_text(angle=00, vjust=0.5, size=5),
                      legend.title = element_text(angle=00, vjust=0.5, size=5),
                      legend.text  = element_text(angle=00, vjust=0.5, size=5),
                      #plot.background= element_rect(fill="red"),
                      #legend.margin  = unit(-0.6,"cm"),
                      plot.margin=unit(c(1,1,1,1),"mm"),
                      legend.position  = "bottom") 
                      
         ggsave("Okuns Law.png",units = "cm", width = 18.65, height = 12.44, dpi=600) 

        ##
        ##     Regress it...
        ##
         OLS_Unemployment <- lm(Unemployment_Rate ~ Output_Gap, data=Output_Gap)
         summary(OLS_Unemployment)

         
        ##     From Peter: 
        ##     You should use gls from nlme instead of lm so you can add an ar(1) process to the random part.  
        ##        Your standard errors will currently be deflated if you're going what I think you are,
        ##

         GLS_Unemployment <- gls(Unemployment_Rate ~ Output_Gap, 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_Unemployment)
         
          ##
          ##     Which Prices?  Implicit_Price_Deflator
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
         Output_Gap <- merge(Output_Gap,
                             General_Price[General_Price$Measure == "Gross Domestic Product - expenditure measure", 
                                           c("TimePeriod", "Inflation", "Change_in_Inflation")] ,
                             by = c("TimePeriod"))
                             
         GLS_UnemploymentPrices <- gls(Unemployment_Rate ~ Inflation, 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
                                  
         summary(GLS_UnemploymentPrices)   
         
         GLS_UnemploymentPrices <- gls(Unemployment_Rate ~ Change_in_Inflation, 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
                                  
         summary(GLS_UnemploymentPrices)                             
          ##
          ##     Which Prices?  CPI and IPD
          ##         Nope.  No evidence - WHAT SO EVERR!!! - that the output gap manifests in inflation
          ##
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
                             CPI[, c("TimePeriod", "CPIInflation", "Change_in_CPIInflation")] ,
                             by = c("TimePeriod"))
         OLS_IPD <- lm(Inflation    ~ Output_Gap, data=Output_Gap)
         OLS_CPI <- lm(CPIInflation ~ Output_Gap, data=Output_Gap)
         summary(OLS_IPD)
         summary(OLS_CPI)

         OLS_IPD <- lm(Change_in_Inflation    ~ Output_Gap, data=Output_Gap)
         OLS_CPI <- lm(Change_in_CPIInflation ~ Output_Gap, data=Output_Gap)
         summary(OLS_IPD)
         summary(OLS_CPI)
         
         GLS_UnemploymentPrices <- gls(Change_in_CPIInflation ~ Output_Gap, 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
                                  
         summary(GLS_UnemploymentPrices)              

        ##
        ##     By GLS
        ##
        
         GLS_IPD <- gls(Change_in_Inflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
        
         GLS_IPD <- gls(Change_in_CPIInflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
                 
         GLS_IPD <- gls(Inflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
        
         GLS_IPD <- gls(CPIInflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
         OLS_IPD <- lm(Change_in_Inflation    ~ Output_Gap, data=Output_Gap)
         OLS_CPI <- lm(Change_in_CPIInflation ~ Output_Gap, data=Output_Gap)
         summary(OLS_IPD)
         summary(OLS_CPI)

        ##
        ##     By GLS
        ##
        
         GLS_IPD <- gls(Change_in_Inflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
        
         GLS_IPD <- gls(Change_in_CPIInflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
                 
         GLS_IPD <- gls(Inflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
        
         GLS_IPD <- gls(CPIInflation    ~ Output_Gap, 
                        data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                        correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_IPD)
