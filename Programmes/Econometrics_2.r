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
            OLS <- lm(Output ~ (-1 + Year + Labour + Capital)*variable, data=Analytical_Set)
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
               
               ocus <- efp(Output ~ (-1 + Year + Labour + Capital)*variable, type = "OLS-CUSUM", 
                           data = Analytical_Set)
               bound.ocus <- boundary(ocus, alpha = 0.05)
               plot(ocus)
               
               bp.inf <- breakpoints(Output ~ (Year + Labour + Capital)*variable, 
                           data =Analytical_Set)
               summary(bp.inf)
               confint(bp.inf)
               
          ##
          ##  Almost everything is wrong:  Autocorrelation, Hetroskedasticity, but no structural Breaks.
          ##      Evidence of misspecification from reset test
          ##
          
          ##
          ##  Lets fix up the autocorrelation.  Lets say autocorrelation relates to time and industries.
          ##      but is it time by industry, or time AND industry?
          ##
            GLS_OLS_1 <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           correlation = corAR1(form = ~ TimePeriod | variable))   
            summary(GLS_OLS_1)

            GLS_OLS_2 <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           correlation = corAR1(form = ~ TimePeriod + variable))   
            summary(GLS_OLS_2)
            
            anova(OLS, GLS_OLS_1)
            anova(OLS, GLS_OLS_2)
            anova(GLS_OLS_1, GLS_OLS_2)
            
          ##
          ##   Log of the likelihood function suggests autocorrelation relates to Time AND Industry
          ##
            
          ##
          ##   Test hetroskedasticity relating to ... industry?  Labour?  Or Capital? Or Year?
          ##
            GLS_OLS_Industry <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ as.numeric(variable)),
                           correlation = corAR1(form = ~ TimePeriod + variable))   
                           
            GLS_OLS_Labour <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ Labour),
                           correlation = corAR1(form = ~ TimePeriod + variable))   
                           
            GLS_OLS_Capital <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ Labour),
                           correlation = corAR1(form = ~ TimePeriod + variable))   
                           
            GLS_OLS_Year <- gls(Output ~ (-1 + Year + Labour + Capital)*variable, 
                           data=Analytical_Set,
                           weights = varFunc(~ Year),
                           correlation = corAR1(form = ~ TimePeriod + variable))   
                           
            anova(GLS_OLS_2, GLS_OLS_Industry)
            anova(GLS_OLS_2, GLS_OLS_Labour)
            anova(GLS_OLS_2, GLS_OLS_Capital)
            anova(GLS_OLS_2, GLS_OLS_Year)
            
          ##
          ##   Hetroskedasticity isn't much - lets ignore :)  Stick with GLS_OLS_2
          ##

            summary(GLS_OLS_2)
            
            acf(GLS_OLS_2$residuals)
            pacf(GLS_OLS_2$residuals)
            
            
        ##
        ##  Third, lets try a system of equations.  The interactions between industry and time are
        ##     captured in the system
        ##
            Analytical_Set <- pdata.frame(Analytical_Set, index = c("variable", "TimePeriod"))
            Model <- Output ~ Year + Labour + Capital
            
            System_Of_Equations <- systemfit(Model, "SUR",
                                             data = Analytical_Set)
            summary(System_Of_Equations)
            
            
        ##
        ##  Finally, lets try a mixed-multilevel model.  This model assumes random coefficients that
        ##     vary by industry.  
        ##
            
            Multi_Level_Int <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ 1 | variable)
                                        
            Multi_Level_Int_Year <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Year) | variable)
                                        
            Multi_Level_Int_Labour <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Labour) | variable)
                                        
            Multi_Level_Int_Capital <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Capital) | variable)

            Multi_Level_Int_Year_Cap  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Year + Capital) | variable)
                                        
            Multi_Level_Labour  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (Labour) | variable)
                                        
            Multi_Level_Capital  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (Capital) | variable)

            Multi_Level_CapLab  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Capital + Labour) | variable)
                                        
            anova(Multi_Level_Int, Multi_Level_Int_Year)
            anova(Multi_Level_Int, Multi_Level_Int_Labour)
            anova(Multi_Level_Int, Multi_Level_Int_Capital)
            anova(Multi_Level_Int, Multi_Level_Int_Year_Cap)
            anova(Multi_Level_Int_Capital, Multi_Level_Int_Year_Cap)
            anova(Multi_Level_Int_Capital, Multi_Level_Int_Year_Cap)
            anova(Multi_Level_Labour, Multi_Level_Capital)
            anova(Multi_Level_Int_Capital, Multi_Level_CapLab)

                              
        ##
        ##  Of the ANOVA tests, the Multi_Level_Int_Capital model is the tested preference.
        ##
            summary(Multi_Level_Int_Capital)
            random.effects(Multi_Level_Int_Capital)                       
            
        ##
        ##  Clean up the Autocorrelation? - yep
        ##
            Multi_Level_1 <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Capital) | variable)

            Multi_Level_2 <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        correlation = corAR1(0,form = ~ Year | variable),
                                        random = ~ (1 + Capital) | variable)
            anova(Multi_Level_1, Multi_Level_2)

            summary( Multi_Level_2)
            random.effects( Multi_Level_2)        
            
            
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Analytical_Set$TimePeriod,"%Y-%m-%d"),
                                          Industry   = str_replace_all(Analytical_Set$variable, "\\.", " "),
                                          Actual_Gross_Output = as.numeric(exp(Analytical_Set$Output)),
                                          Estimates = as.numeric(exp(Multi_Level_2$fitted[,2])))          
                               
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
