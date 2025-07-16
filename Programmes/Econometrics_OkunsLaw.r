##
##    Programme:  Econometrics_OkunsLaw.r
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
   source("R/themes.r")
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
            
            GLS_OLS_Estimates <- data.frame(TimePeriod = as.Date(Analytical_Set$TimePeriod,"%Y-%m-%d"),
                                            Industry   = str_replace_all(Analytical_Set$variable, "\\.", " "),
                                            Estimates_GLS = as.numeric(exp(GLS_OLS_2$fitted)))
            
        ##
        ##  Third, lets try a system of equations.  The interactions between industry and time are
        ##     captured in the system
        ##
            Analytical_Set <- pdata.frame(Analytical_Set, index = c("variable", "TimePeriod"))
            Model <- Output ~ Year + Labour + Capital
            
            System_Of_Equations <- systemfit(Model, "SUR",
                                             data = Analytical_Set,
                                             methodResidCov = "noDfCor",
                                             residCovWeighted = TRUE )
            summary(System_Of_Equations)
            
            SystemEstimates <- data.frame()
            for(i in 1:length(System_Of_Equations))
               {
                  SystemEstimates <- rbind(SystemEstimates,
                                           data.frame(Industry   = System_Of_Equations[[1]][[i]][["eqnLabel"]],
                                                      Something = System_Of_Equations[[1]][[i]][["fitted.values"]],
                                                      SystemEstimates  = as.numeric(exp(System_Of_Equations[[1]][[i]][["fitted.values"]]))))
               }
            SystemEstimates$TimePeriod = row.names(SystemEstimates)
            SystemEstimates$TimePeriod = as.Date(str_sub(SystemEstimates$TimePeriod, start = 2, end = 11), "%Y.%m.%d")
            SystemEstimates$Industry   = str_replace_all(SystemEstimates$Industry, "\\."," ")
            row.names(SystemEstimates) = NULL
            SystemEstimates <- SystemEstimates[,names(SystemEstimates) != "Something"]
            
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

            summary( Multi_Level_1)
            random.effects( Multi_Level_1)        
            
            
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Analytical_Set$TimePeriod,"%Y-%m-%d"),
                                          Industry   = str_replace_all(Analytical_Set$variable, "\\.", " "),
                                          Actual_Gross_Output = as.numeric(exp(Analytical_Set$Output)),
                                          Estimates_MultiLevel = as.numeric(exp(Multi_Level_1$fitted[,2])))
            Actual_Expected <- merge(Actual_Expected,
                                     GLS_OLS_Estimates,
                                     by = c("TimePeriod", "Industry"))
                                          
            Actual_Expected <- merge(Actual_Expected,
                                     SystemEstimates,
                                     by = c("TimePeriod", "Industry"))
                               
            Actual_Expected <- melt(Actual_Expected,
                                    id.vars = c("TimePeriod", "Industry"),
                                    measure.vars = c("Actual_Gross_Output", "Estimates_MultiLevel", "Estimates_GLS", "SystemEstimates"))
                         
            ##
            ##    Fix up the industry names
            ##
            Wrap_Industry <- data.frame(Industry = c("Arts, Recreation and Other Services",
                                                     "Construction",
                                                     "Electricity, Gas, Water and Waste Services",
                                                     "Financial and Insurance Services",
                                                     "Forestry and Mining",
                                                     "Information Media and Telecommunications",
                                                     "Manufacturing",
                                                     "Professional, Scientific, Technical, Administrative and Support Services",
                                                     "Rental, Hiring and Real Estate Services",
                                                     "Retail Trade and Accommodation",
                                                     "Transport, Postal and Warehousing",
                                                     "Wholesale Trade"),
                                    NZIndustry = c("Arts, Recreation\nand Other Services",
                                                 "Construction",
                                                 "Electricity, Gas, Water\nand Waste Services",
                                                 "Financial and\nInsurance Services",
                                                 "Forestry and Mining",
                                                 "Information Media\nand Telecommunications",
                                                 "Manufacturing",
                                                 "Professional, Scientific,\nTechnical, Administrative\nand Support Services",
                                                 "Rental, Hiring and\nReal Estate Services",
                                                 "Retail Trade\nand Accommodation",
                                                 "Transport, Postal\nand Warehousing",
                                                 "Wholesale Trade"), stringsAsFactors = FALSE)
             
               Actual_Expected <- merge(Actual_Expected,
                                        Wrap_Industry,
                                        by = c("Industry"),
                                        all = TRUE)
            ggplot(Actual_Expected[Actual_Expected$variable %in% c("Actual_Gross_Output", "SystemEstimates"),], aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.5) +
                   geom_point(size =.7) +
                   labs(title="New Zealand Production\nGross Output Measure - Actual and Estimated\n") +
                   ylab("Gross Output\n$(Mill)") +
                   scale_colour_manual(values = c("#7b1244","#0094c5"), name="Actual or Expected") +
                   xlab("Time Period\n") +
                   facet_grid(~NZIndustry, scales="free") +
                   theme_bw(base_size=10, base_family = "Gustan-Book") %+replace%
                   theme(legend.title.align=0.5,
                      plot.margin = unit(c(1,3,1,1),"mm"),
                      legend.text  = element_text(size=12),
                      axis.text.x  = element_text(angle=90, size=8),
                      axis.text.y  = element_text(angle=00, size=8),
                      axis.title.y  = element_text(angle=90, size=7.5),
                      strip.text  = element_text(size=6),
                      plot.title = element_text(size = 12),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(0, "cm"),
                      legend.margin = margin(0, 0, 0, 0),
                      legend.position  = "bottom")   
            ggsave("Final_Output/New Zealand Production.png",units = "cm",width = 29.7, height = 21, dpi=600) 
        ##
        ##    Which Industries are cooking and which are uncooking?
        ##
            Industry_Measures <- dcast(Actual_Expected[Actual_Expected$variable %in% c("Actual_Gross_Output", "SystemEstimates"),],
                                       Industry + TimePeriod + NZIndustry ~ variable,
                                       value.var = c("value"))
            Industry_Measures$Output_Gap <- with(Industry_Measures, ((Actual_Gross_Output / SystemEstimates)-1)*100)
        ##
        ##    Reorder the Employee Sizes
        ##
            Industry_Measures <- Industry_Measures[year(Industry_Measures$TimePeriod) >= (year(max(Industry_Measures$TimePeriod))-1),]      
            reorder_dset <- Industry_Measures[Industry_Measures$TimePeriod == max(Industry_Measures$TimePeriod),]
            
            Industry_Measures$NZIndustry <- factor(Industry_Measures$NZIndustry, 
                                                  levels=reorder_dset$NZIndustry[order(reorder_dset$Output_Gap, decreasing = TRUE)]) 
            Industry_Measures$Year <- as.character(year(Industry_Measures$TimePeriod))
                       
                          
        png("Final_Output/Output Gap By Industry.png", w = 15.7, h = 8.3, res = 600, units = "in")
         grid.newpage()
          pushViewport(viewport(layout=grid.layout(nrow = 1,
                                                   ncol = 1)))
           p1 <- ggplot(Industry_Measures, aes(x=NZIndustry, y=Output_Gap, fill=Year)) +
                  geom_bar(stat="identity", position=position_dodge()) + 
                  coord_flip() +
                  scale_colour_manual(aesthetics = c("colour", "fill"), values = nzier.cols(), name="") +
                  labs(x = "", fill = "Industries\n")+
                  labs(y = "\nDifference between Actual and Expected Output\n(Given Capital Stock and Employed Labour)\n") +
                  scale_y_continuous(breaks=seq(-20,20,1), labels=paste0(seq(-20,20,1),"%")) +
                  geom_hline(yintercept=0, color="white", size=1) +
                  theme(axis.text.x = element_text(angle=00)) +
                  geom_text(x=7.5, y=-10, size=6, label="Under\nPerforming\nIndustries", colour = nzier.cols("Brown")) +
                  geom_text(x=7.5, y= 5, size=6, label="Over\nPerforming\nIndustries",  colour = nzier.cols("Brown")) +
                  labs(title = "Output Gap - by Industry", 
                       subtitle = "New Zealand Institute of Economic Research\n") +
                  theme_NZIER3()
         print(p1, vp=vplayout(1,1))
         dev.off() 


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
         Output_Gap$Output_Gap <- with(Output_Gap, ((Actual_Gross_Output / SystemEstimates)-1)*100)
         
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
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = c("#7b1244")) +
                geom_point(size = 1.5, colour = c("#0094c5")) +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=3, nudge_x = 0.001) +
             #   scale_x_continuous(labels = percent) +
             #   scale_y_continuous(labels = percent) +                
                ylab("Unemployment Rate\n") +
                xlab("Economic Production Output Gap\n(Actual Output / Expected Output)") +                
                labs(title="The New Zealand Business Cycle\n")  +         
                theme_bw(base_size=10, base_family = "Gustan-Book") %+replace%
                theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   legend.text  = element_text(size=12),
                   axis.text.x  = element_text(angle=90, size=8),
                   axis.text.y  = element_text(angle=00, size=8),
                   axis.title.y  = element_text(angle=90, size=7.5),
                   strip.text  = element_text(size=6),
                   plot.title = element_text(size = 12),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(0, "cm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")   
                      
         ggsave("Final_Output/Okuns Law - System_of_Equations.png",units = "cm", width = 29.7, height = 21, dpi=600) 

  ##
  ##     Save the output sets
  ##
   save(Actual_Expected, file= "Final_Output/Actual_Expected.rda")
   save(Output_Gap, file= "Final_Output/Output_Gap.rda")
  
  
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
