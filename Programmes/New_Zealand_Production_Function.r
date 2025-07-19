##
##    Programme:  New_Zealand_Production_Function.r
##
##    Objective:  Now all of the InfoShare data has been read in, lets try estimating a production function
##                as an error correction model, based on constant price GDP, Capital and Labour.
##
##                There's a couple of issues: 
##                1. The Capital Stock measure is annual, so I'll take the spline to interpolate the quarters.
##                   For the last year, I'll extrapolate it out by GFKF.
##
##                2. The Quarterly Employment Survey labour measure is quarterly actuals, which will need seasonally
##                   adjusted to match with the SA GDP measure.
##
##                3. The industries are bound to not be on the same definition.
##
##                4. There's no gaurantee that the macro level function will look anything like the micro level
##                   industry functions, or share similar short run dynamics.
##
##    Author:     James Hogan, started 27 June 2025
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
      load("Data_Output/ConstantPrice_SA_Qtr_GDP_Published20250631.rda")
      load("Data_Output/ConstantPrice_Actual_Annual_CapitalStock_Published20250631.rda")
      load("Data_Output/ConstantPrice_Actual_Qtr_PaidHours_Published20250631.rda")
      load("Data_Output/Household_Labour_Force_Survey_Published20250631.rda")
      load("Data_Intermediate/Downloaded_Files.rda")                
   ##
   ## Step 1: Check out the Industries and move each data source to a common industry definition
   ##
      GDP        <- unique(ConstantPrice_SA_Qtr_GDP_Published20250631$Industry)
      CapStock   <- unique(ConstantPrice_Actual_Annual_CapitalStock_Published20250631$Industry)
      Labour     <- unique(ConstantPrice_Actual_Qtr_PaidHours_Published20250631$Industry)
      Unemployed <- Household_Labour_Force_Survey_Published20250631
      
      GDP[!(GDP %in% CapStock)] # Only the unallocated in GDP is different
      #t(t(CapStock))      # These were used to make the below mapping
      #t(t(Labour))
      Reclassify_Industry <- data.table(CapStock = c("Accommodation and Food Services","Administrative and Support Services","Agriculture","Arts and Recreation Services","Central Government Administration, Defence and Public Safety","Construction","Education and Training","Electricity, Gas, Water and Waste Services","Financial and Insurance Services","Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services","Food, Beverage and Tobacco Product Manufacturing","Forestry and Logging","Furniture and Other Manufacturing","Health Care and Social Assistance","Information Media and Telecommunications","Local Government Administration","Metal Product Manufacturing","Mining","Non-Metallic Mineral Product Manufacturing","Other Services","Owner-Occupied Property Operation (National Accounts Only)","Petroleum, Chemical, Polymer and Rubber Product Manufacturing","Printing","Professional, Scientific and Technical Services","Rental, Hiring and Real Estate Services","Retail Trade","Textile, Leather, Clothing and Footwear Manufacturing","Transport Equipment, Machinery and Equipment Manufacturing","Transport, Postal and Warehousing","Wholesale Trade","Wood and Paper Products Manufacturing"),
                                        Labour   = c("Accommodation and Food Services","Professional, Scientific, Technical, Administrative and Support Services","EXCLUDED FROM QES","Arts, Recreation and Other Services","Public Administration and Safety","Construction","Education and Training","Electricity, Gas, Water and Waste Services","Financial and Insurance Services","EXCLUDED FROM QES","Manufacturing","Forestry and Mining","Manufacturing","Health Care and Social Assistance","Information Media and Telecommunications","Public Administration and Safety","Manufacturing","Forestry and Mining","Manufacturing","Arts, Recreation and Other Services","EXCLUDED FROM QES","Manufacturing","Manufacturing","Professional, Scientific, Technical, Administrative and Support Services","Rental, Hiring and Real Estate Services","Retail Trade","Manufacturing","Manufacturing","Transport, Postal and Warehousing","Wholesale Trade","Manufacturing"))

      GDP <- merge(ConstantPrice_SA_Qtr_GDP_Published20250631,
                   Reclassify_Industry,
                   by.x = c("Industry"),
                   by.y = c("CapStock"))

      CapStock <- merge(ConstantPrice_Actual_Annual_CapitalStock_Published20250631,
                        Reclassify_Industry,
                        by.x = c("Industry"),
                        by.y = c("CapStock"))
                        
      Labour <- ConstantPrice_Actual_Qtr_PaidHours_Published20250631
      
      names(Labour)[names(Labour) == "Industry"] <- "Labour"

      GDP <- GDP[,
                 list(Value = sum(Value, na.rm = TRUE)),
                 by = list(Period, Labour)]
                 
      CapStock <- CapStock[,
                 list(Value = sum(Value, na.rm = TRUE)),
                 by = list(Period, Labour)]


   ##
   ## Step 2: Interpolate the annual capital stock into a quarterly measure. I'll use these as "seaonally adjusted"
   ##
   
      XList <- lapply(unique(CapStock$Labour), function(Labour) 
               {  BinVector <- CapStock$Labour == Labour
    
                  F_Data <- CapStock[BinVector,]
                  y = F_Data$Value
                  x = F_Data$Period
                  z = x + 1
                   ##
                   ##    Move time to a common basis
                   ##
                    int1 <- lubridate::interval(lubridate::ymd(z[1]),
                                                lubridate::ymd(z[length(z)]))
                    New_Dates <- lubridate::ymd(z[1]) + months(0:(int1 %/% months(1)))
                    New_Dates <- New_Dates - 1      
                    New_Dates <- New_Dates[month(New_Dates) %in% c(3,6,9,12)]
                   ##
                   ##    Interpolate the Population values
                   ##
                    X <- data.frame()
                    X = tryCatch({ Hate <- smooth.spline(x,y)
                                      Y <- data.frame(Period = New_Dates,
                                                      Value = predict(Hate, as.numeric(New_Dates))$y)
                                      Y$Labour <- as.character(Labour)
                                  return(Y)
                                }, warning = function(w) {
                             }, error = function(e) {
                             }, finally = {
                             })
                          return(X)
               })
         InterpCapStock <- data.table(do.call(rbind, XList))
         InterpCapStock$Value <- as.numeric(InterpCapStock$Value)
         plot(InterpCapStock$Period[InterpCapStock$Labour == "Accommodation and Food Services"], InterpCapStock$Value[InterpCapStock$Labour == "Accommodation and Food Services"])
         #plot(CapStock$Period[CapStock$Labour == "Accommodation and Food Services"], CapStock$Value[CapStock$Labour == "Accommodation and Food Services"])


   ##
   ## Step 3: Seasonally adjust the quarterly QES measure
   ##
      InterpCapStock[Labour == "Accommodation and Food Services",]
      Labour[Labour == "Accommodation and Food Services",]
      GDP[Labour == "Accommodation and Food Services",]

    
     ##
     ##  Merge everything together
     ##
         Together <- merge(GDP,
                           InterpCapStock,
                           by = c("Period", "Labour"))
         Analytical_Set <- merge(Together,
                                 Labour,
                                 by = c("Period", "Labour"),
                                 stringsAsFactors = TRUE)
         names(Analytical_Set) <- c("Period", "Industry", "GDP", "Capital_Stock", "Labour")                 

      
     ##
     ##  Take the logs
     ##
         Analytical_Set$Output  <- log(Analytical_Set$GDP)
         Analytical_Set$Capital <- log(Analytical_Set$Capital_Stock)
         Analytical_Set$Labour  <- log(Analytical_Set$Labour)
         Analytical_Set$Year    <- year(Analytical_Set$Period)
      
     ##
     ##  Estimate the production function
     ##
     ##  
        ##
        ##  First, the baseline:  bog standard OLS
        ##
            OLS <- lm(Output ~ (-1 + Year + Labour + Capital)*Industry, data=Analytical_Set)
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
               
               ocus <- efp(Output ~ (-1 + Year + Labour + Capital)*Industry, type = "OLS-CUSUM", 
                           data = Analytical_Set)
               bound.ocus <- boundary(ocus, alpha = 0.05)
               plot(ocus)
               
              # bp.inf <- breakpoints(Output ~ (Year + Labour + Capital)*Industry, 
                           data =Analytical_Set)
              # summary(bp.inf)
              # confint(bp.inf)
          ##
          ##  Almost everything is wrong:  Autocorrelation, Hetroskedasticity, and structural Breaks.
          ##      Evidence of misspecification from reset test
          ##
          
          ##
          ##  Lets fix up the autocorrelation.  Lets say autocorrelation relates to time and industries.
          ##      but is it time by industry, or time AND industry?
          ##
            GLS_OLS_1 <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
                           data=Analytical_Set,
                           correlation = corAR1(form = ~ Period | as.factor(Industry)))   
            summary(GLS_OLS_1)

         #   GLS_OLS_2 <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
         #                  data=Analytical_Set,
         #                  correlation = corAR1(form = ~ Period + as.factor(Industry)))   
         #   summary(GLS_OLS_2)
            
            anova(OLS, GLS_OLS_1)
            anova(OLS, GLS_OLS_2)
            anova(GLS_OLS_1, GLS_OLS_2)
     
          ##
          ##   Test hetroskedasticity relating to ... industry?  Labour?  Or Capital? Or Year?
          ##
            GLS_OLS_Industry <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
                           data=Analytical_Set,
                           weights = varFunc(~ as.numeric(as.factor(Industry))),
                           correlation = corAR1(form = ~ Period | as.factor(Industry)))   
                           
            GLS_OLS_Labour <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
                           data=Analytical_Set,
                           weights = varFunc(~ Labour),
                           correlation = corAR1(form = ~ Period | as.factor(Industry)))   
                           
            GLS_OLS_Capital <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
                           data=Analytical_Set,
                           weights = varFunc(~ Labour),
                           correlation = corAR1(form = ~ Period | as.factor(Industry)))   
                           
            GLS_OLS_Year <- gls(Output ~ (-1 + Year + Labour + Capital)*as.factor(Industry), 
                           data=Analytical_Set,
                           weights = varFunc(~ Year),
                           correlation = corAR1(form = ~ Period | as.factor(Industry)))   
                           
            anova(GLS_OLS_1, GLS_OLS_Industry)
            anova(GLS_OLS_1, GLS_OLS_Labour)
            anova(GLS_OLS_1, GLS_OLS_Capital)
            anova(GLS_OLS_1, GLS_OLS_Year)
            
         ##
          ##   Hetroskedasticity isn't much - lets ignore :)  Stick with GLS_OLS_2
          ##

            summary(GLS_OLS_1)
            
            acf(GLS_OLS_1$residuals)
            pacf(GLS_OLS_1$residuals)
            
            GLS_OLS_Estimates <- data.frame(TimePeriod = as.Date(Analytical_Set$Period,"%Y-%m-%d"),
                                            Industry   = str_replace_all(Analytical_Set$Industry, "\\.", " "),
                                            Estimates_GLS = as.numeric(exp(GLS_OLS_1$fitted)))
            
        ##
        ##  Third, lets try a system of equations.  The interactions between industry and time are
        ##     captured in the system
        ##
            Analytical_Set <- pdata.frame(Analytical_Set, index = c("Industry", "Period"))
            Model <- Output ~ Year + Labour + Capital
            
            System_Of_Equations <- systemfit(Model, "SUR",
                                             data = Analytical_Set,
                                             methodResidCov = "noDfCor",
                                             residCovWeighted = TRUE )
            summary(System_Of_Equations)
            
            SystemEstimates <- data.frame()
            for(i in 1:length(System_Of_Equations[[1]]))
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
                                        random = ~ 1 | Industry)
                                        
            Multi_Level_Int_Year <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Year) | Industry)
                                        
            Multi_Level_Int_Labour <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Labour) | Industry)
                                        
            Multi_Level_Int_Capital <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Capital) | Industry)

            Multi_Level_Int_Year_Cap  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Year + Capital) | Industry)
                                        
            Multi_Level_Labour  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (Labour) | Industry)
                                        
            Multi_Level_Capital  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (Capital) | Industry)

            Multi_Level_CapLab  <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        random = ~ (1 + Capital + Labour) | Industry)
                                        
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
                                        random = ~ (1 + Capital) | Industry)

            Multi_Level_2 <- lme(Output ~ Year + Labour + Capital,
                                        data = Analytical_Set, 
                                        correlation = corAR1(0,form = ~ Year | Industry),
                                        random = ~ (1 + Capital) | Industry)
            anova(Multi_Level_1, Multi_Level_2)

            summary( Multi_Level_1)
            random.effects( Multi_Level_1)        
            
            
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Analytical_Set$Period,"%Y-%m-%d"),
                                          Industry   = str_replace_all(Analytical_Set$Industry, "\\.", " "),
                                          GDP = as.numeric(exp(Analytical_Set$Output)),
                                          Estimates_MultiLevel = as.numeric(exp(Multi_Level_1$fitted[,2])))
            Actual_Expected <- merge(Actual_Expected,
                                     GLS_OLS_Estimates,
                                     by = c("TimePeriod", "Industry"))
                                          
            Actual_Expected <- merge(Actual_Expected,
                                     SystemEstimates,
                                     by = c("TimePeriod", "Industry"))
                               
            Actual_Expected <- reshape2::melt(Actual_Expected,
                                    id.vars = c("TimePeriod", "Industry"),
                                    measure.vars = c("GDP", "Estimates_MultiLevel", "Estimates_GLS", "SystemEstimates"))

            ##
            ##    Fix up the industry names
            ##
            Wrap_Industry <- data.frame(Industry = c("Public Administration and Safety", 
                                                     "Health Care and Social Assistance",
                                                     "Education and Training",
                                                     "Accommodation and Food Services",
                                                     "Arts, Recreation and Other Services",
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
                                    NZIndustry = c("Public\nAdministration\nand Safety",
                                                   "Health Care and\nSocial Assistance",
                                                   "Education and\nTraining",
                                                   "Accommodation and\nFood Services",
                                                   "Arts, Recreation\nand Other Services",
                                                   "Construction",
                                                   "Electricity,\nGas, Water\nand Waste Services",
                                                   "Financial and\nInsurance Services",
                                                   "Forestry and Mining",
                                                   "Information\nMedia\nand Telecommunications",
                                                   "Manufacturing",
                                                   "Professional,\nScientific,\nTechnical,\nAdministrative\nand Support Services",
                                                   "Rental, Hiring and\nReal Estate Services",
                                                   "Retail Trade\nand Accommodation",
                                                   "Transport, Postal\nand Warehousing",
                                                   "Wholesale Trade"), stringsAsFactors = FALSE)
             
               Actual_Expected <- merge(Actual_Expected,
                                        Wrap_Industry,
                                        by = c("Industry"),
                                        all = TRUE)
               Actual_Expected$NZIndustry <- ifelse(is.na(Actual_Expected$NZIndustry), Actual_Expected$Industry, Actual_Expected$NZIndustry)
            ggplot(Actual_Expected[Actual_Expected$variable %in% c("GDP", "SystemEstimates"),], aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.5) +
                   geom_point(size =.3) +
                   labs(title="New Zealand Production\nGross Domestic Product Measure - Actual and Estimated\n") +
                   ylab("Gross Domestic Product\n$(Mill)") +
                   scale_colour_manual(values = c("#7b1244","#0094c5"), name="Actual or Expected") +
                   xlab("Time Period\n") +
                   facet_grid(~NZIndustry, scales="free") +
                   theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 8,
                                                   family = "MyriadPro-Bold",
                                                   margin = margin(0.0,0.0,0.0,0.0, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 20, family = "MyriadPro-Regular"),
                         plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                         plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                         plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                         plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                         axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                         axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                         axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(10, 10, 10, 10),
                         legend.position  = "bottom")

            ggsave("Graphical_Output/New Zealand Production.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
            
            ggplot(Actual_Expected[(Actual_Expected$variable %in% c("GDP", "SystemEstimates")) &
                                   (year(Actual_Expected$TimePeriod) > 2019),], aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.5) +
                   geom_point(size =.3) +
                   labs(title="New Zealand Production\nGross Domestic Product Measure - Actual and Estimated\nCOVID and since\n") +
                   ylab("Gross Domestic Product\n$(Mill)") +
                   scale_colour_manual(values = c("#7b1244","#0094c5"), name="Actual or Expected") +
                   xlab("Time Period\n") +
                   facet_grid(~NZIndustry, scales="free") +
                   theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 10,
                                                   family = "MyriadPro-Bold",
                                                   margin = margin(0.0,0.0,0.0,0.0, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 20, family = "MyriadPro-Regular"),
                         plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                         plot.subtitle = element_text(size = 22, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                         plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                         plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                         axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                         axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                         axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(10, 10, 10, 10),
                         legend.position  = "bottom")

            ggsave("Graphical_Output/New Zealand Production - COVID and Since.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
        ##
        ##    Which Industries are cooking and which are uncooking?
        ##
            Industry_Measures <- reshape2::dcast(Actual_Expected[Actual_Expected$variable %in% c("GDP", "SystemEstimates"),],
                                                 Industry + TimePeriod + NZIndustry ~ variable,
                                                 value.var = c("value"))
            Industry_Measures$Output_Gap <- with(Industry_Measures, ((GDP / SystemEstimates)-1)*100)
        ##
        ##    Reorder the Employee Sizes
        ##
            Industry_Measures <- Industry_Measures[year(Industry_Measures$TimePeriod) >= (year(max(Industry_Measures$TimePeriod))-1),]      
            reorder_dset <- Industry_Measures[Industry_Measures$TimePeriod == max(Industry_Measures$TimePeriod),]
            
            Industry_Measures$NZIndustry <- factor(Industry_Measures$NZIndustry, 
                                                  levels=reorder_dset$NZIndustry[order(reorder_dset$Output_Gap, decreasing = TRUE)]) 
            Industry_Measures$Year <- as.character(year(Industry_Measures$TimePeriod))
                       
                          
        png("Graphical_Output/Output Gap By Industry.png", w = 15.7, h = 8.3, res = 600, units = "in")

         vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)        
         grid.newpage()
          pushViewport(viewport(layout=grid.layout(nrow = 1,
                                                   ncol = 1)))
           p1 <- ggplot(Industry_Measures, aes(x=NZIndustry, y=Output_Gap, fill=Year)) +
                  geom_bar(stat="identity", position=position_dodge()) + 
                  coord_flip() +
#                  scale_colour_manual(aesthetics = c("colour", "fill"), values = nzier.cols(), name="") +
                  labs(x = "", fill = "Industries\n")+
                  labs(y = "\nDifference between Actual and Expected Output\n(Given Capital Stock and Employed Labour)\n") +
                  scale_y_continuous(breaks=seq(-20,20,1), labels=paste0(seq(-20,20,1),"%")) +
                  geom_hline(yintercept=0, color="white", size=1) +
                  theme(axis.text.x = element_text(angle=00)) +
                  geom_text(x=7.5, y=-10, size=6, label="Under\nPerforming\nIndustries") +
                  geom_text(x=7.5, y= 5, size=6, label="Over\nPerforming\nIndustries") +
                  labs(title = "Output Gap - by Industry", 
                       subtitle = "New Zealand Institute of Economic Research\n")
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
         Output_Gap <- reshape2::dcast(Output_Gap,
                                       TimePeriod ~ variable)
         Output_Gap$Output_Gap <- with(Output_Gap, ((GDP / SystemEstimates)-1)*100)
         
        ##
        ##     Grab some Unemployment and Inflation measures
        ##
         Output_Gap <- merge(Output_Gap,
                             Unemployed[((Unemployed$Age == "Total All Ages") &
                                         (Unemployed$Gender == "Total Both Sexes") &
                                         (Unemployed$Measure == "Unemployment Rate") 
                                         ),],
                             by.x = c("TimePeriod"),
                             by.y = c("Period"))
         names(Output_Gap)[names(Output_Gap) == 'value'] = "Unemployment_Rate"
         ##
         ##    Quite a clear cyclical pattern showing up when you look at the points of a 
         ##       scattergraph, by year
         ##
      showtext_auto()
         
         ggplot(Output_Gap[(month(Output_Gap$TimePeriod) == 3),], aes(x=Output_Gap/100, y=Value/100))     +
                geom_smooth(method=lm) +
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = c("#7b1244")) +
                geom_point(size = 1.5, colour = c("#0094c5")) +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=7, nudge_x = 0.003) +
                scale_x_continuous(labels = percent, breaks = seq(from = -0.08, to = 0.05, by =0.01)) +
                scale_y_continuous(labels = percent, breaks = seq(from = 0, to = 0.13, by =0.01)) +                

                geom_vline(xintercept = 0, size=2, alpha = 0.3, colour = SPCColours("Green")) +

                geom_text(x= -0.02, y=0.11, size=13, label="Below Potential",family ="MyriadPro-Light", hjust = 0,colour = SPCColours("Gold")) +
                geom_text(x= -0.02, y=0.11, size=13, label="Above Potential",  family ="MyriadPro-Light", hjust = -1.155,colour = SPCColours("Gold")) +

                ylab("Unemployment Rate\n") +
                xlab("\nGross Domestic Product Output Gap\n(Actual GDP / Expected GDP)") +                
                labs(title="The New Zealand Business Cycle\n")  +         
         
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
                      plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                      axis.text.y   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(1, "cm"),
                      legend.margin = margin(10, 10, 10, 10),
                      legend.position  = "bottom")
                      
         ggsave("Graphical_Output/Okuns Law - System_of_Equations.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

     showtext_auto()
         
         ggplot(Output_Gap[(month(Output_Gap$TimePeriod) == 3) &
                           (year(Output_Gap$TimePeriod) >= 1992) *
                           (year(Output_Gap$TimePeriod) <= 2017),], aes(x=Output_Gap/100, y=Value/100))     +
                geom_smooth(method=lm) +
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = c("#7b1244")) +
                geom_point(size = 1.5, colour = c("#0094c5")) +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=7, nudge_x = 0.005) +
                scale_x_continuous(labels = percent) +
                scale_y_continuous(labels = percent) +                

                geom_vline(xintercept = 0, size=2, alpha = 0.3, colour = SPCColours("Green")) +

                geom_text(x= -0.01, y=0.11, size=6, label="Below Potential",family ="MyriadPro-Light", hjust = 0,colour = SPCColours("Gold")) +
                geom_text(x= 0, y=0.11, size=6, label="Above Potential",  family ="MyriadPro-Light", hjust = -.25,colour = SPCColours("Gold")) +

                ylab("Unemployment Rate\n") +
                xlab("\nGross Domestic Product Output Gap\n(Actual GDP / Expected GDP)") +                
                labs(title="The New Zealand Business Cycle\n")  +         
         
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
                      plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                      axis.text.y   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(1, "cm"),
                      legend.margin = margin(10, 10, 10, 10),
                      legend.position  = "bottom")
                      
         ggsave("Graphical_Output/Okuns Law - NZIER Comparison.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

      ##
      ##    Do some error checking
      ##
         Actual_Expected[(Actual_Expected$TimePeriod) == max(Actual_Expected$TimePeriod, na.rm=TRUE),]

        ##
        ##     Regress it...
        ##
         OLS_Unemployment <- lm(log(Value) ~ log(Output_Gap), data=Output_Gap)
         summary(OLS_Unemployment)

         GLS_Unemployment <- gls(log(Value) ~ log(Output_Gap), 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_Unemployment)


  ##
  ##     Save the output sets
  ##
   save(Actual_Expected, file= "Data_Output/Actual_Expected.rda")
   save(Output_Gap, file= "Data_Output/Output_Gap.rda")
  
  

   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
