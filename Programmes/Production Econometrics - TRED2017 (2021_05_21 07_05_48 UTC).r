##
##    Programme:  Production Econometrics.r
##
##    Objective:  This programme is a play with deriving an aggregate production function
##                for the New Zealand economy, using TRED as a data source.  This is a 
##                bit preliminary, but I'm thinking it might be an interesting story about
##                decomposing New Zealand activity into supply and demand factors.
##
##                Cutting to the chase, the graph coming out of the multi-level model 
##                suggests New Zealand was producing about its economic capacity for a bit
##                but post-GFC things have SERIOUSLY slowed and industry are operating 
##                below their economic productive potential.
##
##                Comments?  Feedback?  Improvements?
##
##       Author:  James Hogan, Sector Trends, 6 August 2015
##

   rm(list=ls(all=TRUE))
   
   ##
   ##    Read in some functionality, set the working directory and 
   ##       connect to the database
   ##
      library(RODBC)
      library(strucchange)
      library(lmtest)
      library(scales)
      library(dynlm)
      library(systemfit)
      library(ggplot2)
      library(sqldf)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(tseries)
      library(cluster)
      library(lubridate)
      library(nlme)
      library(mbie)
      library(mbieDBmisc)
      library(plm)
      library(splines)
      library(calibrate)
      library(systemfit)
      library(micEconCES)      
      library(forecast)
      TRED <- odbcConnect("TRED_Prod")

   ##
   ##    Read in the Source Data - TRED based ()
   ##
      Gross_Output  <- ImportTS2(TRED, "SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups (Annual-Mar)", silent = TRUE)
      #Labour_Input  <- ImportTS2(TRED, "Full-Time Equivalent Employees by Industry (ANZSIC06) and Sex (Qrtly-Mar/Jun/Sep/Dec)", silent = TRUE)
      
      ##
      ##    After discussing with David Patterson on the 16 Sept, he suggested I use QES Paid Hours as the labour measure.  But that also needs
      ##       an employment quantity measure too..
      ##
      Labour_Hours  <- ImportTS2(TRED, "Average Weekly Paid Hours (FTEs) by Industry (ANZSIC06) and Sex (Qrtly-Mar/Jun/Sep/Dec)", silent = TRUE)
      Labour_Qty    <- ImportTS2(TRED, "Full-Time Equivalent Employees by Industry (ANZSIC06) and Sex (Qrtly-Mar/Jun/Sep/Dec)",   silent = TRUE)
      
      Labour_Hours  <- Labour_Hours[Labour_Hours$CV2 != "Total Both Sexes",]
      Labour_Hours  <- Labour_Hours[Labour_Hours$CV3 == "Total (Ordinary Time + Overtime) Hours",]
      Labour <- merge(Labour_Hours,
                      Labour_Qty,
                      by = c("TimePeriod", "CV1", "CV2"))
      Labour$Total_Hours_Worked <- with(Labour, Value.x*Value.y)
      Labour_Input <- with(Labour,
                         aggregate(list(Value = Total_Hours_Worked),
                                   list(TimePeriod = TimePeriod,  
                                        CV1 = CV1),
                                   sum, 
                                   na.rm = FALSE)
                                   )
         
      
      Capital_Input <- ImportTS2(TRED, "SNE - Series, Balance sheet items, Chain Volume, Actual, ANZSIC06 high-level industry (Annual-Mar)", silent = TRUE)
      PPI_Output    <- ImportTS2(TRED, "Outputs (ANZSIC06) - NZSIOC level 2, Base: Dec. 2010 quarter (=1000) (Qrtly-Mar/Jun/Sep/Dec)", silent = TRUE)
      Unemployed    <- ImportTS2(TRED, "Labour Force Status by Sex by Total Resp Ethnic Group (Qrtly-Mar/Jun/Sep/Dec)", silent = TRUE)
      General_Price <- ImportTS2(TRED, "Series, Implicit price deflator, Actual, Total (Annual-Mar)", silent = TRUE)
      CPI           <- ImportTS2(TRED, "CPI All Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)", silent = TRUE)      

      
##
##    Stoopid earth quake meant SNZ hasn't updated its freaking InfoShare for capital
##       Now I have to rebuild it
##      
#   Capital_Input <- read.csv("na-nov2016-capital-stocks-csv.csv")
#   Capital_Input <- Capital_Input[((Capital_Input$Series_title_1 == "Net Capital Stock") &
#                       (Capital_Input$Series_title_3 == ""))  , c("Period", "Data_value", "Series_title_2")]

#   names(Capital_Input) = c("TimePeriod", "Value", "CV2")
#   Capital_Input$TimePeriod <- as.Date(paste0("31/", str_sub(Capital_Input$TimePeriod, start = 6, end = 7),"/", str_sub(Capital_Input$TimePeriod, start = 1, end = 4)), "%d/%m/%Y")
      
##
##    Ok, now the labour series has been discontinued... FARK... clean this up now
##        
   Unemployed     <- read.csv("P:\\R\\hoganj\\HLF000702_20161219_113005_90.csv")
   Unemployed$UID <- 1:nrow(Unemployed)
  
   test <- melt(Unemployed,
                id.vars = c("UID"))   
   Dates <- test[test$variable == 'Labour.Force.Status.by.Sex.by.Age.Group..Annual.Mar.',]
   ##
   ##    Ditch the non-numeric
   ##
   Dates <- Dates[!is.na(as.numeric(Dates$value)),]
   
   Headings <- test[test$UID < 4,]
   Flip <- dcast(Headings,
                 variable ~ UID)                   
                
   Flip$hold1 <- ""
   Flip$hold2 <- ""
   Flip$hold3 <- ""
    for(i in 2:nrow(Flip))
       {
          Flip$hold1[i] <- ifelse(str_sub(Flip[i,2], start = 1, end = 1) != "", 
                                 Flip[,2][i], 
                                 Flip$hold1[(i-1)])         
                                
          Flip$hold2[i] <- ifelse(str_sub(Flip[i,3], start = 1, end = 1) != "", 
                                 Flip[,3][i], 
                                 Flip$hold2[(i-1)])         
                                
          Flip$hold3[i] <- ifelse(str_sub(Flip[i,4], start = 1, end = 1) != "", 
                                 Flip[,4][i], 
                                 Flip$hold3[(i-1)])         
       }
   ##
   ##    Find only the all sex, all age, unemployment rate
   ##        
    Flip <- Flip[((Flip$hold1 == 'Total Both Sexes')  &
                  (Flip$hold2 == 'Total All Ages')  &
                  (Flip$hold3 == 'Unemployment Rate')),]

    test <- merge(test,
                  Flip,
                  by = c("variable"))
                 
    test <- merge(test,
                  Dates[, c("UID", "value")],
                  by = c("UID"))

   Unemployed <- test[, c("value.x", "value.y")]
   names(Unemployed) = c("Value", "TimePeriod")
      
   Unemployed$TimePeriod <- as.Date(paste0("31/03/", Unemployed$TimePeriod), "%d/%m/%Y")
   Unemployed$Value <- as.numeric(Unemployed$Value)
   
##
##    Back with the rest of the programme
##      
      min(Gross_Output$TimePeriod)      
      min(Labour_Input$TimePeriod)      
      min(Capital_Input$TimePeriod)      
      min(PPI_Output$TimePeriod)      
      min(Unemployed$TimePeriod)      
      min(General_Price$TimePeriod)      
      min(CPI$TimePeriod)      
 

      max(Gross_Output$TimePeriod)      
      max(Labour_Input$TimePeriod)      
      max(Capital_Input$TimePeriod)      
      max(PPI_Output$TimePeriod)      
      max(Unemployed$TimePeriod)      
      max(General_Price$TimePeriod)      
      max(CPI$TimePeriod)    
 
            
      Gross_Output  <- Gross_Output[Gross_Output$CV1 == 'Output', c("TimePeriod", "CV2", "Value")]
      Labour_Input  <- Labour_Input[, c("TimePeriod", "CV1", "Value")]
      Capital_Input <- Capital_Input[, c("TimePeriod", "CV2", "Value")]
      PPI_Output    <- PPI_Output[, c("TimePeriod", "CV1", "Value")]
      CPI           <- CPI[, c("TimePeriod", "CV1", "Value")]
      CPI           <- CPI[month(CPI$TimePeriod) == 3,]
      CPI           <- CPI[!is.na(CPI$TimePeriod),]
      General_Price <- General_Price[General_Price$CV1 == 'Gross Domestic Product - expenditure measure', c("TimePeriod", "CV1", "Value")]
      
       Unemployed    <- Unemployed[((Unemployed$CV1 == 'Unemployment Rate') &
                               (Unemployed$CV2 == 'Total Both Sexes') &
                               (Unemployed$CV3 == 'Total All Ethnic Groups')), c("TimePeriod", "CV1", "Value")]

      names(Gross_Output)[names(Gross_Output)   == 'CV2']  = 'CV1'
      names(Labour_Input)[names(Labour_Input)   == 'CV2']  = 'CV1'
      names(Capital_Input)[names(Capital_Input) == 'CV2']  = 'CV1'
   
   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      Gross_Output  <- melt(Gross_Output, 
                            id.vars = c("TimePeriod", "CV1"),
                            measure.vars = c("Value"))
      Gross_Output  <- Gross_Output[, names(Gross_Output) %in% c("TimePeriod", "CV1", "value")]
      names(Gross_Output)[names(Gross_Output) == "CV1"] = "variable"  
                            
      Labour_Input    <- melt(Labour_Input, 
                              id.vars = c("TimePeriod", "CV1"),
                              measure.vars = c("Value"))      
      Labour_Input  <- Labour_Input[, names(Labour_Input) %in% c("TimePeriod", "CV1", "value")]
      names(Labour_Input)[names(Labour_Input) == "CV1"] = "variable"  
                            
      Capital_Input <-  Capital_Input[, names(Capital_Input) %in% c("TimePeriod", "CV1", "Value")]
      names(Capital_Input)[names(Capital_Input) == "CV1"] = "variable"  
      names(Capital_Input)[names(Capital_Input) == "Value"] = "value"  
      
      names(PPI_Output)[names(PPI_Output) == 'Value']  = "value"      
      names(PPI_Output)[names(PPI_Output) == 'CV1']  = "variable"
         
   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      PPI_Output$YEMar <- as.Date(ifelse(month(PPI_Output$TimePeriod) <= 3, paste0("31/03/", year(PPI_Output$TimePeriod)), paste0("31/03/", year(PPI_Output$TimePeriod)+1)),"%d/%m/%Y")
      PPI_Output$value <- as.numeric(PPI_Output$value)
      PPI_Output <- with(PPI_Output,
                      aggregate(list(value = value),
                                list(TimePeriod = YEMar,  
                                     variable = variable),
                                mean, 
                                na.rm = FALSE)
                                )
      Gross_Output <- merge(Gross_Output,
                            PPI_Output,
                            by = c("TimePeriod", "variable"))
      Gross_Output$Output <- with(Gross_Output, (as.numeric(value.x) / (value.y/1000)))

   ##
   ##    Recode the Industries for Gross_Output
   ##
         Gross_Output$variable <- rename.levels(Gross_Output$variable,
                        orig=c("Accommodation and Food Services",
                               "Administrative and Support Services",
                               "Agriculture",
                               "Arts and Recreation Services",
                               "Central Government Administration, Defence and Public Safety",
                               "Construction",
                               "Education and Training",
                               "Electricity, Gas, Water and Waste Services",
                               "Financial and Insurance Services",
                               "Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services",
                               "Food, Beverage and Tobacco Product Manufacturing",
                               "Forestry and Logging",
                               "Furniture and Other Manufacturing",
                               "Health Care and Social Assistance",
                               "Information Media and Telecommunications",
                               "Local Government Administration",
                               "Metal Product Manufacturing",
                               "Mining",
                               "Non-Metallic Mineral Product Manufacturing",
                               "Other Services",
                               "Owner-Occupied Property Operation (National Accounts Only)",
                               "Petroleum, Chemical, Polymer and Rubber Product Manufacturing",
                               "Printing",
                               "Professional, Scientific and Technical Services",
                               "Rental, Hiring and Real Estate Services",
                               "Retail Trade",
                               "Textile, Leather, Clothing and Footwear Manufacturing",
                               "Total All Industries",
                               "Transport Equipment, Machinery and Equipment Manufacturing",
                               "Transport, Postal and Warehousing",
                               "Wholesale Trade",
                               "Wood and Paper Products Manufacturing"),
                         new=c("Retail Trade and Accommodation",
                               "Professional, Scientific, Technical, Administrative and Support Services",
                               "Biff",
                               "Arts, Recreation and Other Services",
                               "Biff",
                               "Construction",
                               "Education and Training",
                               "Electricity, Gas, Water and Waste Services",
                               "Financial and Insurance Services",
                               "Biff",
                               "Manufacturing",
                               "Forestry and Mining",
                               "Manufacturing",
                               "Health Care and Social Assistance",
                               "Information Media and Telecommunications",
                               "Biff",
                               "Manufacturing",
                               "Forestry and Mining",
                               "Manufacturing",
                               "Arts, Recreation and Other Services",
                               "Biff",
                               "Manufacturing",
                               "Manufacturing",
                               "Professional, Scientific, Technical, Administrative and Support Services",
                               "Rental, Hiring and Real Estate Services",
                               "Retail Trade and Accommodation",
                               "Manufacturing",
                               "Biff",
                               "Manufacturing",
                               "Transport, Postal and Warehousing",
                               "Wholesale Trade",
                               "Manufacturing"))
         Gross_Output <- with(Gross_Output,
                         aggregate(list(Output = Output),
                                   list(TimePeriod = TimePeriod,  
                                        variable = variable),
                                   sum, 
                                   na.rm = FALSE)
                                   )
     ##
     ##  Do the same for the capital inputs
     ##
         Capital_Input$variable <- rename.levels(Capital_Input$variable,
                           orig=c("Agriculture, Forestry and Fishing",
                                  "Arts, Recreation and Other Services",
                                  "Construction",
                                  "Education and Training",
                                  "Electricity, Gas, Water and Waste Services",
                                  "Financial and Insurance Services",
                                  "Health Care and Social Assistance",
                                  "Information Media and Telecommunications",
                                  "Manufacturing",
                                  "Mining",
                                  "Professional, Scientific, Technical, Administrative and Support Services",
                                  "Public Administration and Safety",
                                  "Rental, Hiring and Real Estate Services",
                                  "Retail Trade and Accommodation",
                                  "Total All Industries",
                                  "Transport, Postal and Warehousing",
                                  "Wholesale Trade"),
                            new=c("Forestry and Mining",
                                  "Arts, Recreation and Other Services",
                                  "Construction",
                                  "Education and Training",
                                  "Electricity, Gas, Water and Waste Services",
                                  "Financial and Insurance Services",
                                  "Health Care and Social Assistance",
                                  "Information Media and Telecommunications",
                                  "Manufacturing",
                                  "Forestry and Mining",
                                  "Professional, Scientific, Technical, Administrative and Support Services",
                                  "Public Administration and Safety",
                                  "Rental, Hiring and Real Estate Services",
                                  "Retail Trade and Accommodation",
                                  "Total All Industries",
                                  "Transport, Postal and Warehousing",
                                  "Wholesale Trade"))
         Capital_Input <- with(Capital_Input,
                         aggregate(list(Capital = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = variable),
                                   sum, 
                                   na.rm = FALSE)
                                   )
     ##
     ##  Now for the Labour_Input
     ##
         Labour_Input$variable <- rename.levels(Labour_Input$variable,
                           orig=c("Accommodation and Food Services",
                                  "Arts, Recreation and Other Services",
                                  "Construction",
                                  "Education and Training",
                                  "Electricity, Gas, Water and Waste Services",
                                  "Financial and Insurance Services",
                                  "Forestry and Mining",
                                  "Health Care and Social Assistance",
                                  "Information Media and Telecommunications",
                                  "Manufacturing",
                                  "Professional, Scientific, Technical, Administrative and Support Services",
                                  "Public Administration and Safety",
                                  "Rental, Hiring and Real Estate Services",
                                  "Retail Trade",
                                  "Total All Industries",
                                  "Transport, Postal and Warehousing",
                                  "Wholesale Trade"),
                            new=c("Retail Trade and Accommodation",
                                  "Arts, Recreation and Other Services",
                                  "Construction",
                                  "Education and Training",
                                  "Electricity, Gas, Water and Waste Services",
                                  "Financial and Insurance Services",
                                  "Forestry and Mining",
                                  "Health Care and Social Assistance",
                                  "Information Media and Telecommunications",
                                  "Manufacturing",
                                  "Professional, Scientific, Technical, Administrative and Support Services",
                                  "Public Administration and Safety",
                                  "Rental, Hiring and Real Estate Services",
                                  "Retail Trade and Accommodation",
                                  "Total All Industries",
                                  "Transport, Postal and Warehousing",
                                  "Wholesale Trade"))
         Labour_Input <- with(Labour_Input,
                         aggregate(list(Labour = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = variable),
                                   sum, 
                                   na.rm = FALSE)
                                   )
     
     ##
     ##  Merge everything together
     ##
         Gross_Output <- merge(Gross_Output,
                               Capital_Input,
                               by = c("TimePeriod", "variable"))
         Gross_Output <- merge(Gross_Output,
                               Labour_Input,
                               by = c("TimePeriod", "variable"),
                               stringsAsFactors = TRUE)
         Gross_Output <- Gross_Output[!is.na(Gross_Output$Output),]
         Gross_Output <- Gross_Output[!is.na(Gross_Output$Capital),]
         Gross_Output <- Gross_Output[!is.na(Gross_Output$Labour),]
      
     ##
     ##  Take the logs
     ##
         Gross_Output$Output  <- log(Gross_Output$Output)
         Gross_Output$Capital <- log(Gross_Output$Capital)
         Gross_Output$Labour  <- log(Gross_Output$Labour)
         Gross_Output$Year    <- year(Gross_Output$TimePeriod)
         Gross_Output <- plm.data(Gross_Output, c("variable", "TimePeriod"))
      
     ##
     ##  Estimate the production function
     ##
     ##  
        ##
        ##  First, the baseline:  bog standard OLS
        ##
            OLS <- lm(Output ~ Year + Labour + Capital, data=Gross_Output)
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
              
        ##
        ##  Secondly:  As a mixed-multilevel model.  This model assumes random coefficients that
        ##     vary by industry.  I've also included a weights function for addressing hetroskedasticity.
        ##

          ##
          ##      Adjust for autocorrelation:  Make it related to each industry
          ##
            
            ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Gross_Output, 
                                        weights = varFunc(~ as.numeric(variable)),
                                        #correlation=corAR1(0,form = ~ Year|variable),
                                        random = ~ (Year + Capital + Labour ) |variable)
            summary(ML_Production_LabCap)
            random.effects(ML_Production_LabCap)            
            
            
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Gross_Output$TimePeriod,"%Y-%m-%d"),
                                          Industry   = wrap(str_replace_all(Gross_Output$variable, "\\.", " ")),
                                          Actual_Gross_Output = as.numeric(exp(Gross_Output$Output)),
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
        ##
        ##     Try some predictions
        ##
            Future_Set <- merge(Labour_Input,
                                Capital_Input,
                                by = c("TimePeriod", "variable"))
            Future_Set$Capital <- log(Future_Set$Capital)
            Future_Set$Labour  <- log(Future_Set$Labour)
            Future_Set$Year    <- year(Future_Set$TimePeriod)
            Future_Set <- Future_Set[Future_Set$Year > 2012,]

            Predictions <- data.frame(TimePeriod = Future_Set$TimePeriod,
                                      Industry = wrap(str_replace_all(Future_Set$variable, "\\.", " ")),
                                      variable = "Estimates",
                                      value = as.numeric(exp(predict(ML_Production_LabCap, Future_Set))))
            Predictions <- Predictions[!is.na(Predictions$value),]
            
            #Actual_Expected <- rbind(Actual_Expected, Predictions)

            ggplot(Actual_Expected, aes(x=TimePeriod, y=value, colour=variable))     +
                   geom_line(size =.7) +
                   geom_point(size =.5) +
                   labs(title="New Zealand Production\nCobb-Douglas Function, Estimated with Multi-Level Model\n") +
                   ylab("Gross Output\n$(Mill)") +
                   theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                         strip.text  = element_text(angle=00, vjust=0.5, size=8),
                         legend.position="right")+
                   facet_grid(~Industry, scales="free")
               ##
               ##  Size	Height x Width (mm)	Height x Width (in)
               ##   A3	     420 x 297 mm	      16.5 x 11.7 in
               ##   A4	     297 x 210 mm	      11.7 x 8.3 in
               ##
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
                          Unemployed,
                          by = c("TimePeriod"))
#         Output_Gap <- merge(Output_Gap,
#                          Unemployed[((Unemployed$CV1 == "Unemployment Rate") &
#                                    (Unemployed$CV2 == "Total Both Sexes") &
#                                    (Unemployed$CV3 == 'Total All Ethnic Groups')),c("TimePeriod", "Value")],
#                             by = c("TimePeriod"))
         names(Output_Gap)[names(Output_Gap) == 'Value'] = "Unemployment.Rate"
         ##
         ##    Quite a clear cyclical pattern showing up when you look at the points of a 
         ##       scattergraph, by year
         ##
         ggplot(Output_Gap, aes(x=Output_Gap/100, y=Unemployment.Rate/100))     +
                geom_smooth(method=lm) +
                geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = "green") +
                geom_point(size = 1, colour = "blue") +
                geom_text(aes(label=format(TimePeriod, "%Y")), size=2, nudge_x = 0.0025) +
                scale_x_continuous(labels = percent) +
                scale_y_continuous(labels = percent) +                
                ylab("Unemployment Rate\n") +
                xlab("Economic Production Output Gap\n(Actual Output / Expected Output)") +                
                labs(title="The New Zealand Business Cycle\n Update for 2017\n")  +         
                theme(axis.title.y = element_text(angle=90, vjust=0.5, size=8),
                      axis.title.x = element_text(angle=00, vjust=0.5, size=8),
                      axis.text.x  = element_text(angle=00, vjust=0.5, size=6),
                      axis.text.y  = element_text(angle=00, vjust=0.5, size=6),
                      strip.text   = element_text(angle=00, vjust=0.5, size=5),
                      legend.title = element_text(angle=00, vjust=0.5, size=5),
                      legend.text  = element_text(angle=00, vjust=0.5, size=5),
                      #plot.background= element_rect(fill="red"),
                      legend.margin  = unit(-0.6,"cm"),
                      plot.margin=unit(c(1,1,1,1),"mm"),
                      legend.position  = "bottom") 
                      
         ggsave("Okuns Law - Update for 2017.png",units = "cm", width = 18.65, height = 12.44, dpi=600) 

        ##
        ##     Regress it...
        ##
         OLS_Unemployment <- lm(Unemployment.Rate ~ Output_Gap, data=Output_Gap)
         summary(OLS_Unemployment)

         
        ##     From Peter: 
        ##     You should use gls from nlme instead of lm so you can add an ar(1) process to the random part.  
        ##        Your standard errors will currently be deflated if you're going what I think you are,
        ##

         GLS_Unemployment <- gls(Unemployment.Rate ~ Output_Gap, 
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
               General_Price$Inflation[i] <- ((General_Price$Value[i] / General_Price$Value[(i-1)])-1)*100
            }
         for(i in 2:nrow(General_Price))
            {  
               General_Price$Change_in_Inflation[i] <- (General_Price$Inflation[i] - General_Price$Inflation[(i-1)])
            }
         Output_Gap <- merge(Output_Gap,
                             General_Price[, c("TimePeriod", "Inflation", "Change_in_Inflation")] ,
                             by = c("TimePeriod"))
          ##
          ##     Which Prices?  CPI and IPD
          ##         Nope.  No evidence - WHAT SO EVERR!!! - that the output gap manifests in inflation
          ##
         CPI$CPIInflation <- NA
         CPI$Change_in_CPIInflation <- NA
         for(i in 2:nrow(CPI))
            {  
               CPI$CPIInflation[i] <- ((CPI$Value[i] / CPI$Value[(i-1)])-1)*100
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
