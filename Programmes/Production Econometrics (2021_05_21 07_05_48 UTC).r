##
##    Programme:  Production Econometrics.r
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
      library(plm)
      library(splines)
      library(calibrate)
      library(systemfit)
      library(micEconCES)      
      library(XLConnect)
      library(forecast)
      
   ##
   ##    Read in the Source Data
   ##
      Gross_Output  <- read.csv("Gross_Output_By_Industry.csv", skip = 2)
      Labour_Input  <- read.csv("QES_By_Industry.csv", skip = 1)
      Capital_Input <- read.csv("Capital_Stock_By_Industry.csv", skip = 2)
      PPI_Output    <- read.csv("PPI_Outputs.csv", skip = 1)

   ##
   ##    Drop out all the missing values
   ##
      Gross_Output  <- Gross_Output[!is.na(Gross_Output[,3]),]
      Labour_Input  <- Labour_Input[Labour_Input[,3] != "",]
      Capital_Input <- Capital_Input[!is.na(Capital_Input[,3]),]
      PPI_Output    <- PPI_Output[!is.na(PPI_Output[,3]),]

   ##
   ##    Sort out the Dates
   ##
      Gross_Output$TimePeriod  <- as.Date(str_c("01/03",str_sub(Gross_Output$X.,1,4),sep = "/"),"%d/%m/%Y")
      Labour_Input$TimePeriod  <- as.Date(str_c("1",as.numeric(str_sub(Labour_Input$X,-1))*3,str_sub(Labour_Input$X,1,4),sep = "/"),"%d/%m/%Y")
      Capital_Input$TimePeriod <- as.Date(str_c("01/03",str_sub(Capital_Input$X.,1,4),sep = "/"),"%d/%m/%Y")
      PPI_Output$TimePeriod    <- as.Date(str_c("1",as.numeric(str_sub(PPI_Output$X,-1))*3,str_sub(PPI_Output$X,1,4),sep = "/"),"%d/%m/%Y")

      max(Gross_Output$TimePeriod)      
      max(Labour_Input$TimePeriod)      
      max(Capital_Input$TimePeriod)      
      max(PPI_Output$TimePeriod)      
      unique(Labour_Input$TimePeriod)
      
      Labour_Input  <- Labour_Input[!is.na(Labour_Input$TimePeriod),]
      Labour_Input  <- Labour_Input[,names(Labour_Input) != "X"]
      Gross_Output  <- Gross_Output[,names(Gross_Output) != "X."]
      Capital_Input <- Capital_Input[,names(Capital_Input) != "X."]
      PPI_Output    <- PPI_Output[,names(PPI_Output) != "X"]
   
   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      Gross_Output  <- melt(Gross_Output, id.vars = c("TimePeriod"))
      PPI_Output    <- melt(PPI_Output,   id.vars = c("TimePeriod"))
      Labour_Input  <- melt(Labour_Input, id.vars = c("TimePeriod"))
      Capital_Input <- melt(Capital_Input, id.vars = c("TimePeriod"))
      Labour_Input$value <- as.numeric(Labour_Input$value)
   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      PPI_Output$YEMar <- as.Date(ifelse(month(PPI_Output$TimePeriod) <= 3, paste0("01/03/", year(PPI_Output$TimePeriod)), paste0("01/03/", year(PPI_Output$TimePeriod)+1)),"%d/%m/%Y")
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
                        orig=c("Accommodation.and.Food.Services",
                               "Administrative.and.Support.Services",
                               "Agriculture",
                               "Arts.and.Recreation.Services",
                               "Central.Government.Administration..Defence.and.Public.Safety",
                               "Construction",
                               "Education.and.Training",
                               "Electricity..Gas..Water.and.Waste.Services",
                               "Financial.and.Insurance.Services",
                               "Fishing..Aquaculture.and.Agriculture..Forestry.and.Fishing.Support.Services",
                               "Food..Beverage.and.Tobacco.Product.Manufacturing",
                               "Forestry.and.Logging",
                               "Furniture.and.Other.Manufacturing",
                               "Health.Care.and.Social.Assistance",
                               "Information.Media.and.Telecommunications",
                               "Local.Government.Administration",
                               "Metal.Product.Manufacturing",
                               "Mining",
                               "Non.Metallic.Mineral.Product.Manufacturing",
                               "Other.Services",
                               "Owner.Occupied.Property.Operation..National.Accounts.Only.",
                               "Petroleum..Chemical..Polymer.and.Rubber.Product.Manufacturing",
                               "Printing",
                               "Professional..Scientific.and.Technical.Services",
                               "Rental..Hiring.and.Real.Estate.Services",
                               "Retail.Trade",
                               "Textile..Leather..Clothing.and.Footwear.Manufacturing",
                               "Transport..Postal.and.Warehousing",
                               "Transport.Equipment..Machinery.and.Equipment.Manufacturing",
                               "Wholesale.Trade",
                               "Wood.and.Paper.Products.Manufacturing"
                               ),
                         new=c("Retail.Trade.and.Accommodation",
                               "Professional..Scientific..Technical..Administrative.and.Support.Services",
                               "Biff",
                               "Arts..Recreation.and.Other.Services",
                               "Public.Administration.and.Safety",
                               "Construction",
                               "Education.and.Training",
                               "Electricity..Gas..Water.and.Waste.Services",
                               "Financial.and.Insurance.Services",
                               "Forestry.and.Mining",
                               "Manufacturing",
                               "Forestry.and.Mining",
                               "Manufacturing",
                               "Health.Care.and.Social.Assistance",
                               "Information.Media.and.Telecommunications",
                               "Public.Administration.and.Safety",
                               "Manufacturing",
                               "Forestry.and.Mining",
                               "Manufacturing",
                               "Arts..Recreation.and.Other.Services",
                               "Biff",
                               "Manufacturing",
                               "Manufacturing",
                               "Professional..Scientific..Technical..Administrative.and.Support.Services",
                               "Rental..Hiring.and.Real.Estate.Services",
                               "Retail.Trade.and.Accommodation",
                               "Manufacturing",
                               "Transport..Postal.and.Warehousing",
                               "Manufacturing",
                               "Wholesale.Trade",
                               "Manufacturing"
                              ))
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
                           orig=c("Agriculture..Forestry.and.Fishing",
                                  "Mining"),
                            new=c("Forestry.and.Mining",
                                  "Forestry.and.Mining"))
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
                           orig=c("Retail.Trade",
                                  "Accommodation.and.Food.Services"),
                            new=c("Retail.Trade.and.Accommodation",
                                  "Retail.Trade.and.Accommodation"))
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
                               by = c("TimePeriod", "variable"))
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
        ##  Second, OLS, with dummy variables for industries
        ##        
            OLS_Dummy <- lm(Output ~ (Year + Labour + Capital)*variable, data=Gross_Output)
            summary(OLS_Dummy)
            plot(OLS_Dummy)
            anova(OLS, OLS_Dummy)
           ##
           ##  Test for Autocorrelation:  Yep, higher order Autocorrelation happening now
           ##     Autocorrelation can indicate omitted variable trending in the data.
           ##
               acf(OLS_Dummy$residuals)
               pacf(OLS_Dummy$residuals)
               dwtest(OLS_Dummy)

           ##
           ##  Test for Hetroskedasticity:  Breusch-Pagan test.  
           ##     Yep, heaps of hetroskedasticity - still havent sorted this problem out
           ##
               bptest(OLS_Dummy)           
           
           ##
           ##  Test for Structural Breaks.  Yep, evidence of structure breaks in data
           ##
               sctest(OLS_Dummy)
               reset(OLS_Dummy)
               
        ##
        ##  Third:  As a mixed-multilevel model.  This model assumes random coefficients that
        ##     vary by industry.  I've also included a weights function for addressing hetroskedasticity.
        ##
            ML_Production_LabOnly <- lme(Output ~ Year + Labour + Capital,
                                          data = Gross_Output, 
                                          weights = varFunc(~ as.numeric(variable)),
                                          random = ~ (Labour) |variable)
            summary(ML_Production_LabOnly)
            random.effects(ML_Production_LabOnly)
                                          
            ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Gross_Output, 
                                        weights = varFunc(~ as.numeric(variable)),
                                        random = ~ (Labour + Capital) |variable)
            summary(ML_Production_LabCap)
            random.effects(ML_Production_LabCap)

            anova(ML_Production_LabOnly,ML_Production_LabCap)
          ##
          ##      Check for Autocorrelation
          ##
             Residuals <- data.frame(TimePeriod = as.Date(Gross_Output$TimePeriod,"%Y-%m-%d"),
                                     Industry   = wrap(str_replace_all(Gross_Output$variable, "\\.", " ")),
                                     Residuals  = ML_Production_LabCap$residuals[,2])          
             Residuals$Industry <- as.character(Residuals$Industry)
             
             inds <- unique(Residuals$Industry)
             par(mfrow=c(3,5))
             for (i in 1:length(inds))
             {   acf(ts(Residuals[Residuals$Industry == unique(Residuals$Industry)[i],]$Residuals, start=c(1995,1)),
                      main=inds[i])
             }                                

          ##
          ##      Adjust for autocorrelation:  Make it related to each industry
          ##
            ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Gross_Output, 
                                        weights = varFunc(~ as.numeric(variable)),
                                        correlation=corAR1(0,form = ~ Year|variable),
                                        random = ~ (Labour + Capital) |variable)
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
            
            Actual_Expected <- rbind(Actual_Expected, Predictions)

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
            ggsave("New Zealand Production - Cobb Douglas.png",width = 16.5, height = 11.7, dpi=600) 

         
        ##
        ##  Finally, lets estimate this as a System of Equations
        ##
            ##
            ##    Estimate the system on Panel Data
            ##               
               SUR <- systemfit(Output ~ Year + Labour + Capital,
                                data = Gross_Output,
                                method = "SUR")

            ##
            ##    Grab all the expected values
            ##
               allframes = lapply(1:length(SUR),function(x)SUR[[1]][[x]]$fitted.values)
               Expected <- data.frame(do.call(cbind,allframes))
               for(i in 1:length(SUR)) names(Expected)[i] = SUR[[1]][[i]]$eqnLabel
               Expected <- cbind(TimePeriod = as.factor(unique(Gross_Output$TimePeriod)), Expected)
               Expected <- melt(Expected,
                                id.vars = c("TimePeriod"))
               Expected$Output <- Expected$value
               
               Actual_Expected_System <- rbind.fill(data.frame(Gross_Output,
                                                               Source = "Actual"),
                                                    data.frame(Expected,
                                                               Source = "Expected"))
                                                               
               Actual_Expected_System <- Actual_Expected_System[,names(Actual_Expected_System) %in% c("variable", "TimePeriod", "Output", "Source")]
               Actual_Expected_System$TimePeriod <- as.Date(Actual_Expected_System$TimePeriod,"%Y-%m-%d")                    
               Actual_Expected_System$Industry <- wrap(str_replace_all(Actual_Expected_System$variable, "\\.", " "))
               
            ##
            ##    Grab Plot
            ##
               ggplot(Actual_Expected_System, aes(x=TimePeriod, y=Output, colour=Source))     +
                      geom_line(size = .65) +
                      geom_point(size = .5) +
                      labs(title="New Zealand Production\nCobb-Douglas Function, Estimated with System of Equations\n") +
                      ylab("Gross Output\n$(Mill)") +
                      theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                            strip.text  = element_text(angle=00, vjust=0.5, size=8),
                            legend.position="right")+
                      facet_grid(~Industry, scales="free")
                      
            ##
            ##    Plot the residuals
            ##
               allframes = lapply(1:length(SUR),function(x)SUR[[1]][[x]]$residuals)
               Residuals <- data.frame(do.call(cbind,allframes))
               for(i in 1:length(SUR)) names(Residuals)[i] = SUR[[1]][[i]]$eqnLabel
               Residuals <- cbind(TimePeriod = as.factor(unique(Gross_Output$TimePeriod)), Residuals)
               Residuals <- melt(Residuals,
                                 id.vars = c("TimePeriod"))
               Residuals$TimePeriod <- as.Date(Residuals$TimePeriod,"%Y-%m-%d")                    
               Residuals$Industry <- wrap(str_replace_all(Residuals$variable, "\\.", " "))
               
               ggplot(Residuals, aes(x=TimePeriod, y=value))     +
                      geom_line() +
                      labs(title="Regression Residuals\nSystem of Equations\n") +
                      theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8),
                            strip.text  = element_text(angle=00, vjust=0.5, size=8),
                            legend.position="right")+
                      facet_grid(~Industry, scales="free")
                ##
                ##      Check for Autocorrelation
                ##
                   inds <- unique(Residuals$Industry)
                   par(mfrow=c(3,5))
                   for (i in 1: length(inds))
                   {   acf(ts(subset(Residuals, Industry == unique(Residuals$Industry)[i])$value, start=c(1995,1)),
                            main=inds[i])
                   }
                                
