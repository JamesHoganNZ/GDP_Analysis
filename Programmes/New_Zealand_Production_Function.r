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
   ##    Load data from somewhere  
   ##
      load("Data_Output/ConstantPrice_SA_Qtr_GDP_Published20250631.rda")
      load("Data_Output/ConstantPrice_Actual_Annual_CapitalStock_Published20250631.rda")
      load("Data_Output/ConstantPrice_Actual_Qtr_PaidHours_Published20250631.rda")
                     
   ##
   ## Step 1: Check out the Industries and move each data source to a common industry definition
   ##
      GDP      <- unique(ConstantPrice_SA_Qtr_GDP_Published20250631$Industry)
      CapStock <- unique(ConstantPrice_Actual_Annual_CapitalStock_Published20250631$Industry)
      Labour   <- unique(ConstantPrice_Actual_Qtr_PaidHours_Published20250631$Industry)
      
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
         Together <- merge(Together,
                           Labour,
                           by = c("Period", "Labour"),
                           stringsAsFactors = TRUE)
         names(Together) <- c("Period", "Industry", "GDP", "Capital_Stock", "Labour")                 
                           
      
     ##
     ##  Take the logs
     ##
         Together$Output  <- log(Together$GDP)
         Together$Capital <- log(Together$Capital_Stock)
         Together$Labour  <- log(Together$Labour)
         Together$Year    <- year(Together$Period)
         
         Together <- pdata.frame(data.frame(Together), c("Industry", "Period"))
   
     ##
     ##  Estimate the production function
     ##
     ##  
        ##
        ##  First, the baseline:  bog standard OLS
        ##
            OLS <- lm(Output ~ (Year + Labour + Capital)*Industry, data=Together)
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

             ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Together, 
                                        weights = varFunc(~ as.numeric(Industry)),
                                        #correlation=corAR1(0,form = ~ Year|variable),
                                        random = ~ (Year + Capital + Labour ) |Industry)



             ML_Production_LabCap <- lme(Output ~ Year + Labour + Capital,
                                        data = Together, 

                                        random = ~ (Year + Capital + Labour ) |Industry)














            
            ML_Production_LabCap <- lme(Output ~ (Year + Labour + Capital)|Industry,
                                        data = Together), 
                                        weights = varFunc(~ as.numeric(Industry))) ,
                                        #correlation=corAR1(0,form = ~ Year|variable),
                                        random = ~ (Year + Capital + Labour ) |Industry)
            summary(ML_Production_LabCap)
            random.effects(ML_Production_LabCap)            
           
        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(TimePeriod = as.Date(Together$Period,"%Y-%m-%d"),
                                          Industry   = str_wrap(str_replace_all(Together$Industry, "\\.", " ")),
                                          Actual_Gross_Output = as.numeric(exp(Together$Output)),
                                          Estimates = as.numeric(exp(ML_Production_LabCap$fitted[,2])))          
                               
            Actual_Expected <- reshape2::melt(Actual_Expected,
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
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
