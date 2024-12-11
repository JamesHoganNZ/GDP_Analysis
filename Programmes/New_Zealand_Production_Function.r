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
      load("Data_Output/ConstantPrice_SA_Qtr_GDP_Published20241206.rda")
      load("Data_Output/ConstantPrice_Actual_Annual_CapitalStock_Published20241206.rda")
      load("Data_Output/ConstantPrice_Actual_Qtr_PaidHours_Published20241206.rda")
                     ConstantPrice_Actual_Qtr_PaidHours_Published20241206
   ##
   ## Step 1: Check out the Industries and move each data source to a common industry definition
   ##
      GDP      <- unique(ConstantPrice_SA_Qtr_GDP_Published20241206$Industry)
      CapStock <- unique(ConstantPrice_Actual_Annual_CapitalStock_Published20241206$Industry)
      Labour   <- unique(ConstantPrice_Actual_Qtr_PaidHours_Published20241206$Industry)
      
      GDP[!(GDP %in% CapStock)] # Only the unallocated in GDP is different
      #t(t(CapStock))      # These were used to make the below mapping
      #t(t(Labour))
      Reclassify_Industry <- data.table(CapStock = c("Accommodation and Food Services","Administrative and Support Services","Agriculture","Arts and Recreation Services","Central Government Administration, Defence and Public Safety","Construction","Education and Training","Electricity, Gas, Water and Waste Services","Financial and Insurance Services","Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services","Food, Beverage and Tobacco Product Manufacturing","Forestry and Logging","Furniture and Other Manufacturing","Health Care and Social Assistance","Information Media and Telecommunications","Local Government Administration","Metal Product Manufacturing","Mining","Non-Metallic Mineral Product Manufacturing","Other Services","Owner-Occupied Property Operation (National Accounts Only)","Petroleum, Chemical, Polymer and Rubber Product Manufacturing","Printing","Professional, Scientific and Technical Services","Rental, Hiring and Real Estate Services","Retail Trade","Textile, Leather, Clothing and Footwear Manufacturing","Transport Equipment, Machinery and Equipment Manufacturing","Transport, Postal and Warehousing","Wholesale Trade","Wood and Paper Products Manufacturing"),
                                        Labour   = c("Accommodation and Food Services","Professional, Scientific, Technical, Administrative and Support Services","EXCLUDED FROM QES","Arts, Recreation and Other Services","Public Administration and Safety","Construction","Education and Training","Electricity, Gas, Water and Waste Services","Financial and Insurance Services","EXCLUDED FROM QES","Manufacturing","Forestry and Mining","Manufacturing","Health Care and Social Assistance","Information Media and Telecommunications","Public Administration and Safety","Manufacturing","Forestry and Mining","Manufacturing","Arts, Recreation and Other Services","EXCLUDED FROM QES","Manufacturing","Manufacturing","Professional, Scientific, Technical, Administrative and Support Services","Rental, Hiring and Real Estate Services","Retail Trade","Manufacturing","Manufacturing","Transport, Postal and Warehousing","Wholesale Trade","Manufacturing"))

      GDP <- merge(ConstantPrice_SA_Qtr_GDP_Published20241206,
                   Reclassify_Industry,
                   by.x = c("Industry"),
                   by.y = c("CapStock"))

      CapStock <- merge(ConstantPrice_Actual_Annual_CapitalStock_Published20241206,
                        Reclassify_Industry,
                        by.x = c("Industry"),
                        by.y = c("CapStock"))
                        
      Labour <- ConstantPrice_Actual_Qtr_PaidHours_Published20241206
      
      GDP <- GDP[,
                 list(GDP = sum(Value, na.rm = TRUE)),
                 by = list(Period, 
                           Industry = Labour)]
                 
      CapStock <- CapStock[,
                 list(Value = sum(Value, na.rm = TRUE)),
                 by = list(Period, 
                           Industry = Labour)]


   ##
   ## Step 2: Interpolate the annual capital stock into a quarterly measure. I'll use these as "seaonally adjusted"
   ##
      CapStock  <- data.frame(CapStock)
      XList <- lapply(unique(CapStock$Industry), function(Industry) 
               {  F_Data <- CapStock[CapStock$Industry == Industry,]
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
                                      Y$Industry <- as.character(Industry)
                                  return(Y)
                                }, warning = function(w) {
                             }, error = function(e) {
                             }, finally = {
                             })
                          return(X)
               })
         InterpCapStock <- data.table(do.call(rbind, XList))
         InterpCapStock$Capital_Stock <- as.numeric(InterpCapStock$Value)


      ##
      ##    Stick capital, labour and output all together
      ##
         Production_Data <- merge(GDP,
                                  InterpCapStock[,c("Period","Industry","Capital_Stock")],
                                  by = c("Period", "Industry"))
         Production_Data <- merge(Production_Data,
                                  Labour,
                                  by = c("Period", "Industry"))
         names(Production_Data)[names(Production_Data) == "Value"] <- "PaidHours"                        

      ##
      ##    Make some logged variables
      ##
         Production_Data$Logged_L <- log(Production_Data$PaidHours)
         Production_Data$Logged_K <- log(Production_Data$Capital_Stock)
         Production_Data$Logged_Y <- log(Production_Data$GDP )

         model1 <- lm(Logged_Y ~ Industry + Industry*(Logged_K + Logged_L),  data = Production_Data)
         summary(model1)
         confint(model1)
         
         model2 <- lme(Logged_Y ~ Logged_K + Logged_L, random = ~1|Industry, data = Production_Data)
         summary(model2)
         confint(model2)
      ##
      ##    Panel Model
      ##
         
         Panel <- pdata.frame(Production_Data, index = c("Industry","Period"))
         head(Panel)
         
         wonder1 <- plm(Logged_Y ~ Logged_K + Logged_L, data=Panel, model="random")
         summary(wonder1)
         plmtest(wonder1,effect="twoways",type="ghm")
 
         wonder2 <- pvcm(Logged_Y ~ Logged_K + Logged_L,data=Panel, model = "random")
         summary(wonder2)
         pooltest(wonder1,wonder2)
         
      ##      
      ##    Lets try some panel data measures
      ##             
         
         wonder1 <- plm(Logged_Y ~ Logged_K + Logged_L, data=Panel, model="within")
         wonder2 <- plm(Logged_Y ~ Logged_K + Logged_L, data=Panel, model="random")
         phtest(wonder1,wonder2)
         pwtest(Logged_Y ~ Logged_K + Logged_L, data=Panel)
         pbsytest(Logged_Y ~ Logged_K + Logged_L, data=Panel,test="j")         
         pbsytest(Logged_Y ~ Logged_K + Logged_L, data=Panel)         
         pbsytest(Logged_Y ~ Logged_K + Logged_L, data=Panel,test="re")         
         pbltest(Logged_Y ~ Logged_K + Logged_L, data=Panel,alternative="onesided")         
         pcdtest(Logged_Y ~ Logged_K + Logged_L, data=Production_Data)         
          
         lmAR1ML<-gls(Logged_Y ~ (Logged_K + Logged_L)*Industry,
                      data=Production_Data, 
                      correlation=corAR1(0,form=~Period|Industry))     
         summary(lmAR1ML)
                      
         reAR1ML<-lme(Logged_Y ~ (Logged_K + Logged_L)*Industry -1 - Logged_K - Logged_L,
                      data=Production_Data, 
                      correlation=corAR1(0,form=~Period|Industry),
                      random=~1|Industry)   
         summary(reAR1ML)

                      
         lmML <- gls(Logged_Y ~ (Logged_K + Logged_L)*Industry,data=Production_Data)                      
         reML <- lme(Logged_Y ~ (Logged_K + Logged_L)*Industry,data=Production_Data,random=~1|Industry)
         anova(lmML,lmAR1ML)
         anova(reML,reAR1ML)
         summary(lmAR1ML)
         summary(reAR1ML)
          
          
         tmp2 <- data.frame(Production_Data, residuals=lmAR1ML$residuals)          
         ggplot(tmp2, aes(x=Period, y=residuals))     +
           geom_line() +
           facet_wrap(~Industry, ncol=3)

          
         tmp2 <- data.frame(Production_Data, residuals=reAR1ML$residuals)          
         ggplot(tmp2, aes(x=Period, y=residuals.Industry))     +
           geom_line() +
           facet_wrap(~Industry, ncol=3)
         ggplot(tmp2, aes(x=Period, y=residuals.fixed))     +
           geom_line() +
           facet_wrap(~Industry, ncol=3)
           
         coefs <- as.data.frame(confint(reAR1ML))
         coefs$Industry <- row.names(coefs)
         names(coefs) <- c("Lower", "Upper", "Industry")
         coefs$Mid <- as.numeric(coef(test))
         coefs$Type <- rep(c("Intercept", "Growth"), rep(nrow(coefs)/2, 2))
         ggplot(subset(coefs, Type=="Growth"), aes(y=Industry)) +
         geom_point(aes(x=Mid)) +
         geom_segment(aes(x=Lower, xend=Upper, y=1:length(Industry), yend=1:length(Industry)))

##
##    And we're done
##
