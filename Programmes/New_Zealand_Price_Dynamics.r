##
##    Programme:  New_Zealand_Price_Dynamics.r
##
##    Objective:  
##
##
##    https://medium.com/@marc.jacobs012/cointegration-of-time-series-in-r-a6543dacf66e
##
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
      load("Data_Output/ConstantPrice_Actual_Qtr_PaidHours_Published20250731.rda")
      load("Data_Output/ConstantPrice_SA_Qtr_GDP_Published20250631.rda")
      
      load("Data_Output/Broad_Money20250731.rda")
      load("Data_Output/Credit_Card_Stats20250731.rda")
      load("Data_Output/Deposits_by_Industry20250731.rda")
      load("Data_Output/Loans_by_Industry20250731.rda")
      load("Data_Output/Retail_Interest_Rates20250731.rda")
      load("Data_Output/Sector_lending20250731.rda")

      load("Data_Output/CurrentPrice_SA_Qtr_CPILevel3_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_CPINonStd_Less_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_CPIAllGroup_Published20250731.rda")

      load("Data_Output/CurrentPrice_SA_Qtr_CPILevel2_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_CPILevel2_Published20250731.rda")


      load("Data_Output/CurrentPrice_Actual_Qtr_CPILevel1_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_CPIRegional_Published20250731.rda")
      load("Data_Output/CurrentPrice_Actual_Qtr_NonStd_Published20250731.rda")
      load("Data_Output/CurrentPrice_SA_Qtr_CPILevel1_Published20250731.rda")
      load("Data_Output/CurrentPrice_SA_Qtr_CPITradableNonTrad_Published20250731.rda")
      load("Data_Intermediate/Downloaded_Files.rda")     


      load("Data_Intermediate/RAWDATA_XXTA.rda") 

      ##
      ##    Grab the production function output
      ##
      load("Data_Output/Actual_Expected.rda")
      load("Data_Output/Output_Gap.rda")

   ##
   ## Create a weighted average price index
   ##
      House_Prices <- data.table(RAWDATA_XXTA)
      House_Prices$Date <- as.Date(House_Prices$Date, "%Y-%m-%d")
      House_Prices <- House_Prices[,
                                   list(Quantity_Sold = sum(as.numeric(Quantity), na.rm = TRUE),
                                        Weighted_Quantity_Sold = sum(as.numeric(Quantity)*as.numeric(Fisher_PI), na.rm = TRUE),
                                        National_Prices = sum(as.numeric(Quantity)*as.numeric(Fisher_PI), na.rm = TRUE) /  sum(as.numeric(Quantity), na.rm = TRUE)),
                                   by = list(Date)]

      ggplot(House_Prices, 
             aes(x = Date, 
                 y = National_Prices))     +
             geom_line(size =.5) +
             geom_point(size =.3)
  ##
   ## Interpolate fisher ideal price index
   ##

      F_Data <- House_Prices
      y = F_Data$National_Prices[!is.nan(F_Data$National_Prices)]
      x = F_Data$Date[!is.nan(F_Data$National_Prices)]
      z = x + 1
       ##
       ##    Move time to a common basis
       ##
        int1 <- lubridate::interval(lubridate::ymd(z[1]),
                                    lubridate::ymd(z[length(z)]))
        New_Dates <- lubridate::ymd(z[1]) + months(0:(int1 %/% months(1)))
        New_Dates <- New_Dates - 1      
        New_Dates <- New_Dates[month(New_Dates) %in% c(1:12)]
       ##
       ##    Interpolate the Population values
       ##
        House_Prices <- data.frame()
        House_Prices = tryCatch({ Hate <- smooth.spline(x,y)
                          Y <- data.frame(Period = New_Dates,
                                          National_Prices = predict(Hate, as.numeric(New_Dates))$y)
                    }, warning = function(w) {
                 }, error = function(e) {
                 }, finally = {
                 })
                 
         plot(House_Prices$Period, House_Prices$National_Prices)

             
   ##
   ## Step 1: Simplify these names
   ##
      CPI_Level1_Actual    <- CurrentPrice_Actual_Qtr_CPILevel1_Published20250731
      CPI_Level2_Actual    <- CurrentPrice_Actual_Qtr_CPILevel2_Published20250731
      CPI_Level3_Actual    <- CurrentPrice_SA_Qtr_CPILevel3_Published20250731
      CPI_NonStd_Less      <- CurrentPrice_Actual_Qtr_CPINonStd_Less_Published20250731
      Loans_By_Industry    <- Loans_by_Industry20250731
      Deposits_By_Industry <- Deposits_by_Industry20250731
      CPI                  <- CurrentPrice_Actual_Qtr_CPIAllGroup_Published20250731
      Interest_Rates       <- Retail_Interest_Rates20250731[Retail_Interest_Rates20250731$Measure2 == "B1. Floating first mortgage new customer housing rate",c("Period", "Value")]
      Hours_Worked         <- ConstantPrice_Actual_Qtr_PaidHours_Published20250731
      Real_GDP             <- ConstantPrice_SA_Qtr_GDP_Published20250631
      Household_Debt       <- Sector_lending20250731
      
#      Household_Debt <- Household_Debt[((Household_Debt$Measure1 == "Housing & personal consumer ") &
#                                        (Household_Debt$Measure2 == "C. Total (C1+C2)")),]
      Household_Debt <- Household_Debt[((Household_Debt$Measure1 == "Housing ") &
                                        (Household_Debt$Measure2 == "A. Total (A1+A2)")),]


      t(t(unique(CPI_NonStd_Less$CPI_Item)))
      t(t(unique(CPI_Level1_Actual$Group)))
      t(t(unique(CPI_Level2_Actual$Subgroup)))
      t(t(unique(CPI_Level3_Actual$Class)))

      Mapping_Frame <- data.frame(CPI_Item = c("All groups less alcoholic beverages and tobacco group","All groups less central and local government charges","All groups less credit services subgroup","All groups less food group","All groups plus interest","All groups less International travel","All groups less alcoholic beverages subgroup","All groups less cigarettes and tobacco subgroup","All groups less clothing and footwear group","All groups less communication group","All groups less education group","All groups less food group and vehicle fuels","All groups less food group, household energy subgroup and vehicle fuels","All groups less food group, household energy subgroup, vehicle fuels, & government charges","All groups less health group","All groups less household contents and services group","All groups less household energy subgroup and vehicles fuels","All groups less housing","All groups less housing and household utilities group","All groups less housing and household utilities group and credit services subgroup","All groups less miscellaneous goods and services group","All groups less petrol class","All groups less purchasing of new housing class","All groups less recreation and culture group","All groups less rentals for housing subgroup","All groups less transport group","All groups less vehicle fuels"),
                                  Group    = c("Alcoholic beverages and tobacco",NA,NA,"Food",NA,NA,NA,NA,"Clothing and footwear","Communication","Education","Food","Food",NA,"Health","Household contents and services",NA,NA,"Housing and household utilities","Housing and household utilities","Miscellaneous goods and services",NA,NA,"Recreation and culture",NA,"Transport",NA),
                                  Subgroup = c(NA,NA,"Credit services",NA,NA,NA,"Alcoholic beverages","Cigarettes and tobacco",NA,NA,NA,NA,"Household energy",NA,NA,NA,"Household energy",NA,NA,"Credit services",NA,NA,NA,NA,"Actual rentals for housing",NA,NA),
                                  Class    = c(NA,NA,NA,NA,NA,"International air transport",NA,NA,NA,NA,NA,"Petrol","Petrol",NA,NA,NA,"Petrol",NA,NA,NA,NA,"Petrol","Purchase of housing",NA,NA,NA,NA))

      Wonder <- merge(CPI_NonStd_Less,
                      Mapping_Frame,
                      by = c("CPI_Item"))


      Relative_Prices <- merge(CPI_Level1_Actual,
                               Wonder,
                               by= c("Group", "Period"))

      Relative_Prices$Relative_Price <- Relative_Prices$Value.x / Relative_Prices$Value.y


      ggplot(Relative_Prices, 
             aes(x = Period, 
                 y = Relative_Price, 
                 colour = Group))     +
             geom_line(size =.5) +
             geom_point(size =.3) +
             facet_grid(~Group, scales="free") +
             labs(title="Relative Prices\n")

      ##
      ##
      ##
             
      Relative_Prices <- merge(CPI_Level2_Actual ,
                               Wonder[is.na(Wonder$Group) & is.na(Wonder$Class),],
                               by= c("Subgroup", "Period"))

      Relative_Prices$Relative_Price <- Relative_Prices$Value.x / Relative_Prices$Value.y


      ggplot(Relative_Prices, 
             aes(x = Period, 
                 y = Relative_Price, 
                 colour = Subgroup))     +
             geom_line(size =.5) +
             geom_point(size =.3) +
             facet_grid(~Subgroup, scales="free") +
             labs(title="Relative Prices\n")
             
      ##
      ##
      ##
             
      Relative_Prices <- merge(CPI_Level3_Actual ,
                               Wonder[is.na(Wonder$Subgroup),],
                               by= c("Class", "Period"))

      Relative_Prices$Relative_Price <- Relative_Prices$Value.x / Relative_Prices$Value.y


      ggplot(Relative_Prices, 
             aes(x = Period, 
                 y = Relative_Price, 
                 colour = Class))     +
             geom_line(size =.5) +
             geom_point(size =.3) +
             facet_grid(~Class, scales="free") +
             labs(title="Relative Prices\n")
             
             
      ##
      ##
      ##

      ggplot(CurrentPrice_Actual_Qtr_CPIAllGroup_Published20250731, 
             aes(x = Period, 
                 y = Value, 
                 colour = CPI_Item))     +
             geom_line(size =.5) +
             geom_point(size =.3) +
             labs(title="General Prices\n")

   ##
   ## Interpolate the quarterly CPI into monthly measures
   ##
   
      XList <- lapply(unique(CPI$CPI_Item), function(Labour) 
               {  BinVector <- CPI$CPI_Item == Labour
    
                  F_Data <- CPI[BinVector,]
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
                    New_Dates <- New_Dates[month(New_Dates) %in% c(1:12)]
                   ##
                   ##    Interpolate the Population values
                   ##
                    X <- data.frame()
                    X = tryCatch({ Hate <- smooth.spline(x,y)
                                      Y <- data.frame(Period = New_Dates,
                                                      Value = predict(Hate, as.numeric(New_Dates))$y)
                                      Y$CPI_Item <- as.character(Labour)
                                  return(Y)
                                }, warning = function(w) {
                             }, error = function(e) {
                             }, finally = {
                             })
                          return(X)
               })
         InterpCPI <- data.table(do.call(rbind, XList))
         InterpCPI$Value <- as.numeric(InterpCPI$Value)
         plot(InterpCPI$Period, InterpCPI$Value)

   ##
   ## Interpolate the quarterly Labour Measure into monthly measures
   ##
   
      XList <- lapply(unique(Hours_Worked$Industry), function(Labour) 
               {  BinVector <- Hours_Worked$Industry == Labour
    
                  F_Data <- Hours_Worked[BinVector,]
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
                    New_Dates <- New_Dates[month(New_Dates) %in% c(1:12)]
                   ##
                   ##    Interpolate the Population values
                   ##
                    X <- data.frame()
                    X = tryCatch({ Hate <- smooth.spline(x,y)
                                      Y <- data.frame(Period = New_Dates,
                                                      Value = predict(Hate, as.numeric(New_Dates))$y)
                                      Y$Industry <- as.character(Labour)
                                  return(Y)
                                }, warning = function(w) {
                             }, error = function(e) {
                             }, finally = {
                             })
                          return(X)
               })
         InterpHours_Worked <- data.table(do.call(rbind, XList))
         InterpHours_Worked$Hours_Worked <- as.numeric(InterpHours_Worked$Value)
         plot(InterpHours_Worked$Period, InterpHours_Worked$Value)
   ##
   ## Interpolate the quarterly Real_GDP into monthly measures
   ##
   
      XList <- lapply(unique(Real_GDP$Industry), function(Labour) 
               {  BinVector <- Real_GDP$Industry == Labour
    
                  F_Data <- Real_GDP[BinVector,]
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
                    New_Dates <- New_Dates[month(New_Dates) %in% c(1:12)]
                   ##
                   ##    Interpolate the Population values
                   ##
                    X <- data.frame()
                    X = tryCatch({ Hate <- smooth.spline(x,y)
                                      Y <- data.frame(Period = New_Dates,
                                                      Value = predict(Hate, as.numeric(New_Dates))$y)
                                      Y$Industry <- as.character(Labour)
                                  return(Y)
                                }, warning = function(w) {
                             }, error = function(e) {
                             }, finally = {
                             })
                          return(X)
               })
         InterpReal_GDP <- data.table(do.call(rbind, XList))
         InterpReal_GDP$Real_GDP <- as.numeric(InterpReal_GDP$Value)
         plot(InterpReal_GDP$Period, InterpReal_GDP$Value)

 

             

   ##
   ##    Deflate Loans by Industry`
   ##
      Real_Loans_By_Industry <- merge(Loans_By_Industry,
                                      InterpCPI,
                                      by = c("Period"))
      Real_Loans_By_Industry <- merge(Real_Loans_By_Industry,
                                      House_Prices,
                                      by = c("Period"))


#     Real_Loans_By_Industry$Real_Loans_By_Industry <- Real_Loans_By_Industry$Value.x / Real_Loans_By_Industry$Value.y
      Real_Loans_By_Industry$Real_Loans_By_Industry <- Real_Loans_By_Industry$Value.x / Real_Loans_By_Industry$National_Prices

      Real_Loans_By_Industry <- do.call(rbind, lapply(unique(Real_Loans_By_Industry$Industry), function(industry) {
                                                         X <- Real_Loans_By_Industry[Industry == industry,]

                                                         X$Quarterly_PC <- NA
                                                         X$Annual_PC    <- NA
                                                         
                                                         for(i in 2:nrow(X)) X$Quarterly_PC[i] <- ((X$Real_Loans_By_Industry[i]/X$Real_Loans_By_Industry[(i-1)])-1)*100
                                                         for(i in 4:nrow(X)) X$Annual_PC[i]    <- ((X$Real_Loans_By_Industry[i]/X$Real_Loans_By_Industry[(i-3)])-1)*100
                                                         
                                                         return(X)
                                                      }))

      ggplot(Real_Loans_By_Industry[Industry == "Households - Total",], 
             aes(x = Period, 
                 y = Real_Loans_By_Industry, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Constant Price Household Debt\nBase - March 2017\n", 
                  title="Constant Price Household Loans\n",
                  caption = "RBNZ: 'Assets - Loans & Repos by Industry - S34' deflated by CPI") +
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
                   legend.position  = "none")

                      
         ggsave("Graphical_Output/Household Loans - Qty_Fisher_Ideal_Prices.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

      ggplot(Real_Loans_By_Industry[Industry == "Households - Total",], 
             aes(x = Period, 
                 y = Quarterly_PC, 
                 colour = Industry))     +
             geom_smooth(size =1, colour = SPCColours("Red"), span = 0.3) +
             #geom_smooth(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             geom_hline(yintercept = 0, size = 1,  colour = SPCColours("Green")) +
             scale_y_continuous(labels = scales::label_percent(),breaks = seq(from=-2.0, to=2.0, by=.10)) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Monthly Percentage Change", 
                  title="Monthly Growth in Constant Price Household Loans\n",
                  caption = "RBNZ: 'Assets - Loans & Repos by Industry - S34' deflated by CPI") +
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
                   legend.position  = "none")

                      
         ggsave("Graphical_Output/Household Loans - Monthly Change_Fisher_Ideal_Prices.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))



   ##
   ##    Add interest rates
   ##
      Household_Debt <- merge(Real_Loans_By_Industry[Industry == "Households - Total",],
                              InterpCPI,
                              by = c("Period"))
      Household_Debt <- merge(Household_Debt,
                              House_Prices,
                              by = c("Period"))
                              
      Household_Debt$CPIDeflated_Household_Debt    <- Household_Debt$Value.x / Household_Debt$Value.y
      Household_Debt$FisherDeflated_Household_Debt <- Household_Debt$Value.x / Household_Debt$National_Prices.x


      Analytical_Set <- merge(Household_Debt[, c("Period", "CPIDeflated_Household_Debt", "FisherDeflated_Household_Debt", "National_Prices.x")],
                              Interest_Rates,
                              by = "Period")
      names(Analytical_Set)[names(Analytical_Set) == "Value"] <- "Housing_Interest_Rate"

      InterpHours_Worked <- InterpHours_Worked[,
                                               list(Hours_Worked = sum(Hours_Worked, na.rm = TRUE)/1000000),
                                               by = list(Period)]
                                               
      InterpReal_GDP <- InterpReal_GDP[,
                                      list(Real_GDP = sum(Real_GDP, na.rm = TRUE)/1000),
                                      by = list(Period)]

      Analytical_Set <- merge(Analytical_Set,
                              InterpHours_Worked,
                              by = "Period")
      Analytical_Set <- merge(Analytical_Set,
                              InterpReal_GDP,
                              by = "Period")

     ##
     ##  Take the logs
     ##
         Analytical_Set$Mortgages <- log(Analytical_Set$FisherDeflated_Household_Debt)
         Analytical_Set$Interest  <- log(Analytical_Set$Housing_Interest_Rate)
         Analytical_Set$Labour    <- log(Analytical_Set$Hours_Worked)
         Analytical_Set$GDP       <- log(Analytical_Set$Real_GDP)
         Analytical_Set$Log_Prices<- log(Analytical_Set$National_Prices)


##
## tau3, phi2, and phi3 are trend, drift and none
##  significantly different from the critical value means that the data IS stationary 
##

Unit_Root <-ur.df(Analytical_Set$Mortgages, 
                  lags = 12, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)

Unit_Root <-ur.df(Analytical_Set$Interest, 
                  lags = 12, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)


Unit_Root <-ur.df(Analytical_Set$Labour, 
                  lags = 12, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)


Unit_Root <-ur.df(Analytical_Set$Log_Prices, 
                  lags = 12, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)

Unit_Root <-ur.df(Analytical_Set$GDP, 
                  lags = 12, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)

##
##    Mortgages is non stationary without trend, is lag 4
##    Interest is nonstationary without trend, is lag 4
##    Labour is nonstationary with trend, is lag 8
##    Log_Prices is nonstationary without trend, is lag 5
##
      Analytical_Set

      VARselect(data.frame(Analytical_Set$Mortgages,
                           Analytical_Set$Interest,
                           Analytical_Set$Log_Prices,
                           Analytical_Set$Labour), 
                lag.max = 12, 
                type = c("const"))    
      ##
      ##    Do some causality tests
      ##
      
        VAR_Model <- VAR(data.frame(Mortgages = Analytical_Set$Mortgages,
                                    Log_Prices = Analytical_Set$Log_Prices),
                                  p = 12,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Mortgages")  # mortgage demand does not cause prices to change.
        causality(VAR_Model,cause = "Log_Prices") # But price changes do cause mortgage demand to change.
      
      
        VAR_Model <- VAR(data.frame(Mortgages = Analytical_Set$Mortgages,
                                    Labour = Analytical_Set$Labour),
                                  p = 12,
                                  type = "const")
                                  
        causality(VAR_Model,cause = "Mortgages")  # mortgage demand causes labour to change??
        causality(VAR_Model,cause = "Labour")     # But labour demand does not cause mortgage demand to change??
      
      
      
      
      jotest=ca.jo(data.frame(Analytical_Set$Log_Prices,
                              Analytical_Set$Interest,
                              Analytical_Set$Mortgages,
                              Analytical_Set$Labour), 
                              type="trace", 
                              K=4, 
                              ecdet="const", 
                              spec="longrun")
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
                        

















      testStatistics <- jotest@teststat
      criticalValues <- jotest@criticalValues
      
   ##
   ##    Estimate the dynamic component
   ## 
      dynamic_bit <- cajools(jotest,
                         reg.number=NULL)
      summary(dynamic_bit)
      
      dynamic_bit <- alphaols(jotest)
      summary(dynamic_bit)

   ##
   ##    Derive the speed of adjustment, and test its significance
   ##       Taken from here: https://stats.stackexchange.com/questions/96645/finding-significance-levels-for-cointegrating-coefficients-in-cajorls
   ##
   ##    The coefficients on ECT1 are the speeds of adjustment of the regression variable to disequilibrium in the long run position.
   ##
      jotest=ca.jo(data.frame(Analytical_Set$Mortgages,
                           Analytical_Set$Interest,
                           Analytical_Set$GDP,
                           Analytical_Set$Log_Prices,
                           Analytical_Set$Labour), 
                           type="trace", 
                           K=9, 
                           ecdet="const", 
                           spec="longrun")
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
                        


=======================================
                              Model 1  
---------------------------------------
Analytical_Set.Mortgages.l8   -1.00    
                                       
Analytical_Set.Interest.l8    -0.07 *  
                              (0.03)   
Analytical_Set.Log_Prices.l8  -0.69 ***
                              (0.05)   
Analytical_Set.Labour.l8       1.75 ***
                              (0.17)   
constant                      52.89 ***
                              (4.11)   
---------------------------------------
Dummy                          1       
=======================================
*** p < 0.001; ** p < 0.01; * p < 0.05

##
##    These two cointergrating relationships: Mortgages ~ interest + prices + GDP/Labour
##                                            GDP ~ Labour
##

         jotest=ca.jo(data.frame(Analytical_Set$Mortgages,
                                 Analytical_Set$Interest,
                                 Analytical_Set$Log_Prices,
                                 Analytical_Set$GDP), 
                                 type="trace", 
                                 K=8, 
                                 ecdet="const", 
                                 spec="longrun")
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
                        
                        
 
=======================================
                              Model 1  
---------------------------------------
Analytical_Set.Mortgages.l8   -1.00    
                                       
Analytical_Set.Interest.l8    -0.07 *  
                              (0.03)   
Analytical_Set.Log_Prices.l8  -0.69 ***
                              (0.05)   
Analytical_Set.Labour.l8       1.75 ***
                              (0.17)   
constant                      52.89 ***
                              (4.11)   
---------------------------------------
Dummy                          1       
=======================================
*** p < 0.001; ** p < 0.01; * p < 0.05
                       
                       

         jotest=ca.jo(data.frame(Analytical_Set$Labour,
                                 Analytical_Set$GDP), 
                                 type="trace", 
                                 K=12, 
                                 ecdet="const", 
                                 spec="longrun")
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
                        
                         