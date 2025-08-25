##
##    Programme:  Money Demand Analytical Set.r
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
      names(Real_Loans_By_Industry)[names(Real_Loans_By_Industry) == "Value.x"] <- "Loans_By_Industry"
      names(Real_Loans_By_Industry)[names(Real_Loans_By_Industry) == "Value.y"] <- "CPI_All_Groups"
                                      
      Real_Loans_By_Industry <- merge(Real_Loans_By_Industry,
                                      House_Prices,
                                      by = c("Period"))
      names(Real_Loans_By_Industry)[names(Real_Loans_By_Industry) == "National_Prices"] <- "Fisher_Ideal_House_Prices"

      ##
      ##    Estimate mortgages
      ##
      Real_Loans_By_Industry$Real_Loans_By_Industry <- Real_Loans_By_Industry$Loans_By_Industry / Real_Loans_By_Industry$Fisher_Ideal_House_Prices

      Real_Loans_By_Industry <- do.call(rbind, lapply(unique(Real_Loans_By_Industry$Industry), function(industry) {
                                                         X <- Real_Loans_By_Industry[Industry == industry,]

                                                         X$Quarterly_PC <- NA
                                                         X$Annual_PC    <- NA
                                                         
                                                         for(i in 2:nrow(X)) X$Quarterly_PC[i] <- ((X$Real_Loans_By_Industry[i]/X$Real_Loans_By_Industry[(i-1)])-1)*100
                                                         for(i in 4:nrow(X)) X$Annual_PC[i]    <- ((X$Real_Loans_By_Industry[i]/X$Real_Loans_By_Industry[(i-3)])-1)*100
                                                         
                                                         return(X)
                                                      }))


      Analytical_Set <- merge(Real_Loans_By_Industry,
                              Interest_Rates,
                              by = "Period")
      names(Analytical_Set)[names(Analytical_Set) == "Value"] <- "Floating_First_Mortgage_Interest_Rate"

      InterpHours_Worked <- InterpHours_Worked[,
                                               list(Paid_Hours_Worked = sum(Hours_Worked, na.rm = TRUE)/1000000),
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
         Analytical_Set$Real_Mortgages                <- log(Analytical_Set$Real_Loans_By_Industry)
         Analytical_Set$House_Mortgage_Interest_Rate  <- log(Analytical_Set$Floating_First_Mortgage_Interest_Rate)
         Analytical_Set$Labour_Paid_Hours_Worked      <- log(Analytical_Set$Paid_Hours_Worked)
         Analytical_Set$Log_GDP                       <- log(Analytical_Set$Real_GDP)
         Analytical_Set$Log_Fisher_Ideal_House_Prices <- log(Analytical_Set$Fisher_Ideal_House_Prices)

##
##  Save
##
   Money_Demand_Analytical_Set <- Analytical_Set
   save(Money_Demand_Analytical_Set, file = "Data_Output/Money_Demand_Analytical_Set.rda")
   