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

      ##
      ##    Grab the production function output
      ##
      load("Data_Output/Actual_Expected.rda")
      load("Data_Output/Output_Gap.rda")

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
      Real_Loans_By_Industry$Real_Loans_By_Industry <- Real_Loans_By_Industry$Value.x / Real_Loans_By_Industry$Value.y

      ggplot(Real_Loans_By_Industry[Industry == "Households - Total",], 
             aes(x = Period, 
                 y = Real_Loans_By_Industry, 
                 colour = Industry))     +
             geom_line(size =.5) +
             geom_point(size =.3) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(title="Real Household Loans\n") +
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



   ##
   ##    Add interest rates
   ##
      Household_Debt <- merge(Household_Debt,
                              InterpCPI,
                              by = c("Period"))
                              
      Household_Debt$Real_Household_Debt <- Household_Debt$Value.x / Household_Debt$Value.y

      Analytical_Set <- merge(Household_Debt[, c("Period", "Real_Household_Debt")],
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
         Analytical_Set$Mortgages <- log(Analytical_Set$Real_Household_Debt)
         Analytical_Set$Interest  <- log(Analytical_Set$Housing_Interest_Rate)
         Analytical_Set$Labour    <- log(Analytical_Set$Hours_Worked)
         Analytical_Set$GDP       <- log(Analytical_Set$Real_GDP)


##
## tau3, phi2, and phi3 are trend, drift and none
##  significantly different from the critical value means that the data IS stationary 
##

Unit_Root <-ur.df(Analytical_Set$Real_GDP, 
                  lags = 24, 
                  selectlags = "AIC", 
                  type = "trend")      
summary(Unit_Root)
plot.ts(Unit_Root@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(Unit_Root@res)


##
##    Real_Household_Debt is stationary around a trend
##    Housing_Interest_Rate is stationary around a trend
##    Hours_Worked is nonstationary around a trend
##

   OLS <- lm(Real_Household_Debt ~ Housing_Interest_Rate + Hours_Worked + Real_GDP, data=Analytical_Set)
   summary(OLS)

   OLS <- lm(Mortgages ~ Interest + Labour + GDP, data=Analytical_Set)
   summary(OLS)





##
##    WTF...
##
   testicles <- merge(Real_Loans_By_Industry[Industry == "Households - Total", c("Period", "Industry", "Value.x", "Real_Loans_By_Industry")],
                      Household_Debt[,c("Period", "Measure1", "Value.x", "Real_Household_Debt")],
                      by = c("Period"))






   testicles <- merge(Real_Loans_By_Industry[Real_Loans_By_Industry$Industry == "Households - Total", c("Period", "Industry", "Value.x", "Real_Loans_By_Industry")],
                      Household_Debt[,c("Period", "Measure1", "Value.x", "Real_Household_Debt")],
                      by = c("Period"))

