##
##    Programme:  Clean_HCE.r
##
##    Objective:  This programme cleans up the data downloaded from Infoshare
##
##    Author:     James Hogan
##
##
   ##
   ##    Clear the memory.
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      load("Data_Intermediate/Publish_Date.rda")
      load("Data_Intermediate/Downloaded_Files.rda")  
                     
   ##
   ##    Load the raw data
   ##
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents$Subject_Link <- str_split_fixed(Contents$Dframe, "XX", n = 2)[,2]
      Contents <- Contents[str_detect(Contents$DataFrames, "RAWDATA_"),]
      
      Contents <- merge(Contents,
                        Downloaded_Files,
                        by = c("Subject_Link"))

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Domestic Household Consumption"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Domestic Household Consumption"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    Constant Price Seasonally Adjusted Quarterly HCE
      ##
         ConstantPrice_SA_Qtr_HCE <- All_Data[[Contents$Subject_Link[Contents$Measure == "Series, GDP(E), Chain Volume, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(ConstantPrice_SA_Qtr_HCE),
                              HCE_Item = as.character(c("Period", ConstantPrice_SA_Qtr_HCE[3,2:length(ConstantPrice_SA_Qtr_HCE)])))
         ConstantPrice_SA_Qtr_HCE <- data.table::melt(ConstantPrice_SA_Qtr_HCE,
                                                      id.var = c("V1"))
                              
         ConstantPrice_SA_Qtr_HCE$Period <- as.Date(paste0(str_sub(ConstantPrice_SA_Qtr_HCE$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_HCE$V1, "Q1"), "03",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_HCE$V1, "Q2"), "06",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_HCE$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(ConstantPrice_SA_Qtr_HCE$Period) <- month(ConstantPrice_SA_Qtr_HCE$Period) + 1
         ConstantPrice_SA_Qtr_HCE$Period <- ConstantPrice_SA_Qtr_HCE$Period - 1
         
         ConstantPrice_SA_Qtr_HCE$Value  <- as.numeric(str_replace_all(ConstantPrice_SA_Qtr_HCE$value, ",",""))
         ConstantPrice_SA_Qtr_HCE <- ConstantPrice_SA_Qtr_HCE[!is.na(ConstantPrice_SA_Qtr_HCE$Value) & 
                                                              !is.na(ConstantPrice_SA_Qtr_HCE$Period),]

         ConstantPrice_SA_Qtr_HCE <- merge(ConstantPrice_SA_Qtr_HCE,
                                           Rename,
                                           by = c("variable"))
         ConstantPrice_SA_Qtr_HCE <- ConstantPrice_SA_Qtr_HCE[,c("Period", "HCE_Item", "Value")]
         ConstantPrice_SA_Qtr_HCE <- ConstantPrice_SA_Qtr_HCE[order(ConstantPrice_SA_Qtr_HCE$Period, ConstantPrice_SA_Qtr_HCE$HCE_Item)]
         
         ##
         ## Step 3: Error check
         ##
               Plot_Me <- ConstantPrice_SA_Qtr_HCE[,
                                                   list(Value = sum(Value, na.rm = TRUE)),
                                                   by = list(Period)]
#               plot(Plot_Me$Period, Plot_Me$Value, type = "l")

      ##
      ##    Nominal Price Seasonally Adjusted Quarterly HCE
      ##
         CurrentPrice_SA_Qtr_HCE <- All_Data[[Contents$Subject_Link[Contents$Measure == "Series, GDP(E), Nominal, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_SA_Qtr_HCE),
                              HCE_Item = as.character(c("Period", CurrentPrice_SA_Qtr_HCE[3,2:length(CurrentPrice_SA_Qtr_HCE)])))
         CurrentPrice_SA_Qtr_HCE <- data.table::melt(CurrentPrice_SA_Qtr_HCE,
                                                      id.var = c("V1"))
                              
         CurrentPrice_SA_Qtr_HCE$Period <- as.Date(paste0(str_sub(CurrentPrice_SA_Qtr_HCE$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_HCE$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_HCE$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_HCE$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_SA_Qtr_HCE$Period) <- month(CurrentPrice_SA_Qtr_HCE$Period) + 1
         CurrentPrice_SA_Qtr_HCE$Period <- CurrentPrice_SA_Qtr_HCE$Period - 1
         
         CurrentPrice_SA_Qtr_HCE$Value  <- as.numeric(str_replace_all(CurrentPrice_SA_Qtr_HCE$value, ",",""))
         CurrentPrice_SA_Qtr_HCE <- CurrentPrice_SA_Qtr_HCE[!is.na(CurrentPrice_SA_Qtr_HCE$Value) & 
                                                              !is.na(CurrentPrice_SA_Qtr_HCE$Period),]

         CurrentPrice_SA_Qtr_HCE <- merge(CurrentPrice_SA_Qtr_HCE,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_SA_Qtr_HCE <- CurrentPrice_SA_Qtr_HCE[,c("Period", "HCE_Item", "Value")]
         CurrentPrice_SA_Qtr_HCE <- CurrentPrice_SA_Qtr_HCE[order(CurrentPrice_SA_Qtr_HCE$Period, CurrentPrice_SA_Qtr_HCE$HCE_Item)]
         
         ##
         ## Step 3: Error check
         ##
               Plot_Me <- CurrentPrice_SA_Qtr_HCE[,
                                                   list(Value = sum(Value, na.rm = TRUE)),
                                                   by = list(Period)]
#               plot(Plot_Me$Period, Plot_Me$Value, type = "l")
      ##
      ##    Estimate the IPD
      ##
            IPD_SA_Qtr_HCE <- merge(CurrentPrice_SA_Qtr_HCE,
                                    ConstantPrice_SA_Qtr_HCE,
                                    by = c("Period", "HCE_Item"))
            IPD_SA_Qtr_HCE$Implicit_Price_Deflator <-   IPD_SA_Qtr_HCE$Value.x / IPD_SA_Qtr_HCE$Value.y                      
            names(IPD_SA_Qtr_HCE) = c("Period", "HCE_Item", "Current_Price", "Constant_Price", "Implicit_Price_Deflator")


            Plot_Me1 <- CurrentPrice_SA_Qtr_HCE[,
                                                list(Value = sum(Value, na.rm = TRUE)),
                                                by = list(Period)]
            Plot_Me2 <- ConstantPrice_SA_Qtr_HCE[,
                                                list(Value = sum(Value, na.rm = TRUE)),
                                                by = list(Period)]
            Plot_Me3 <- merge(Plot_Me1,
                              Plot_Me2,
                              by = c("Period"))
            Plot_Me3$Implicit_Price_Deflator <- Plot_Me3$Value.x / Plot_Me3$Value.y                      

#            plot(Plot_Me3$Period, Plot_Me3$Implicit_Price_Deflator, type = "l")

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("ConstantPrice_SA_Qtr_HCE_Published", Publish_Date), ConstantPrice_SA_Qtr_HCE)
            save(list = paste0("ConstantPrice_SA_Qtr_HCE_Published", Publish_Date), 
                 file = paste0("Data_Output/ConstantPrice_SA_Qtr_HCE_Published", Publish_Date,".rda"))
                 
            assign(paste0("CurrentPrice_SA_Qtr_HCE_Published", Publish_Date), ConstantPrice_SA_Qtr_HCE)
            save(list = paste0("CurrentPrice_SA_Qtr_HCE_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_HCE_Published", Publish_Date,".rda"))
                 
            assign(paste0("IPD_SA_Qtr_HCE_Published", Publish_Date), ConstantPrice_SA_Qtr_HCE)
            save(list = paste0("IPD_SA_Qtr_HCE_Published", Publish_Date), 
                 file = paste0("Data_Output/IPD_SA_Qtr_HCE_Published", Publish_Date,".rda"))
           
##
##    And we're done
##
