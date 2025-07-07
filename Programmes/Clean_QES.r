##
##    Programme:  Clean_QES.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Labour Inputs"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Labour Inputs"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    Constant Price Seasonally Adjusted Quarterly GDP
      ##
         ConstantPrice_Actual_Qtr_PaidHours <- All_Data[[Contents$Subject_Link[Contents$Measure == "Total Paid Hours by Industry (ANZSIC06) (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(ConstantPrice_Actual_Qtr_PaidHours),
                              Industry = as.character(c("Period", ConstantPrice_Actual_Qtr_PaidHours[2,2:length(ConstantPrice_Actual_Qtr_PaidHours)])))
         Rename <- Rename[Industry != "Total All Industries"]
         ConstantPrice_Actual_Qtr_PaidHours <- data.table::melt(ConstantPrice_Actual_Qtr_PaidHours,
                                                      id.var = c("V1"))
                                                      
         ConstantPrice_Actual_Qtr_PaidHours$Period <- as.Date(paste0(str_sub(ConstantPrice_Actual_Qtr_PaidHours$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(ConstantPrice_Actual_Qtr_PaidHours$V1, "Q1"), "03",
                                                           ifelse(str_detect(ConstantPrice_Actual_Qtr_PaidHours$V1, "Q2"), "06",
                                                           ifelse(str_detect(ConstantPrice_Actual_Qtr_PaidHours$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(ConstantPrice_Actual_Qtr_PaidHours$Period) <- month(ConstantPrice_Actual_Qtr_PaidHours$Period) + 1
         ConstantPrice_Actual_Qtr_PaidHours$Period <- ConstantPrice_Actual_Qtr_PaidHours$Period - 1
         
         ConstantPrice_Actual_Qtr_PaidHours$Value  <- as.numeric(str_replace_all(ConstantPrice_Actual_Qtr_PaidHours$value, ",",""))
         ConstantPrice_Actual_Qtr_PaidHours <- ConstantPrice_Actual_Qtr_PaidHours[!is.na(ConstantPrice_Actual_Qtr_PaidHours$Value) & 
                                                              !is.na(ConstantPrice_Actual_Qtr_PaidHours$Period),]

         ConstantPrice_Actual_Qtr_PaidHours <- merge(ConstantPrice_Actual_Qtr_PaidHours,
                                           Rename,
                                           by = c("variable"))
         ConstantPrice_Actual_Qtr_PaidHours <- ConstantPrice_Actual_Qtr_PaidHours[,c("Period", "Industry", "Value")]
         ConstantPrice_Actual_Qtr_PaidHours <- ConstantPrice_Actual_Qtr_PaidHours[order(ConstantPrice_Actual_Qtr_PaidHours$Period, ConstantPrice_Actual_Qtr_PaidHours$Industry)]
         
   ##
   ## Step 3: Error check
   ##
         Plot_Me <- ConstantPrice_Actual_Qtr_PaidHours[,
                                             list(Value = sum(Value, na.rm = TRUE)),
                                             by = list(Period)]
         #plot(Plot_Me$Period, Plot_Me$Value, type = "l")

   ##
   ## Save files our produce some final output of something
   ##
      assign(paste0("ConstantPrice_Actual_Qtr_PaidHours_Published", Publish_Date), ConstantPrice_Actual_Qtr_PaidHours)
      save(list = paste0("ConstantPrice_Actual_Qtr_PaidHours_Published", Publish_Date), 
           file = paste0("Data_Output/ConstantPrice_Actual_Qtr_PaidHours_Published", Publish_Date,".rda"))
##
##    And we're done
##
