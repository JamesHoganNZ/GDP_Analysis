##
##    Programme:  Clean_CPI.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Consumer Prices"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Consumer Prices"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_SA_Qtr_CPILevel3 <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_SA_Qtr_CPILevel3),
                              CPI_Item = as.character(c("Period", CurrentPrice_SA_Qtr_CPILevel3[3,2:length(CurrentPrice_SA_Qtr_CPILevel3)])))
         CurrentPrice_SA_Qtr_CPILevel3 <- data.table::melt(CurrentPrice_SA_Qtr_CPILevel3,
                                                      id.var = c("V1"))
                              
         CurrentPrice_SA_Qtr_CPILevel3$Period <- as.Date(paste0(str_sub(CurrentPrice_SA_Qtr_CPILevel3$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel3$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel3$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel3$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_SA_Qtr_CPILevel3$Period) <- month(CurrentPrice_SA_Qtr_CPILevel3$Period) + 1
         CurrentPrice_SA_Qtr_CPILevel3$Period <- CurrentPrice_SA_Qtr_CPILevel3$Period - 1
         
         CurrentPrice_SA_Qtr_CPILevel3$Value  <- as.numeric(str_replace_all(CurrentPrice_SA_Qtr_CPILevel3$value, ",",""))
         CurrentPrice_SA_Qtr_CPILevel3 <- CurrentPrice_SA_Qtr_CPILevel3[!is.na(CurrentPrice_SA_Qtr_CPILevel3$Value) & 
                                                              !is.na(CurrentPrice_SA_Qtr_CPILevel3$Period),]

         CurrentPrice_SA_Qtr_CPILevel3 <- merge(CurrentPrice_SA_Qtr_CPILevel3,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_SA_Qtr_CPILevel3 <- CurrentPrice_SA_Qtr_CPILevel3[,c("Period", "CPI_Item", "Value")]
         CurrentPrice_SA_Qtr_CPILevel3 <- CurrentPrice_SA_Qtr_CPILevel3[order(CurrentPrice_SA_Qtr_CPILevel3$Period, CurrentPrice_SA_Qtr_CPILevel3$CPI_Item)]
         

      ##
      ##    CPI Non-standard All Groups Less/Plus Selected Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPINonStandard <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Non-standard All Groups Less/Plus Selected Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPINonStandard),
                              CPI_Item = as.character(c("Period", CurrentPrice_Actual_Qtr_CPINonStandard[2,2:length(CurrentPrice_Actual_Qtr_CPINonStandard)])))
         CurrentPrice_Actual_Qtr_CPINonStandard <- data.table::melt(CurrentPrice_Actual_Qtr_CPINonStandard,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPINonStandard$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPINonStandard$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPINonStandard$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPINonStandard$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPINonStandard$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPINonStandard$Period) <- month(CurrentPrice_Actual_Qtr_CPINonStandard$Period) + 1
         CurrentPrice_Actual_Qtr_CPINonStandard$Period <- CurrentPrice_Actual_Qtr_CPINonStandard$Period - 1
         
         CurrentPrice_Actual_Qtr_CPINonStandard$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPINonStandard$value, ",",""))
         CurrentPrice_Actual_Qtr_CPINonStandard <- CurrentPrice_Actual_Qtr_CPINonStandard[!is.na(CurrentPrice_Actual_Qtr_CPINonStandard$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPINonStandard$Period),]

         CurrentPrice_Actual_Qtr_CPINonStandard <- merge(CurrentPrice_Actual_Qtr_CPINonStandard,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_CPINonStandard <- CurrentPrice_Actual_Qtr_CPINonStandard[,c("Period", "CPI_Item", "Value")]
         CurrentPrice_Actual_Qtr_CPINonStandard <- CurrentPrice_Actual_Qtr_CPINonStandard[order(CurrentPrice_Actual_Qtr_CPINonStandard$Period, CurrentPrice_Actual_Qtr_CPINonStandard$CPI_Item)]
         

      ##
      ##    CPI All Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPIAllGroup <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI All Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPIAllGroup),
                              CPI_Item = as.character(c("Period", CurrentPrice_Actual_Qtr_CPIAllGroup[2,2:length(CurrentPrice_Actual_Qtr_CPIAllGroup)])))
         CurrentPrice_Actual_Qtr_CPIAllGroup <- data.table::melt(CurrentPrice_Actual_Qtr_CPIAllGroup,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPIAllGroup$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPIAllGroup$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIAllGroup$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIAllGroup$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIAllGroup$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPIAllGroup$Period) <- month(CurrentPrice_Actual_Qtr_CPIAllGroup$Period) + 1
         CurrentPrice_Actual_Qtr_CPIAllGroup$Period <- CurrentPrice_Actual_Qtr_CPIAllGroup$Period - 1
         
         CurrentPrice_Actual_Qtr_CPIAllGroup$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPIAllGroup$value, ",",""))
         CurrentPrice_Actual_Qtr_CPIAllGroup <- CurrentPrice_Actual_Qtr_CPIAllGroup[!is.na(CurrentPrice_Actual_Qtr_CPIAllGroup$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPIAllGroup$Period),]

         CurrentPrice_Actual_Qtr_CPIAllGroup <- merge(CurrentPrice_Actual_Qtr_CPIAllGroup,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_CPIAllGroup <- CurrentPrice_Actual_Qtr_CPIAllGroup[,c("Period", "CPI_Item", "Value")]
         CurrentPrice_Actual_Qtr_CPIAllGroup <- CurrentPrice_Actual_Qtr_CPIAllGroup[order(CurrentPrice_Actual_Qtr_CPIAllGroup$Period, CurrentPrice_Actual_Qtr_CPIAllGroup$CPI_Item)]
         

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date), CurrentPrice_SA_Qtr_CPILevel3)
            save(list = paste0("CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date,".rda"))
                 
            assign(paste0("CurrentPrice_Actual_Qtr_CPINonStandard_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPINonStandard)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPINonStandard_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPINonStandard_Published", Publish_Date,".rda"))
                 
            assign(paste0("CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPIAllGroup)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date,".rda"))
           
##
##    And we're done
##
