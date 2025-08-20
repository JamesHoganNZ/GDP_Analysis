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
                              Class = as.character(c("Period", CurrentPrice_SA_Qtr_CPILevel3[3,2:length(CurrentPrice_SA_Qtr_CPILevel3)])))
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
         CurrentPrice_SA_Qtr_CPILevel3 <- CurrentPrice_SA_Qtr_CPILevel3[,c("Period", "Class", "Value")]
         CurrentPrice_SA_Qtr_CPILevel3 <- CurrentPrice_SA_Qtr_CPILevel3[order(CurrentPrice_SA_Qtr_CPILevel3$Period, CurrentPrice_SA_Qtr_CPILevel3$Class)]
         
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
         CurrentPrice_Actual_Qtr_CPINonStd_Less <- CurrentPrice_Actual_Qtr_CPINonStandard[order(CurrentPrice_Actual_Qtr_CPINonStandard$Period, CurrentPrice_Actual_Qtr_CPINonStandard$CPI_Item)]


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
      ##    CPI Non-standard Tradable & Non-tradable series,Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_SA_Qtr_CPITradableNonTrad <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Non-standard Tradable & Non-tradable series,Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_SA_Qtr_CPITradableNonTrad),
                              CPI_Item = as.character(c("Period", CurrentPrice_SA_Qtr_CPITradableNonTrad[3,2:length(CurrentPrice_SA_Qtr_CPITradableNonTrad)])))
         CurrentPrice_SA_Qtr_CPITradableNonTrad <- data.table::melt(CurrentPrice_SA_Qtr_CPITradableNonTrad,
                                                      id.var = c("V1"))
                              
         CurrentPrice_SA_Qtr_CPITradableNonTrad$Period <- as.Date(paste0(str_sub(CurrentPrice_SA_Qtr_CPITradableNonTrad$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPITradableNonTrad$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPITradableNonTrad$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPITradableNonTrad$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_SA_Qtr_CPITradableNonTrad$Period) <- month(CurrentPrice_SA_Qtr_CPITradableNonTrad$Period) + 1
         CurrentPrice_SA_Qtr_CPITradableNonTrad$Period <- CurrentPrice_SA_Qtr_CPITradableNonTrad$Period - 1
         
         CurrentPrice_SA_Qtr_CPITradableNonTrad$Value  <- as.numeric(str_replace_all(CurrentPrice_SA_Qtr_CPITradableNonTrad$value, ",",""))
         CurrentPrice_SA_Qtr_CPITradableNonTrad <- CurrentPrice_SA_Qtr_CPITradableNonTrad[!is.na(CurrentPrice_SA_Qtr_CPITradableNonTrad$Value) & 
                                                              !is.na(CurrentPrice_SA_Qtr_CPITradableNonTrad$Period),]

         CurrentPrice_SA_Qtr_CPITradableNonTrad <- merge(CurrentPrice_SA_Qtr_CPITradableNonTrad,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_SA_Qtr_CPITradableNonTrad <- CurrentPrice_SA_Qtr_CPITradableNonTrad[,c("Period", "CPI_Item", "Value")]
         CurrentPrice_SA_Qtr_CPITradableNonTrad <- CurrentPrice_SA_Qtr_CPITradableNonTrad[order(CurrentPrice_SA_Qtr_CPITradableNonTrad$Period, CurrentPrice_SA_Qtr_CPITradableNonTrad$CPI_Item)]


      ##
      ##    CPI Level 1 Groups for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_SA_Qtr_CPILevel1 <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 1 Groups for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_SA_Qtr_CPILevel1),
                              Group = as.character(c("Period", CurrentPrice_SA_Qtr_CPILevel1[3,2:length(CurrentPrice_SA_Qtr_CPILevel1)])))
         CurrentPrice_SA_Qtr_CPILevel1 <- data.table::melt(CurrentPrice_SA_Qtr_CPILevel1,
                                                      id.var = c("V1"))
                              
         CurrentPrice_SA_Qtr_CPILevel1$Period <- as.Date(paste0(str_sub(CurrentPrice_SA_Qtr_CPILevel1$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel1$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel1$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel1$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_SA_Qtr_CPILevel1$Period) <- month(CurrentPrice_SA_Qtr_CPILevel1$Period) + 1
         CurrentPrice_SA_Qtr_CPILevel1$Period <- CurrentPrice_SA_Qtr_CPILevel1$Period - 1
         
         CurrentPrice_SA_Qtr_CPILevel1$Value  <- as.numeric(str_replace_all(CurrentPrice_SA_Qtr_CPILevel1$value, ",",""))
         CurrentPrice_SA_Qtr_CPILevel1 <- CurrentPrice_SA_Qtr_CPILevel1[!is.na(CurrentPrice_SA_Qtr_CPILevel1$Value) & 
                                                              !is.na(CurrentPrice_SA_Qtr_CPILevel1$Period),]

         CurrentPrice_SA_Qtr_CPILevel1 <- merge(CurrentPrice_SA_Qtr_CPILevel1,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_SA_Qtr_CPILevel1 <- CurrentPrice_SA_Qtr_CPILevel1[,c("Period", "Group", "Value")]
         CurrentPrice_SA_Qtr_CPILevel1 <- CurrentPrice_SA_Qtr_CPILevel1[order(CurrentPrice_SA_Qtr_CPILevel1$Period, CurrentPrice_SA_Qtr_CPILevel1$Group)]

      ##
      ##    CPI Non-standard Selected Quarterly Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_NonStd <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Non-standard Selected Quarterly Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_NonStd),
                              CPI_Item = as.character(c("Period", CurrentPrice_Actual_Qtr_NonStd[2,2:length(CurrentPrice_Actual_Qtr_NonStd)])))
         CurrentPrice_Actual_Qtr_NonStd <- data.table::melt(CurrentPrice_Actual_Qtr_NonStd,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_NonStd$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_NonStd$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_NonStd$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_NonStd$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_NonStd$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_NonStd$Period) <- month(CurrentPrice_Actual_Qtr_NonStd$Period) + 1
         CurrentPrice_Actual_Qtr_NonStd$Period <- CurrentPrice_Actual_Qtr_NonStd$Period - 1
         
         CurrentPrice_Actual_Qtr_NonStd$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_NonStd$value, ",",""))
         CurrentPrice_Actual_Qtr_NonStd <- CurrentPrice_Actual_Qtr_NonStd[!is.na(CurrentPrice_Actual_Qtr_NonStd$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_NonStd$Period),]

         CurrentPrice_Actual_Qtr_NonStd <- merge(CurrentPrice_Actual_Qtr_NonStd,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_NonStd <- CurrentPrice_Actual_Qtr_NonStd[,c("Period", "CPI_Item", "Value")]
         CurrentPrice_Actual_Qtr_NonStd <- CurrentPrice_Actual_Qtr_NonStd[order(CurrentPrice_Actual_Qtr_NonStd$Period, CurrentPrice_Actual_Qtr_NonStd$CPI_Item)]

      ##
      ##    CPI Regional Groups (Broad Regions) (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPIRegional <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Regional Groups (Broad Regions) (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPIRegional),
                              Region   = as.character(c("Period", CurrentPrice_Actual_Qtr_CPIRegional[2,2:length(CurrentPrice_Actual_Qtr_CPIRegional)])),
                              CPI_Item = as.character(c("Period", CurrentPrice_Actual_Qtr_CPIRegional[3,2:length(CurrentPrice_Actual_Qtr_CPIRegional)])))
                              
         for(i in 2:nrow(Rename))
         {
            Rename[i,2] <- ifelse(((Rename[i,2] == "") & (Rename[(i-1),2] != "")), Rename[(i-1),2], Rename[i,2])
         }
                              
         CurrentPrice_Actual_Qtr_CPIRegional <- data.table::melt(CurrentPrice_Actual_Qtr_CPIRegional,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPIRegional$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPIRegional$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIRegional$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIRegional$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPIRegional$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPIRegional$Period) <- month(CurrentPrice_Actual_Qtr_CPIRegional$Period) + 1
         CurrentPrice_Actual_Qtr_CPIRegional$Period <- CurrentPrice_Actual_Qtr_CPIRegional$Period - 1
         
         CurrentPrice_Actual_Qtr_CPIRegional$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPIRegional$value, ",",""))
         CurrentPrice_Actual_Qtr_CPIRegional <- CurrentPrice_Actual_Qtr_CPIRegional[!is.na(CurrentPrice_Actual_Qtr_CPIRegional$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPIRegional$Period),]

         CurrentPrice_Actual_Qtr_CPIRegional <- merge(CurrentPrice_Actual_Qtr_CPIRegional,
                                                       Rename,
                                                       by = c("variable"))
         CurrentPrice_Actual_Qtr_CPIRegional <- CurrentPrice_Actual_Qtr_CPIRegional[,c("Period","Region", "CPI_Item", "Value")]
         CurrentPrice_Actual_Qtr_CPIRegional <- CurrentPrice_Actual_Qtr_CPIRegional[order(CurrentPrice_Actual_Qtr_CPIRegional$Period, 
                                                                                          CurrentPrice_Actual_Qtr_CPIRegional$Region, 
                                                                                          CurrentPrice_Actual_Qtr_CPIRegional$CPI_Item)]

      ##
      ##    CPI Level 1 Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPILevel1 <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 1 Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPILevel1),
                              Group = as.character(c("Period", CurrentPrice_Actual_Qtr_CPILevel1[2,2:length(CurrentPrice_Actual_Qtr_CPILevel1)])))
         CurrentPrice_Actual_Qtr_CPILevel1 <- data.table::melt(CurrentPrice_Actual_Qtr_CPILevel1,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPILevel1$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPILevel1$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel1$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel1$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel1$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPILevel1$Period) <- month(CurrentPrice_Actual_Qtr_CPILevel1$Period) + 1
         CurrentPrice_Actual_Qtr_CPILevel1$Period <- CurrentPrice_Actual_Qtr_CPILevel1$Period - 1
         
         CurrentPrice_Actual_Qtr_CPILevel1$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPILevel1$value, ",",""))
         CurrentPrice_Actual_Qtr_CPILevel1 <- CurrentPrice_Actual_Qtr_CPILevel1[!is.na(CurrentPrice_Actual_Qtr_CPILevel1$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPILevel1$Period),]

         CurrentPrice_Actual_Qtr_CPILevel1 <- merge(CurrentPrice_Actual_Qtr_CPILevel1,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_CPILevel1 <- CurrentPrice_Actual_Qtr_CPILevel1[,c("Period", "Group", "Value")]
         CurrentPrice_Actual_Qtr_CPILevel1 <- CurrentPrice_Actual_Qtr_CPILevel1[order(CurrentPrice_Actual_Qtr_CPILevel1$Period, CurrentPrice_Actual_Qtr_CPILevel1$Group)]

        
      ##
      ##    CPI Level 2 Subgroups for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_SA_Qtr_CPILevel2 <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 2 Subgroups for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_SA_Qtr_CPILevel2),
                              Subgroup = as.character(c("Period", CurrentPrice_SA_Qtr_CPILevel2[3,2:length(CurrentPrice_SA_Qtr_CPILevel2)])))
         CurrentPrice_SA_Qtr_CPILevel2 <- data.table::melt(CurrentPrice_SA_Qtr_CPILevel2,
                                                      id.var = c("V1"))
                              
         CurrentPrice_SA_Qtr_CPILevel2$Period <- as.Date(paste0(str_sub(CurrentPrice_SA_Qtr_CPILevel2$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel2$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel2$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_SA_Qtr_CPILevel2$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_SA_Qtr_CPILevel2$Period) <- month(CurrentPrice_SA_Qtr_CPILevel2$Period) + 1
         CurrentPrice_SA_Qtr_CPILevel2$Period <- CurrentPrice_SA_Qtr_CPILevel2$Period - 1
         
         CurrentPrice_SA_Qtr_CPILevel2$Value  <- as.numeric(str_replace_all(CurrentPrice_SA_Qtr_CPILevel2$value, ",",""))
         CurrentPrice_SA_Qtr_CPILevel2 <- CurrentPrice_SA_Qtr_CPILevel2[!is.na(CurrentPrice_SA_Qtr_CPILevel2$Value) & 
                                                              !is.na(CurrentPrice_SA_Qtr_CPILevel2$Period),]

         CurrentPrice_SA_Qtr_CPILevel2 <- merge(CurrentPrice_SA_Qtr_CPILevel2,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_SA_Qtr_CPILevel2 <- CurrentPrice_SA_Qtr_CPILevel2[,c("Period", "Subgroup", "Value")]
         CurrentPrice_SA_Qtr_CPILevel2 <- CurrentPrice_SA_Qtr_CPILevel2[order(CurrentPrice_SA_Qtr_CPILevel2$Period, CurrentPrice_SA_Qtr_CPILevel2$Subgroup)]

      ##
      ##    CPI Level 2 Subgroups Tradables and Non-tradables (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 2 Subgroups Tradables and Non-tradables (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad),
                              Subgroup = as.character(c("Period", CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad[3,2:length(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad)])))
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- data.table::melt(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period) <- month(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period) + 1
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period <- CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period - 1
         
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$value, ",",""))
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad[!is.na(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period),]

         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- merge(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad[,c("Period", "Subgroup", "Value")]
         CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad <- CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad[order(CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Period, CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad$Subgroup)]




      ##
      ##    CPI Level 2 Subgroups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_CPILevel2 <- All_Data[[Contents$Subject_Link[Contents$Measure == "CPI Level 2 Subgroups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_CPILevel2),
                              Subgroup = as.character(c("Period", CurrentPrice_Actual_Qtr_CPILevel2[2,2:length(CurrentPrice_Actual_Qtr_CPILevel2)])))
         CurrentPrice_Actual_Qtr_CPILevel2 <- data.table::melt(CurrentPrice_Actual_Qtr_CPILevel2,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_CPILevel2$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_CPILevel2$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_CPILevel2$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_CPILevel2$Period) <- month(CurrentPrice_Actual_Qtr_CPILevel2$Period) + 1
         CurrentPrice_Actual_Qtr_CPILevel2$Period <- CurrentPrice_Actual_Qtr_CPILevel2$Period - 1
         
         CurrentPrice_Actual_Qtr_CPILevel2$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_CPILevel2$value, ",",""))
         CurrentPrice_Actual_Qtr_CPILevel2 <- CurrentPrice_Actual_Qtr_CPILevel2[!is.na(CurrentPrice_Actual_Qtr_CPILevel2$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_CPILevel2$Period),]

         CurrentPrice_Actual_Qtr_CPILevel2 <- merge(CurrentPrice_Actual_Qtr_CPILevel2,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_CPILevel2 <- CurrentPrice_Actual_Qtr_CPILevel2[,c("Period", "Subgroup", "Value")]
         CurrentPrice_Actual_Qtr_CPILevel2 <- CurrentPrice_Actual_Qtr_CPILevel2[order(CurrentPrice_Actual_Qtr_CPILevel2$Period, CurrentPrice_Actual_Qtr_CPILevel2$Subgroup)]




         

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date), CurrentPrice_SA_Qtr_CPILevel3)
            save(list = paste0("CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_CPILevel3_Published", Publish_Date,".rda"))
                 
            assign(paste0("CurrentPrice_Actual_Qtr_CPINonStd_Less_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPINonStd_Less)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPINonStd_Less_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPINonStd_Less_Published", Publish_Date,".rda"))
                 
            assign(paste0("CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPIAllGroup)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPIAllGroup_Published", Publish_Date,".rda"))

            assign(paste0("CurrentPrice_Actual_Qtr_CPILevel1_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPILevel1)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPILevel1_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPILevel1_Published", Publish_Date,".rda"))

            assign(paste0("CurrentPrice_Actual_Qtr_CPIRegional_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPIRegional)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPIRegional_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPIRegional_Published", Publish_Date,".rda"))
           
            assign(paste0("CurrentPrice_Actual_Qtr_NonStd_Published", Publish_Date), CurrentPrice_Actual_Qtr_NonStd)
            save(list = paste0("CurrentPrice_Actual_Qtr_NonStd_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_NonStd_Published", Publish_Date,".rda"))
           
            assign(paste0("CurrentPrice_SA_Qtr_CPILevel1_Published", Publish_Date), CurrentPrice_SA_Qtr_CPILevel1)
            save(list = paste0("CurrentPrice_SA_Qtr_CPILevel1_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_CPILevel1_Published", Publish_Date,".rda"))
           
            assign(paste0("CurrentPrice_SA_Qtr_CPITradableNonTrad_Published", Publish_Date), CurrentPrice_SA_Qtr_CPITradableNonTrad)
            save(list = paste0("CurrentPrice_SA_Qtr_CPITradableNonTrad_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_CPITradableNonTrad_Published", Publish_Date,".rda"))
           
            assign(paste0("CurrentPrice_Actual_Qtr_CPILevel2_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPILevel2)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPILevel2_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPILevel2_Published", Publish_Date,".rda"))
           
            assign(paste0("CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad_Published", Publish_Date), CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad)
            save(list = paste0("CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_CPILevel2TradableNonTrad_Published", Publish_Date,".rda"))
            
            assign(paste0("CurrentPrice_SA_Qtr_CPILevel2_Published", Publish_Date), CurrentPrice_SA_Qtr_CPILevel2)
            save(list = paste0("CurrentPrice_SA_Qtr_CPILevel2_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_CPILevel2_Published", Publish_Date,".rda"))
          
           
##
##    And we're done
##
