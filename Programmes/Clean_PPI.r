##
##    Programme:  Clean_PPI.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Producers Prices"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Producers Prices"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         CurrentPrice_Actual_Qtr_PPILevel1 <- All_Data[[Contents$Subject_Link[Contents$Measure == "Outputs (ANZSIC06) - NZSIOC level 1, Base: Dec. 2010 quarter (=1000) (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(CurrentPrice_Actual_Qtr_PPILevel1),
                              PPI_Item = as.character(c("Period", CurrentPrice_Actual_Qtr_PPILevel1[2,2:length(CurrentPrice_Actual_Qtr_PPILevel1)])))
         CurrentPrice_Actual_Qtr_PPILevel1 <- data.table::melt(CurrentPrice_Actual_Qtr_PPILevel1,
                                                      id.var = c("V1"))
                              
         CurrentPrice_Actual_Qtr_PPILevel1$Period <- as.Date(paste0(str_sub(CurrentPrice_Actual_Qtr_PPILevel1$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_PPILevel1$V1, "Q1"), "03",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_PPILevel1$V1, "Q2"), "06",
                                                           ifelse(str_detect(CurrentPrice_Actual_Qtr_PPILevel1$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(CurrentPrice_Actual_Qtr_PPILevel1$Period) <- month(CurrentPrice_Actual_Qtr_PPILevel1$Period) + 1
         CurrentPrice_Actual_Qtr_PPILevel1$Period <- CurrentPrice_Actual_Qtr_PPILevel1$Period - 1
         
         CurrentPrice_Actual_Qtr_PPILevel1$Value  <- as.numeric(str_replace_all(CurrentPrice_Actual_Qtr_PPILevel1$value, ",",""))
         CurrentPrice_Actual_Qtr_PPILevel1 <- CurrentPrice_Actual_Qtr_PPILevel1[!is.na(CurrentPrice_Actual_Qtr_PPILevel1$Value) & 
                                                              !is.na(CurrentPrice_Actual_Qtr_PPILevel1$Period),]

         CurrentPrice_Actual_Qtr_PPILevel1 <- merge(CurrentPrice_Actual_Qtr_PPILevel1,
                                           Rename,
                                           by = c("variable"))
         CurrentPrice_Actual_Qtr_PPILevel1 <- CurrentPrice_Actual_Qtr_PPILevel1[,c("Period", "PPI_Item", "Value")]
         CurrentPrice_Actual_Qtr_PPILevel1 <- CurrentPrice_Actual_Qtr_PPILevel1[order(CurrentPrice_Actual_Qtr_PPILevel1$Period, CurrentPrice_Actual_Qtr_PPILevel1$PPI_Item)]
         

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("CurrentPrice_Actual_Qtr_PPILevel1_Published", Publish_Date), CurrentPrice_Actual_Qtr_PPILevel1)
            save(list = paste0("CurrentPrice_Actual_Qtr_PPILevel1_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_PPILevel1_Published", Publish_Date,".rda"))
           
##
##    And we're done
##
