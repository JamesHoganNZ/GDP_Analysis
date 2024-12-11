##
##    Programme:  Clean_GFKF.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Domestic Productive Investment"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Domestic Productive Investment"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
      ##
         ConstantPrice_SA_Qtr_GFKF <- All_Data[[Contents$Subject_Link[Contents$Measure == "Series, GDP(E), Chain volume, Seasonally adjusted, Asset type (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(ConstantPrice_SA_Qtr_GFKF),
                              GFKF_Item = as.character(c("Period", ConstantPrice_SA_Qtr_GFKF[3,2:length(ConstantPrice_SA_Qtr_GFKF)])))
         ConstantPrice_SA_Qtr_GFKF <- data.table::melt(ConstantPrice_SA_Qtr_GFKF,
                                                      id.var = c("V1"))
                              
         ConstantPrice_SA_Qtr_GFKF$Period <- as.Date(paste0(str_sub(ConstantPrice_SA_Qtr_GFKF$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GFKF$V1, "Q1"), "03",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GFKF$V1, "Q2"), "06",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GFKF$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(ConstantPrice_SA_Qtr_GFKF$Period) <- month(ConstantPrice_SA_Qtr_GFKF$Period) + 1
         ConstantPrice_SA_Qtr_GFKF$Period <- ConstantPrice_SA_Qtr_GFKF$Period - 1
         
         ConstantPrice_SA_Qtr_GFKF$Value  <- as.numeric(str_replace_all(ConstantPrice_SA_Qtr_GFKF$value, ",",""))
         ConstantPrice_SA_Qtr_GFKF <- ConstantPrice_SA_Qtr_GFKF[!is.na(ConstantPrice_SA_Qtr_GFKF$Value) & 
                                                              !is.na(ConstantPrice_SA_Qtr_GFKF$Period),]

         ConstantPrice_SA_Qtr_GFKF <- merge(ConstantPrice_SA_Qtr_GFKF,
                                           Rename,
                                           by = c("variable"))
         ConstantPrice_SA_Qtr_GFKF <- ConstantPrice_SA_Qtr_GFKF[,c("Period", "GFKF_Item", "Value")]
         ConstantPrice_SA_Qtr_GFKF <- ConstantPrice_SA_Qtr_GFKF[order(ConstantPrice_SA_Qtr_GFKF$Period, ConstantPrice_SA_Qtr_GFKF$GFKF_Item)]
         

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("ConstantPrice_SA_Qtr_GFKF_Published", Publish_Date), ConstantPrice_SA_Qtr_GFKF)
            save(list = paste0("ConstantPrice_SA_Qtr_GFKF_Published", Publish_Date), 
                 file = paste0("Data_Output/ConstantPrice_SA_Qtr_GFKF_Published", Publish_Date,".rda"))
           
##
##    And we're done
##
