##
##    Programme:  Clean_GDP.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Gross Domestic Production"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Gross Domestic Production"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    Constant Price Seasonally Adjusted Quarterly GDP
      ##
         ConstantPrice_SA_Qtr_GDP <- All_Data[[Contents$Subject_Link[Contents$Measure == "Series, GDP(P), Chain volume, Seasonally adjusted, ANZSIC06 industry groups (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(ConstantPrice_SA_Qtr_GDP),
                              Industry = as.character(c("Period", ConstantPrice_SA_Qtr_GDP[3,2:length(ConstantPrice_SA_Qtr_GDP)])))
         Rename <- Rename[Industry != "Total All Industries"]
         ConstantPrice_SA_Qtr_GDP <- data.table::melt(ConstantPrice_SA_Qtr_GDP,
                                                      id.var = c("V1"))
                              
         ConstantPrice_SA_Qtr_GDP$Period <- as.Date(paste0(str_sub(ConstantPrice_SA_Qtr_GDP$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q1"), "03",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q2"), "06",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(ConstantPrice_SA_Qtr_GDP$Period) <- month(ConstantPrice_SA_Qtr_GDP$Period) + 1
         ConstantPrice_SA_Qtr_GDP$Period <- ConstantPrice_SA_Qtr_GDP$Period - 1
         
         ConstantPrice_SA_Qtr_GDP$Value  <- as.numeric(str_replace_all(ConstantPrice_SA_Qtr_GDP$value, ",",""))
         ConstantPrice_SA_Qtr_GDP <- ConstantPrice_SA_Qtr_GDP[!is.na(ConstantPrice_SA_Qtr_GDP$Value) & 
                                                              !is.na(ConstantPrice_SA_Qtr_GDP$Period),]

         ConstantPrice_SA_Qtr_GDP <- merge(ConstantPrice_SA_Qtr_GDP,
                                           Rename,
                                           by = c("variable"))
         ConstantPrice_SA_Qtr_GDP <- ConstantPrice_SA_Qtr_GDP[,c("Period", "Industry", "Value")]
         ConstantPrice_SA_Qtr_GDP <- ConstantPrice_SA_Qtr_GDP[order(ConstantPrice_SA_Qtr_GDP$Period, ConstantPrice_SA_Qtr_GDP$Industry)]
         
   ##
   ## Step 3: Error check
   ##
         Plot_Me <- ConstantPrice_SA_Qtr_GDP[,
                                             list(Value = sum(Value, na.rm = TRUE)),
                                             by = list(Period)]
#         plot(Plot_Me$Period, Plot_Me$Value, type = "l")

   ##
   ## Save files our produce some final output of something
   ##
      assign(paste0("ConstantPrice_SA_Qtr_GDP_Published", Publish_Date), ConstantPrice_SA_Qtr_GDP)
      save(list = paste0("ConstantPrice_SA_Qtr_GDP_Published", Publish_Date), 
           file = paste0("Data_Output/ConstantPrice_SA_Qtr_GDP_Published", Publish_Date,".rda"))
##
##    And we're done
##
