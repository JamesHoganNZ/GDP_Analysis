##
##    Programme:  Clean_Unemployment.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Unemployment"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Unemployment"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##   Household labour force survey data
      ##
         ConstantPrice_SA_Qtr_GDP <- All_Data[[Contents$Subject_Link[Contents$Measure == "Labour Force Status by Sex by Age Group (Qrtly-Mar/Jun/Sep/Dec)"]]]
         Rename <- data.table(variable = names(ConstantPrice_SA_Qtr_GDP),
                              Gender = as.character(c("Period", ConstantPrice_SA_Qtr_GDP[2,2:length(ConstantPrice_SA_Qtr_GDP)])),
                              Age = as.character(c("Period", ConstantPrice_SA_Qtr_GDP[3,2:length(ConstantPrice_SA_Qtr_GDP)])),
                              Measure = as.character(c("Period", ConstantPrice_SA_Qtr_GDP[4,2:length(ConstantPrice_SA_Qtr_GDP)])))
                              
         for (i in 2:nrow(Rename)) 
         {
             Rename$Gender[i] <- ifelse(((Rename$Gender[i] == "") & (Rename$Gender[(i-1)] != "")),  Rename$Gender[(i-1)], Rename$Gender[i])
             Rename$Age[i] <- ifelse(((Rename$Age[i] == "") & (Rename$Age[(i-1)] != "")),  Rename$Age[(i-1)], Rename$Age[i])
             Rename$Measure[i] <- ifelse(((Rename$Measure[i] == "") & (Rename$Measure[(i-1)] != "")),  Rename$Measure[(i-1)], Rename$Measure[i])
         }

         ConstantPrice_SA_Qtr_GDP <- data.table::melt(ConstantPrice_SA_Qtr_GDP,
                                                      id.var = c("V1"))
#                                                      id.var = c("V1", "V2", "V3", "V4"))
                              
         ConstantPrice_SA_Qtr_GDP$Period <- as.Date(paste0(str_sub(ConstantPrice_SA_Qtr_GDP$V1,start = 1, end = 4),"-",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q1"), "03",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q2"), "06",
                                                           ifelse(str_detect(ConstantPrice_SA_Qtr_GDP$V1, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                           
         month(ConstantPrice_SA_Qtr_GDP$Period) <- month(ConstantPrice_SA_Qtr_GDP$Period) + 1
         ConstantPrice_SA_Qtr_GDP$Period <- ConstantPrice_SA_Qtr_GDP$Period - 1
         
         ConstantPrice_SA_Qtr_GDP$Value  <- as.numeric(str_replace_all(ConstantPrice_SA_Qtr_GDP$value, "\\.\\.",""))
         ConstantPrice_SA_Qtr_GDP <- ConstantPrice_SA_Qtr_GDP[!is.na(ConstantPrice_SA_Qtr_GDP$Value) & 
                                                              !is.na(ConstantPrice_SA_Qtr_GDP$Period),]

         ConstantPrice_SA_Qtr_GDP <- merge(ConstantPrice_SA_Qtr_GDP,
                                           Rename,
                                           by = c("variable"))
         Household_Labour_Force_Survey <- ConstantPrice_SA_Qtr_GDP[,c("Period", "Gender", "Age", "Measure", "Value")]
         Household_Labour_Force_Survey <- Household_Labour_Force_Survey[order(Household_Labour_Force_Survey$Period, 
                                                                              Household_Labour_Force_Survey$Gender, 
                                                                              Household_Labour_Force_Survey$Age, 
                                                                              Household_Labour_Force_Survey$Measure)]

   ##
   ## Save files our produce some final output of something
   ##
      assign(paste0("Household_Labour_Force_Survey_Published", Publish_Date), Household_Labour_Force_Survey)
      save(list = paste0("Household_Labour_Force_Survey_Published", Publish_Date), 
           file = paste0("Data_Output/Household_Labour_Force_Survey_Published", Publish_Date,".rda"))
##
##    And we're done
##
