##
##    Programme:  Clean_GDPI.r
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

      All_Data <- lapply(Contents$DataFrames[Contents$Focus == "Domestic Income Flows"], function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link[Contents$Focus == "Domestic Income Flows"], side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    This ones a bit more complex designed
      ##
         GDPI <- data.table(All_Data[[Contents$Subject_Link[Contents$Measure == "GDP(I), current prices (Qrtly-Mar/Jun/Sep/Dec)"]]])
         
         GDPI$ID <- 1:nrow(GDPI)
         
         Dimensions <- data.table::melt(GDPI[1:4,],
                                        id.var = c("ID"))         
         Dimensions <- data.table::dcast(Dimensions,
                                         variable ~ ID,
                                         value.var = c("value"))         
         Dimensions[1,3] <- Dimensions[2,3]
         Dimensions[1,4] <- Dimensions[2,4]
         Dimensions[1,5] <- Dimensions[2,5]

         Dimensions[1,3] <- "Period"
         Dimensions[1,4] <- "Period"
         Dimensions[1,5] <- "Period"

                                         
         for(i in 2:nrow(Dimensions))
         {
            Dimensions[i,2] <- ifelse(((Dimensions[i,2] == "") & (Dimensions[(i-1),2] != "")), Dimensions[(i-1),2], Dimensions[i,2])
            Dimensions[i,3] <- ifelse(((Dimensions[i,3] == "") & (Dimensions[(i-1),3] != "")), Dimensions[(i-1),3], Dimensions[i,3])
            Dimensions[i,4] <- ifelse(((Dimensions[i,4] == "") & (Dimensions[(i-1),4] != "")), Dimensions[(i-1),4], Dimensions[i,4])
            Dimensions[i,5] <- ifelse(((Dimensions[i,5] == "") & (Dimensions[(i-1),5] != "")), Dimensions[(i-1),5], Dimensions[i,5])
         }

         Data <- data.table::melt(GDPI[5:nrow(GDPI),],
                                  id.var = c("ID"))         

         Together <- merge(Data,
                           Dimensions,
                           by = c("variable"))
                           
         ##
         ##    Dates & Data
         ##
            Dates <- Together[`2` == "Period"]
            Data  <- Together[(`2` != "Period") & (value != "..") & (value != "")]
            Data$value <- as.numeric(str_replace_all(Data$value, ",",""))


            Dates$Period <- as.Date(paste0(str_sub(Dates$value,start = 1, end = 4),"-",
                                    ifelse(str_detect(Dates$value, "Q1"), "03",
                                    ifelse(str_detect(Dates$value, "Q2"), "06",
                                    ifelse(str_detect(Dates$value, "Q3"), "09","12"))), "-01"), "%Y-%m-%d")
                                                               
            month(Dates$Period) <- month(Dates$Period) + 1
            Dates$Period <- Dates$Period - 1

            Dates <- Dates[!is.na(Dates$Period),]

         ##
         ##    Bring it together
         ##
            Together <- merge(Dates[,c("ID", "Period")],
                              Data,
                              by = c("ID"))
            Together <- Together[`4` != 'Total']
            CurrentPrice_Actual_Qtr_GDPI <- Together[`2` == "Actual", c(7,8,2,4)]
            CurrentPrice_SA_Qtr_GDPI     <- Together[`2` == "Seasonally Adjusted", c(7,8,2,4)]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("CurrentPrice_Actual_Qtr_GDPI_Published", Publish_Date), CurrentPrice_Actual_Qtr_GDPI)
            save(list = paste0("CurrentPrice_Actual_Qtr_GDPI_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_Actual_Qtr_GDPI_Published", Publish_Date,".rda"))

            assign(paste0("CurrentPrice_SA_Qtr_GDPI_Published", Publish_Date), CurrentPrice_SA_Qtr_GDPI)
            save(list = paste0("CurrentPrice_SA_Qtr_GDPI_Published", Publish_Date), 
                 file = paste0("Data_Output/CurrentPrice_SA_Qtr_GDPI_Published", Publish_Date,".rda"))

           
##
##    And we're done
##
