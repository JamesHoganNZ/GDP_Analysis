##
##    Programme:  Clean_institutional_Sector_Accounts.r
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
   ##    Load the raw data
   ##
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents$Subject_Link <- str_split_fixed(Contents$Dframe, "XX", n = 2)[,2]
      Contents <- Contents[str_detect(Contents$DataFrames, "isal"),]
      
      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link, side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##    na-isal-june-2025-quarter-consolidated-accounts
      ##
         Dset <- All_Data[["na-isal-june-2025-quarter-consolidated-accounts"]]
         Dset <- Dset[, c("Series_reference", "period", "value", "Status", "seasonality", "SNA_Account", "Transaction", "Transaction_Label", "Sector", "Sector_name")]
         Dset$Actual_Seasadj <- ifelse(Dset$seasonality == "A", "Actual",
                              ifelse(Dset$seasonality == "S", "Seasonally_Adjusted", "UNKNOWN"))
                              
         Dset$Period <- as.Date(paste0(Dset$period, "-01"), "%Y.%m-%d")
         month(Dset$Period) <- month(Dset$Period) + 1
         Dset$Period <- Dset$Period - 1

         Dset$Value  <- as.numeric(str_replace_all(Dset$value, ",",""))

         ISA_Consolidated_Accounts <- Dset[,c("Actual_Seasadj","Series_reference", "Status",  "SNA_Account", "Transaction", "Transaction_Label", "Sector", "Sector_name", "Period", "Value")]

      ##
      ##    na-isal-june-2025-quarter-consolidated-accounts
      ##
         Dset <- All_Data[["na-isal-june-2025-quarter-institutional-sector-accounts"]]
         Dset <- Dset[, c("Series_reference","period","value","seasonality","SNA_Account","Transaction","Transaction_Label","Asset_type","Asset_type_label","Sector","Sector_name")]
         
         Dset$Actual_Seasadj <- ifelse(Dset$seasonality == "A", "Actual",
                              ifelse(Dset$seasonality == "S", "Seasonally_Adjusted", "UNKNOWN"))
                              
         Dset$Period <- as.Date(paste0(Dset$period, "-01"), "%Y.%m-%d")
         month(Dset$Period) <- month(Dset$Period) + 1
         Dset$Period <- Dset$Period - 1
         
         Dset$Value  <- as.numeric(str_replace_all(Dset$value, ",",""))

         Institutional_Sector_Accounts <- Dset[,c("Series_reference","Actual_Seasadj","SNA_Account","Transaction","Transaction_Label","Asset_type","Asset_type_label","Sector","Sector_name","Period","Value")]

      ##
      ##    Supplementary Table 1-5A
      ##
         Dset <- All_Data[["na-isal-june-2025-quarter-supplementary-table-1-5A"]]
         Dset <- Dset[, c("Series_reference","Period","Value","Unit","Series_name","Group")]
         
         Dset$Period <- as.Date(paste0(Dset$Period, "-01"), "%Y.%m-%d")
         month(Dset$Period) <- month(Dset$Period) + 1
         Dset$Period <- Dset$Period - 1

         Dset$Value  <- as.numeric(str_replace_all(Dset$Value, ",",""))

         ISA_SupplementaryTable15A <- Dset[,c("Series_reference","Group","Series_name","Unit","Period","Value")]

      ##
      ##    Supplementary Table 1-5B
      ##
         Dset <- All_Data[["na-isal-june-2025-quarter-supplementary-table-1-5B"]]
         Dset <- Dset[, c("Series_reference","Period","Value","Unit","Seasonality","Series_name","RBNZ_seriesID","Transaction","Transaction_label","Asset_type","Sector","Sector_name")]
         Dset$Actual_Seasadj <- ifelse(Dset$seasonality == "A", "Actual",
                              ifelse(Dset$seasonality == "S", "Seasonally_Adjusted", "UNKNOWN"))
                              
         
         Dset$Period <- as.Date(paste0(Dset$Period, "-01"), "%Y.%m-%d")
         month(Dset$Period) <- month(Dset$Period) + 1
         Dset$Period <- Dset$Period - 1

         Dset$Value  <- as.numeric(str_replace_all(Dset$Value, ",",""))

         ISA_SupplementaryTable15B <- Dset[,c("Actual_Seasadj","Series_reference","Unit","Series_name","Transaction","Transaction_label","Asset_type","Sector","Sector_name","Period","Value")]

         
   ##
   ## Save files our produce some final output of something
   ##
      save(ISA_Consolidated_Accounts,     file = "Data_Output/ISA_Consolidated_Accounts.rda")
      save(Institutional_Sector_Accounts, file = "Data_Output/Institutional_Sector_Accounts.rda")
      save(ISA_SupplementaryTable15A,     file = "Data_Output/ISA_SupplementaryTable15A.rda")
      save(ISA_SupplementaryTable15B,     file = "Data_Output/ISA_SupplementaryTable15B.rda")
##
##    And we're done
##
