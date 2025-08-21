##
##    Programme:  Clean_RBNZ_Data.r
##
##    Objective:  This programme cleans up the data downloaded from RBNZ. Manually downloaded
##                because the RBNZ are idiots
##
##    Author:     James Hogan, 20 August 2025
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
                     
   ##
   ##    Load the raw data
   ##
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents$Subject_Link <- str_split_fixed(Contents$Dframe, "XX", n = 2)[,2]
      Contents <- Contents[str_detect(Contents$DataFrames, "RAWDATA_"),]

      Contents <- Contents[Contents$Subject_Link %in% c("hb3", "hc12-month-end", "hc5", "hc50-long-run", "hs34", "hs41"),]
      Contents <- Contents[!str_detect(Contents$DataFrames, "Series Definitions"),]
      Contents <- Contents[!str_detect(Contents$DataFrames, "Table Description"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Data_Intermediate/", File))  
                           X <- data.table(get(str_split_fixed(File, "\\.", n = 2)[,1]))
                           return(X)})
      names(All_Data) <- str_trim(Contents$Subject_Link, side = c("both"))

   ##
   ## Step 2: Start cleaning it up data 
   ##
      ##
      ##   hb3: Retail interest rates: Weighted average lending and deposits - B3
      ##
         HB3 <- All_Data[["hb3"]]
         Rename <- data.table(variable    = names(HB3),
                              Measure1    = as.character(c("Period", HB3[1,2:length(HB3)])),
                              Measure2    = as.character(c("Period", HB3[2,2:length(HB3)])),
                              Unit        = as.character(c("Period", HB3[4,2:length(HB3)])),
                              RBNZ_Series = as.character(c("Period", HB3[5,2:length(HB3)])))
                              
 
         HB3 <- data.table::melt(HB3,
                                 id.var = c("V1"))
                              
         HB3$Period <- as.Date(paste0(HB3$V1, "-01"), "%b %Y-%d")
                                                           
         month(HB3$Period) <- month(HB3$Period) + 1
         HB3$Period <- HB3$Period - 1
         
         HB3$Value  <- as.numeric(str_replace_all(HB3$value, ",",""))
         HB3 <- HB3[!is.na(HB3$Value) &  !is.na(HB3$Period),]

         HB3 <- merge(HB3,
                      Rename,
                      by = c("variable"))
         Retail_Interest_Rates <- HB3[,c("Period", "Measure1", "Measure2", "Unit", "RBNZ_Series","Value")]
         Retail_Interest_Rates <- Retail_Interest_Rates[order(Retail_Interest_Rates$Measure1, 
                                                              Retail_Interest_Rates$Measure1, 
                                                              Retail_Interest_Rates$Unit, 
                                                              Retail_Interest_Rates$RBNZ_Series, 
                                                              Retail_Interest_Rates$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Retail_Interest_Rates", Publish_Date), Retail_Interest_Rates)
            save(list = paste0("Retail_Interest_Rates", Publish_Date), 
                 file = paste0("Data_Output/Retail_Interest_Rates", Publish_Date,".rda"))
           
      ##
      ##   hc5: Sector lending (registered banks and non-bank lending institutions) - C5
      ##
         HC5 <- All_Data[["hc5"]]
         Rename <- data.table(variable    = names(HC5),
                              Measure1    = as.character(c("Period", HC5[1,2:length(HC5)])),
                              Measure2    = as.character(c("Period", HC5[2,2:length(HC5)])),
                              Unit        = as.character(c("Period", HC5[4,2:length(HC5)])),
                              RBNZ_Series = as.character(c("Period", HC5[5,2:length(HC5)])))
                              
 
         HC5 <- data.table::melt(HC5,
                                 id.var = c("V1"))
                                 
         HC5$Period <- as.Date(paste0(HC5$V1, "-01"), "%b %Y-%d")
                                                           
         month(HC5$Period) <- month(HC5$Period) + 1
         HC5$Period <- HC5$Period - 1
         
         HC5$Value  <- as.numeric(str_replace_all(HC5$value, ",",""))
         HC5 <- HC5[!is.na(HC5$Value) &  !is.na(HC5$Period),]

         HC5 <- merge(HC5,
                      Rename,
                      by = c("variable"))
         Sector_lending <- HC5[,c("Period", "Measure1", "Measure2", "Unit", "RBNZ_Series","Value")]
         Sector_lending <- Sector_lending[order(Sector_lending$Measure1, 
                                                              Sector_lending$Measure1, 
                                                              Sector_lending$Unit, 
                                                              Sector_lending$RBNZ_Series, 
                                                              Sector_lending$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Sector_lending", Publish_Date), Sector_lending)
            save(list = paste0("Sector_lending", Publish_Date), 
                 file = paste0("Data_Output/Sector_lending", Publish_Date,".rda"))
      ##
      ##   c12: Credit card statistics: business and personal balances outstanding and interest rates, at month end - C12
      ##
         C12 <- All_Data[["hc12-month-end"]]
         Rename <- data.table(variable    = names(C12),
                              Measure1    = as.character(c("Period", C12[1,2:length(C12)])),
                              Measure2    = as.character(c("Period", C12[2,2:length(C12)])),
                              Unit        = as.character(c("Period", C12[4,2:length(C12)])),
                              RBNZ_Series = as.character(c("Period", C12[5,2:length(C12)])))
                              
 
         C12 <- data.table::melt(C12,
                                 id.var = c("V1"))
                                 
         C12$Period <- as.Date(paste0(C12$V1, "-01"), "%b %Y-%d")
                                                           
         month(C12$Period) <- month(C12$Period) + 1
         C12$Period <- C12$Period - 1
         
         C12$Value  <- as.numeric(str_replace_all(C12$value, ",",""))
         C12 <- C12[!is.na(C12$Value) &  !is.na(C12$Period),]

         C12 <- merge(C12,
                      Rename,
                      by = c("variable"))
         Credit_Card_Stats <- C12[,c("Period", "Measure1", "Measure2", "Unit", "RBNZ_Series","Value")]
         Credit_Card_Stats <- Credit_Card_Stats[order(Credit_Card_Stats$Measure1, 
                                                              Credit_Card_Stats$Measure1, 
                                                              Credit_Card_Stats$Unit, 
                                                              Credit_Card_Stats$RBNZ_Series, 
                                                              Credit_Card_Stats$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Credit_Card_Stats", Publish_Date), Credit_Card_Stats)
            save(list = paste0("Credit_Card_Stats", Publish_Date), 
                 file = paste0("Data_Output/Credit_Card_Stats", Publish_Date,".rda"))
           
      ##
      ##   c50: Broad money, domestic credit and private sector credit (depository corporations) - C50
      ##
         hc50 <- All_Data[["hc50-long-run"]]
         Rename <- data.table(variable    = names(hc50),
                              Measure1    = as.character(c("Period", hc50[1,2:length(hc50)])),
                              Measure2    = as.character(c("Period", hc50[2,2:length(hc50)])),
                              Unit        = as.character(c("Period", hc50[4,2:length(hc50)])),
                              RBNZ_Series = as.character(c("Period", hc50[5,2:length(hc50)])))
                              
 
         hc50 <- data.table::melt(hc50,
                                 id.var = c("V1"))
                                 
         hc50$Period <- as.Date(paste0(hc50$V1, "-01"), "%b %Y-%d")
                                                           
         month(hc50$Period) <- month(hc50$Period) + 1
         hc50$Period <- hc50$Period - 1
         
         hc50$Value  <- as.numeric(str_replace_all(hc50$value, ",",""))
         hc50 <- hc50[!is.na(hc50$Value) &  !is.na(hc50$Period),]

         hc50 <- merge(hc50,
                      Rename,
                      by = c("variable"))
         Broad_Money <- hc50[,c("Period", "Measure1", "Measure2", "Unit", "RBNZ_Series","Value")]
         Broad_Money <- Broad_Money[order(Broad_Money$Measure1, 
                                                              Broad_Money$Measure1, 
                                                              Broad_Money$Unit, 
                                                              Broad_Money$RBNZ_Series, 
                                                              Broad_Money$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Broad_Money", Publish_Date), Broad_Money)
            save(list = paste0("Broad_Money", Publish_Date), 
                 file = paste0("Data_Output/Broad_Money", Publish_Date,".rda"))
           
      ##
      ##   s34: Banks: Assets - Loans & Repos by Industry - S34
      ##
         s34 <- All_Data[["hs34" ]]
         Rename <- data.table(variable    = names(s34),
                              Industry    = as.character(c("Period", s34[2,2:length(s34)])),
                              Unit        = as.character(c("Period", s34[4,2:length(s34)])),
                              RBNZ_Series = as.character(c("Period", s34[5,2:length(s34)])))
                              
 
         s34 <- data.table::melt(s34,
                                 id.var = c("V1"))
                                 
         s34$Period <- as.Date(paste0(s34$V1, "-01"), "%b %Y-%d")
                                                           
         month(s34$Period) <- month(s34$Period) + 1
         s34$Period <- s34$Period - 1
         
         s34$Value  <- as.numeric(str_replace_all(s34$value, ",",""))
         s34 <- s34[!is.na(s34$Value) &  !is.na(s34$Period),]

         s34 <- merge(s34,
                      Rename,
                      by = c("variable"))
         Loans_by_Industry <- s34[,c("Period", "Industry", "Unit", "RBNZ_Series","Value")]
         Loans_by_Industry <- Loans_by_Industry[order(Loans_by_Industry$Industry, 
                                                              Loans_by_Industry$Unit, 
                                                              Loans_by_Industry$RBNZ_Series, 
                                                              Loans_by_Industry$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Loans_by_Industry", Publish_Date), Loans_by_Industry)
            save(list = paste0("Loans_by_Industry", Publish_Date), 
                 file = paste0("Data_Output/Loans_by_Industry", Publish_Date,".rda"))
           
           
      ##
      ##   s41: Banks: Liabilities - Deposits by industry - S41
      ##
         hs41 <- All_Data[["hs41" ]]
         Rename <- data.table(variable    = names(hs41),
                              Industry    = as.character(c("Period", hs41[2,2:length(hs41)])),
                              Unit        = as.character(c("Period", hs41[4,2:length(hs41)])),
                              RBNZ_Series = as.character(c("Period", hs41[5,2:length(hs41)])))
                              
 
         hs41 <- data.table::melt(hs41,
                                 id.var = c("V1"),
                                 variable.factor = FALSE)
                                 
         hs41$Period <- as.Date(paste0(hs41$V1, "-01"), "%b %Y-%d")
                                                           
         month(hs41$Period) <- month(hs41$Period) + 1
         hs41$Period <- hs41$Period - 1
         
         hs41$Value  <- as.numeric(str_replace_all(hs41$value, ",",""))
         hs41 <- hs41[!is.na(hs41$Value) &  !is.na(hs41$Period),]

         hs41 <- merge(hs41,
                      Rename,
                      by = c("variable"))
         Deposits_by_Industry <- hs41[,c("Period", "Industry", "Unit", "RBNZ_Series","Value")]
         Deposits_by_Industry <- Deposits_by_Industry[order(Deposits_by_Industry$Industry, 
                                                              Deposits_by_Industry$Unit, 
                                                              Deposits_by_Industry$RBNZ_Series, 
                                                              Deposits_by_Industry$Period, 
                                                              )]

         ##
         ## Save files our produce some final output of something
         ##
            assign(paste0("Deposits_by_Industry", Publish_Date), Deposits_by_Industry)
            save(list = paste0("Deposits_by_Industry", Publish_Date), 
                 file = paste0("Data_Output/Deposits_by_Industry", Publish_Date,".rda"))
           
           
           
           
##
##    And we're done
##
