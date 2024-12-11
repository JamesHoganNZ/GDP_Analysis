##
##    Programme:  Read_Spreadsheets.r
##
##    Objective:  This programme is designed to read in the excel spreadsheets in Data_Raw,
##                cycle through each tab of each spreadsheet, and save the tab as an exact replica of the 
##                tab into the directory "Data_Intermediate/". Each tab is saved as:
##                "RAWDATA_<TAB NAME>XX<SPREADSHEET NAME>.rda", so both the tab and the spreadsheet can be
##                separated with a str_split_fixed(). All of the columns are saved as character type.
##
##                This programme will ALWAYS give a 100% accurate reproduction of the tab as an R dataframe.
##
##                It uses RDCOMClient. A copy of the RDCOMClient library is saved here:
##                G:\Greenhouse_Gas_Inventory_Team\Training\R_4
##
##                New people into the team need RDCOMClient copied from the above location, into their own version of 
##                R libraries:
##
##                > .libPaths()
##                [1] "C:/Users/HoganJa/Documents/R/win-library/4.1" "C:/Program Files/R/R-4.1.3/library"         
##
##                RDCOMClient hints from here: https://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/DCOM.html
##
##    Author:    James Hogan, Green House Gas Inventory, Policy & Trade, 27 June 2022
##

   rm(list=ls(all=TRUE))
   
   ##
   ##    Find the spreadsheets
   ##      
      Base_Path <- paste0(getwd(), "/Data_Raw")
   
      X <- as.data.frame(system(paste0('cmd.exe /c dir /b "', Base_Path, '" /s'), intern = T), stringsAsFactors = FALSE)
      names(X) = "Details"
      Directories <- as.data.frame(X[str_detect(X$Details, "\\."),], stringsAsFactors = FALSE)
      names(Directories) = "Details"
      Directories$Biff <- str_replace_all(Directories$Details, paste0(str_replace_all(getwd(), "\\/", "\\\\\\\\"), "\\\\Data_Raw\\\\"), "")
   ##
   ##    Clean up the spreadsheet name as its save name
   ##
   Directories$Save_Name <- str_split_fixed(Directories$Biff, "\\.", n = 2)[,1]
   Directories$Save_Name <- str_replace_all(Directories$Save_Name, " ","_")
   ##
   ##    Delete the old files
   ##
      unlink("Data_Intermediate/RAWDATA_*.rda")
   
   ##
   ##    A little fix to make this run quicker over the inter
   ##
   Directories <- Directories[str_detect(Directories$Biff, ".xls"),]
  
   ##
   ##    Initialise some RDCOMClient parameters
   ##
      ex <- COMCreate("Excel.Application")
      ex[["Visible"]] <- TRUE
      ex[["DisplayAlerts"]] <- FALSE
      ex[["AskToUpdateLinks"]] <- FALSE
      Workbook <- ex[["workbooks"]]
      
   ##
   ##    Grab all the sheet names
   ##
     DidntRead <- data.frame(File = character(),
                              Tab = character())
                              
     for(File in 1:nrow(Directories))
      {
       tic(paste("Reading Worksheet", Directories$Details[File]))
       current_file <- Workbook$Open(Directories$Details[File])
         for(j in 1:current_file$worksheets()$count())
         {
            Name <- current_file$worksheets(j)$name()
            ##
            ##  RDCOMClient keeps breaking, so we're going save each tab as CSV and read it in
            ##
             Current_Tab <- current_file$Worksheets(as.character(Name))
             if(Current_Tab[["ProtectionMode"]] == TRUE){Current_Tab[["ProtectionMode"]] <- FALSE}
             
            ## Current_Tab$unprotect()
             unlink("Data_Intermediate/test.csv", force = TRUE)
             result = tryCatch({ 
                                 ## Current_Tab$Range("A1:IV60000")$RemoveSubtotal()
                                 ex[["DisplayAlerts"]] <- FALSE
                                 Current_Tab$SaveAs(FileName =  paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Data_Intermediate\\test.csv"), FileFormat = 6)
                                 Sys.sleep(2)
                                 ##
                                 ##     Stick everything back in an appropriately named data frame and save
                                 ##
                                  X <- read.csv(paste0(str_replace_all(getwd(), "\\/", "\\\\\\\\"), "\\\\Data_Intermediate\\\\test.csv"), colClasses = c("character"), header=FALSE)
                                  X[] <- lapply(X, as.character)
                                  #X[] <- lapply(X, str_trim)  # this fails for some spreadsheet tabs - I dont know why
                                  assign(paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), X )
                                  save(list = paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), 
                                       file = paste0("Data_Intermediate/RAWDATA_", Name, "XX", Directories$Save_Name[File], ".rda"))
                                  rm(list=paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]))
                                  Sys.sleep(2)
                               }, warning = function(w) {
                               }, error = function(e) {
                                    print(paste0("Spreadsheet: '", Directories$Details[File], "', tab '", Name, "' wasnt read"))
                                   DidntRead <- rbind.fill( DidntRead , data.frame(File = Directories$Details[File],
                                                                                    Tab = Name))
                               }, finally = {
                               })  
         }
         ##
         ##     Close the RDCOMClient spreadsheet
         ##
         current_file$Close()
      toc()
     }
  ex$quit()
  unlink("Data_Intermediate/test.csv")
##
##    And we're done
##
   