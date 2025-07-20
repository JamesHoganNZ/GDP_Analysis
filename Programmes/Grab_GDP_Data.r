##
##    Programme:  Grab_GDP_Data.r
##
##    Objective:  This programme goes to the New Zealand Reserve Bank website and pulls down all of its
##                statistical data
##
##                There's a little trick with infoshare. At the bottom is a "helpful" chatbox which covers
##                the "go" button and prevents selenium being able to press the go button. The browser needs
##                to shrink in size.
##
##

   rm(list=ls(all=TRUE))
   
   file_path <- paste0(str_replace_all(getwd(),"/", "\\\\\\\\"), "\\\\Data_Raw\\\\")
   fprof <- makeFirefoxProfile(list(browser.download.dir = file_path,
                                    browser.download.folderList = 2L,
                                    browser.download.manager.showWhenStarting = FALSE,
                                    browser.helperApps.neverAsk.openFile = "text/csv",
                                    browser.helperApps.neverAsk.saveToDisk = "text/csv")
                               )
    
    rD <- rsDriver(browser=c("firefox"), chromever = "114.0.5735.90", extraCapabilities = fprof, phantomver = NULL)

    remDr <- rD[["client"]]

    Downloaded_Files <- data.frame(Measure = character(),
                                   File    = character(),
                                   Focus   = character())
   ##
   ##    Go to the overseas-merchandise-trade-datasets website, let the site work out its javascript, and then parse it 
   ##
      Base_URL <- "https://infoshare.stats.govt.nz/"
      remDr$navigate(Base_URL)
      Sys.sleep(3)
      
      ##
      ##    Get the GDP stats
      ##
         webElem <- remDr$findElement(using = "link text", "Economic indicators")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)

         webElem <- remDr$findElement(using = "link text", "National Accounts - SNA 2008 - SNE")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
      ##
      ##    Delete the old SNE files
      ##
         unlink("Data_Raw/*.xls")
         
      ##
      ##    Series, GDP(P), Chain volume, Seasonally adjusted, ANZSIC06 industry groups (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "Series, GDP(P), Chain volume, Seasonally adjusted, ANZSIC06 industry groups (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Gross Domestic Production"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               Sys.sleep(5)
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)            
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
      ##
      ##    Series, External account, Chain volume, Seasonally adjusted, Total (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "Series, External account, Chain volume, Seasonally adjusted, Total (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "External Trade"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               Sys.sleep(5)
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
      ##
      ##    Series, GDP(E), Chain Volume, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "Series, GDP(E), Chain Volume, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Domestic Household Consumption"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl09_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               Sys.sleep(5)
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
         
      ##
      ##    	Series, GDP(E), Nominal, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "Series, GDP(E), Nominal, Seasonally Adjusted, Household FCE by item (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Domestic Household Consumption"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl09_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()

      ##
      ##    	Series, GDP(E), Chain volume, Seasonally adjusted, Asset type (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "Series, GDP(E), Chain volume, Seasonally adjusted, Asset type (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Domestic Productive Investment"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
      ##
      ##    	GDP(I), current prices (Qrtly-Mar/Jun/Sep/Dec)
      ##
            Measure <- "GDP(I), current prices (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Domestic Income Flows"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl09_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(2)
               webElem$acceptAlert()   ## THIS TURNS OFF THE ANNOYING ALERT BUTTON!!! What is mission it was to find this command              
               Sys.sleep(5)
               
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()

      ##
      ## Grab some price information
      ##
         webElem <- remDr$findElement(using = "link text", "Consumers Price Index - CPI")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
      
         ##
         ##    	CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "CPI Level 3 Classes for New Zealand, Seasonally adjusted (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Consumer Prices"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
            ##
            ##    Go Back to main page
            ##
            remDr$navigate(Base_URL)
            
            
         ##
         ##    	CPI Non-standard All Groups Less/Plus Selected Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "CPI Non-standard All Groups Less/Plus Selected Groupings for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Consumer Prices"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
         ##
         ##    	CPI All Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "CPI All Groups for New Zealand (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Consumer Prices"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
         ##
         ##    	Outputs (ANZSIC06) - NZSIOC level 1, Base: Dec. 2010 quarter (=1000) (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "Producers Price Index - PPI"
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)

            Measure <- "Outputs (ANZSIC06) - NZSIOC level 1, Base: Dec. 2010 quarter (=1000) (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Producers Prices"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()

         ##
         ##       Grab some Capital Stock
         ##    	Series, Balance sheet items, Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)
         ##
            Measure <- "Series, Balance sheet items, Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)"
            Focus   <- "Capital Stock"
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)

            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()
         ##
         ##       Grab some Labour Measures
         ##    	Quarterly Employment Survey - QEM
         ##
         
         webElem <- remDr$findElement(using = "link text", "Work income and spending")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
         

         webElem <- remDr$findElement(using = "link text", "Quarterly Employment Survey - QEM")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
         
      
         ##
         ##    	Total Paid Hours by Industry (ANZSIC06) (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "Total Paid Hours by Industry (ANZSIC06) (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Labour Inputs"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
         
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()
            ##
            ##    Select the download as excel option
            ##
            Prelist <- list.files("Data_Raw")
               Sys.sleep(5)   
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(5)
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]

            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()

         ##
         ##       Grab some Unemployment measures
         ##    	Quarterly Employment Survey - QEM
         ##
         
         webElem <- remDr$findElement(using = "link text", "Work income and spending")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
         

         webElem <- remDr$findElement(using = "link text", "Household Labour Force Survey - HLF")
         webElem$sendKeysToElement(list(key = "enter"))
         Sys.sleep(3)
      
         ##
         ##    	Total Paid Hours by Industry (ANZSIC06) (Qrtly-Mar/Jun/Sep/Dec)
         ##
            Measure <- "Labour Force Status by Sex by Age Group (Qrtly-Mar/Jun/Sep/Dec)"
            Focus   <- "Unemployment"
            
            webElem <- remDr$findElement(using = "link text", Measure)
            webElem$sendKeysToElement(list(key = "enter"))
            Sys.sleep(3)
         
            ##
            ##    Select all of the box elements
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl02_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl04_lblSelectAll")
            webElem$clickElement()

            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl07_lblSelectAll")
            webElem$clickElement()
            
            webElem <- remDr$findElement(using = "id", "ctl00_MainContent_ctl09_lblSelectAll")
            webElem$clickElement()
            
            ##
            ##    Select the download as excel option
            ##

            Prelist <- list.files("Data_Raw")
               option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'xls']")
               option$clickElement()
               
               webElem <- remDr$findElement(using = "name", "ctl00$MainContent$btnGo")
               webElem$clickElement()
               Sys.sleep(2)
               webElem$acceptAlert()   ## THIS TURNS OFF THE ANNOYING ALERT BUTTON!!! What is mission it was to find this command              
               Sys.sleep(5)
               
            Postlist <- list.files("Data_Raw")
            File <- Postlist[!(Postlist %in% Prelist)]



            Downloaded_Files <- rbind(Downloaded_Files,
                                      data.frame(Measure = Measure,
                                                 File    = File,
                                                 Focus   = Focus))
            ##
            ##    Go Back to main page
            ##
            webElem <- remDr$findElement(using = "id", "ctl00_headerUserControl_browseTab")
            webElem$clickElement()

     
   ##
   ## Lets leave it at that for the moment :)
   ##
   remDr$close()
   rD$server$stop()

   Downloaded_Files$Subject_Link <- str_split_fixed(Downloaded_Files$File, "\\.", n = 2)[,1]
   save(Downloaded_Files, file = "Data_Intermediate/Downloaded_Files.rda")

