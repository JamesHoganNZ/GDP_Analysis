##    Programme:  GDP_Analysis.r
##
##    Objective:  I've got some time on my hands between my Bank of New Zealand and 
##                my SPC gig. Stats NZ released its GDP measures which were really 
##                bad. But lets make a project which can systematically evaluate the 
##                GDP data, the exports/imports data and the financial data.
##
##    Plan of  :  Lets start off with a project which pulls in all of the SNZ GDP data.
##    Attack   :  It will also interface with the imports and exports data. And well
##                download the RBNZ data.
##
##
##    Important:  This programme will interface with the Trade project here:
##    Linkages :  C:\Work_Related_Projects\Trade_Modelling
##
##
##
##    Author   :  James Hogan, JamesHogan Ltd, 20 June 2024
##
##
   ##
   ##    Clear the decks and load up some functionality
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(extrafont)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
   ##
   ##    Project-specific libraries
   ##
      library(XML)
      library(data.table)
      library(textclean)
      library(RSelenium)
      library(splines)
      library(strucchange)
      library(lmtest)
      library(scales)
      library(dynlm)
      library(systemfit)
      library(tseries)
      library(cluster)
      library(nlme)
      library(plm)
      library(systemfit)
      library(micEconCES)      
      library(forecast)
      library(grid)
      library(gridExtra)

   ##
   ##    Set working directory
   ##
      setwd("C:\\GIT_Projects\\GDP_Analysis")

      ##
      ##    STEP 1:  Update the trade stats
      ##
         source("C:/Work_Related_Projects/Trade_Modelling/integrate.r")
         
      ##
      ##    STEP 2:  Update the Reserve Bank stats. This isn't working yet because the RB blocks
      ##             screen scraping :-/
      ##
         source("C:/Work_Related_Projects/RBNZ_Analysis/integrate.r")

      ##
      ## Reset the working directory
      ##
      setwd("C:\\GIT_Projects\\GDP_Analysis")

      ##
      ##    STEP 3: Grab some Stats NZ data.
      ##
         Publish_Date <- "20250631"
         save(Publish_Date, file = "Data_Intermediate/Publish_Date.rda")
         
         source("Programmes/Grab_GDP_Data.r") # This grabs a whole heap of info off InfoShare
         source("Programmes/Read_Spreadsheets.r") # This reads in the above data
         
      ##
      ##    STEP 4: Data Cleaning - make it tidy
      ##
         source("Programmes/Clean_GDP.r")
         source("Programmes/Clean_ExternalTrade.r")
         source("Programmes/Clean_HCE.r")
         source("Programmes/Clean_GFKF.r")
         source("Programmes/Clean_GDPI.r")
         source("Programmes/Clean_CPI.r")
         source("Programmes/Clean_PPI.r")
         source("Programmes/Clean_Capital_Stock.r")
         source("Programmes/Clean_QES.r")
         source("Programmes/Clean_Unemployment.r")
         
      ##
      ##    STEP 5: Estimate a long run and short run production function
      ##
         source("Programmes/New_Zealand_Production_Function.r")
      ##
      ##    STEP x: Final output
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
##
##   End of programme
##
