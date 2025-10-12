##
##    Programme:  Where_Does_The_Money_Go.r
##
##    Objective:  
##
##    Author:     James Hogan, started 27 June 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      load("Data_Intermediate/ConstantPrice_SA_Qtr_GDP_Published20240331.rda")
      load("Data_Intermediate/ConstantPrice_Actual_Annual_CapitalStock_Published20240331.rda")
      load("Data_Intermediate/ConstantPrice_SA_Qtr_GDP_Published20240331.rda")
                     
   ##
   ## Step 1: Check out the Industries and move each data source to a common industry definition
   ##




   ##
   ## Step 2: Interpolate the annual capital stock into a quarterly measure. I'll use these as "seaonally adjusted"
   ##
   
   
   ##
   ## Step 3: Seasonally adjust the quarterly QES measure
   ##

   ##
   ## Step 4: Combine the data sources together into a common industry and time period, and save. This will become our
   ##         modelling data set.
   ##





   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
