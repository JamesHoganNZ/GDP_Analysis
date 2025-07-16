##
##    Programme:  Create_Analytical_Set.r
##
##    Objective:  This programme is a port from something I'd written at MBIE.  I'm
##                modularising it now because I don't have TRED, so must read in the
##                data separately from InfoShare.  The following spreadsheets have been
##                downloaded from InfoShare, opened in Excel, and saved as Excel spread-
##                sheets.  This programme reads them in and tidy's there structure.
##
##       Author:  James Hogan, NZIER, 13 November 2018
##
   rm(list=ls(all=TRUE))
   
   ##
   ## Load the source data frames
   ##
      load("Data_Intermediate/Capital_Input.rda")
      load("Data_Intermediate/CPI.rda")
      load("Data_Intermediate/General_Price.rda")
      load("Data_Intermediate/Gross_Output.rda")
      load("Data_Intermediate/Labour_Hours.rda")
      load("Data_Intermediate/Labour_Qty.rda")
      load("Data_Intermediate/PPI_Output.rda")
      load("Data_Intermediate/Unemployed.rda")
   
   ##
   ##    After discussing with David Patterson on the 16 Sept, he suggested I use QES Paid Hours as the labour measure.  But that also needs
   ##       an employment quantity measure too..
   ##
   
   Labour_Hours  <- Labour_Hours[Labour_Hours$Gender != "Total Both Sexes",]
   Labour_Hours  <- Labour_Hours[Labour_Hours$Measure == "Total (Ordinary Time + Overtime) Hours",]
   Labour <- merge(Labour_Hours,
                   Labour_Qty,
                   by = c("TimePeriod", "Gender", "Industries"))
   Labour$Total_Hours_Worked <- with(Labour, value.x*value.y)
   Labour_Input <- with(Labour,
                      aggregate(list(value = Total_Hours_Worked),
                                list(TimePeriod = TimePeriod,  
                                     Industries = Industries),
                                sum, 
                                na.rm = FALSE)
                                )
      
   ##
   ##    Find only the all age, unemployment rate
   ##        
    Unemployed <- Unemployed[((Unemployed$Age == 'Total All Ages')  &
                              (Unemployed$Gender == 'Total Both Sexes') &
                              (Unemployed$Measure == 'Unemployment Rate')),]

   ##
   ##    Back with the rest of the programme
   ##      
      min(Labour_Hours$TimePeriod)      
      min(Labour_Input$TimePeriod)      
      min(Capital_Input$TimePeriod)      
      min(PPI_Output$TimePeriod)      
      min(Unemployed$TimePeriod)      
      min(General_Price$TimePeriod)      
      min(CPI$TimePeriod)      
 

      max(Labour_Hours$TimePeriod)      
      max(Labour_Input$TimePeriod)      
      max(Capital_Input$TimePeriod)      
      max(PPI_Output$TimePeriod)      
      max(Unemployed$TimePeriod)      
      max(General_Price$TimePeriod)      
      max(CPI$TimePeriod)    
    
            
      Gross_Output  <- Gross_Output[Gross_Output$Measure == 'Output', c("TimePeriod", "Industries", "value")]
      Labour_Input  <- Labour_Input[, c("TimePeriod", "Industries", "value")]
      Capital_Input <- Capital_Input[, c("TimePeriod", "Industries", "value")]
      PPI_Output    <- PPI_Output[, c("TimePeriod", "Industries", "value")]
      CPI           <- CPI[, c("TimePeriod", "Measure", "value")]
      CPI           <- CPI[month(CPI$TimePeriod) == 3,]
      CPI           <- CPI[!is.na(CPI$TimePeriod),]
      General_Price <- General_Price[General_Price$Measure == 'Gross Domestic Product - expenditure measure', c("TimePeriod", "Measure", "value")]
      
       Unemployed    <- Unemployed[((Unemployed$Measure == 'Unemployment Rate') &
                                    (Unemployed$Age == 'Total All Ages') ), c("TimePeriod", "Measure", "value")]

   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      Gross_Output  <- melt(Gross_Output, 
                            id.vars = c("TimePeriod", "Industries"),
                            measure.vars = c("value"))
      Gross_Output  <- Gross_Output[, names(Gross_Output) %in% c("TimePeriod", "Industries", "value")]
      names(Gross_Output)[names(Gross_Output) == "Industries"] = "variable"  
                            
      Labour_Input    <- melt(Labour_Input, 
                              id.vars = c("TimePeriod", "Industries"),
                              measure.vars = c("value"))      
      Labour_Input  <- Labour_Input[, names(Labour_Input) %in% c("TimePeriod", "Industries", "value")]
      names(Labour_Input)[names(Labour_Input) == "Industries"] = "variable"  
                            
      Capital_Input <-  Capital_Input[, names(Capital_Input) %in% c("TimePeriod", "Industries", "value")]
      names(Capital_Input)[names(Capital_Input) == "Industries"] = "variable"  
      
      names(PPI_Output)[names(PPI_Output) == 'Industries']  = "variable"
         
   ##
   ##    Deflate Gross_Output by the PPI_Output
   ##
      PPI_Output$YEMar <- as.Date(ifelse(month(PPI_Output$TimePeriod) <= 3, paste0("31/03/", year(PPI_Output$TimePeriod)), paste0("31/03/", year(PPI_Output$TimePeriod)+1)),"%d/%m/%Y")
      PPI_Output$value <- as.numeric(PPI_Output$value)
      PPI_Output <- with(PPI_Output,
                      aggregate(list(value = value),
                                list(TimePeriod = YEMar,  
                                     variable = variable),
                                mean, 
                                na.rm = FALSE)
                                )
      Gross_Output <- merge(Gross_Output,
                            PPI_Output,
                            by = c("TimePeriod", "variable"),
                            all = TRUE)
      Gross_Output$Output <- with(Gross_Output, (as.numeric(value.x) / (value.y/1000)))

   ##
   ##    Recode the Industries for Gross_Output
   ##
      Gross_Output_Recode <- data.frame(variable=c("Accommodation and Food Services",
                                                   "Administrative and Support Services",
                                                   "Agriculture",
                                                   "Arts and Recreation Services",
                                                   "Central Government Administration, Defence and Public Safety",
                                                   "Construction",
                                                   "Education and Training",
                                                   "Electricity, Gas, Water and Waste Services",
                                                   "Financial and Insurance Services",
                                                   "Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services",
                                                   "Food, Beverage and Tobacco Product Manufacturing",
                                                   "Forestry and Logging",
                                                   "Furniture and Other Manufacturing",
                                                   "Health Care and Social Assistance",
                                                   "Information Media and Telecommunications",
                                                   "Local Government Administration",
                                                   "Metal Product Manufacturing",
                                                   "Mining",
                                                   "Non-Metallic Mineral Product Manufacturing",
                                                   "Other Services",
                                                   "Owner-Occupied Property Operation (National Accounts Only)",
                                                   "Petroleum, Chemical, Polymer and Rubber Product Manufacturing",
                                                   "Printing",
                                                   "Professional, Scientific and Technical Services",
                                                   "Rental, Hiring and Real Estate Services",
                                                   "Retail Trade",
                                                   "Textile, Leather, Clothing and Footwear Manufacturing",
                                                   "Total All Industries",
                                                   "Transport Equipment, Machinery and Equipment Manufacturing",
                                                   "Transport, Postal and Warehousing",
                                                   "Wholesale Trade",
                                                   "Wood and Paper Products Manufacturing"),
                                         new=c("Retail Trade and Accommodation",
                                               "Professional, Scientific, Technical, Administrative and Support Services",
                                               "Biff",
                                               "Arts, Recreation and Other Services",
                                               "Biff",
                                               "Construction",
                                               "Education and Training",
                                               "Electricity, Gas, Water and Waste Services",
                                               "Financial and Insurance Services",
                                               "Biff",
                                               "Manufacturing",
                                               "Forestry and Mining",
                                               "Manufacturing",
                                               "Health Care and Social Assistance",
                                               "Information Media and Telecommunications",
                                               "Biff",
                                               "Manufacturing",
                                               "Forestry and Mining",
                                               "Manufacturing",
                                               "Arts, Recreation and Other Services",
                                               "Biff",
                                               "Manufacturing",
                                               "Manufacturing",
                                               "Professional, Scientific, Technical, Administrative and Support Services",
                                               "Rental, Hiring and Real Estate Services",
                                               "Retail Trade and Accommodation",
                                               "Manufacturing",
                                               "Biff",
                                               "Manufacturing",
                                               "Transport, Postal and Warehousing",
                                               "Wholesale Trade",
                                               "Manufacturing"))
                                                             
      Gross_Output <- merge(Gross_Output,
                            Gross_Output_Recode,
                            by = c("variable"),
                            all = TRUE)
      Gross_Output <- with(Gross_Output,
                      aggregate(list(Output = Output),
                                list(TimePeriod = TimePeriod,  
                                     variable = new),
                                sum, 
                                na.rm = TRUE)
                                )
     ##
     ##  Do the same for the capital inputs
     ##
         Capital_Input_Recode <- data.frame(variable=c("Agriculture, Forestry and Fishing",
                                                       "Arts, Recreation and Other Services",
                                                       "Construction",
                                                       "Education and Training",
                                                       "Electricity, Gas, Water and Waste Services",
                                                       "Financial and Insurance Services",
                                                       "Health Care and Social Assistance",
                                                       "Information Media and Telecommunications",
                                                       "Manufacturing",
                                                       "Mining",
                                                       "Professional, Scientific, Technical, Administrative and Support Services",
                                                       "Public Administration and Safety",
                                                       "Rental, Hiring and Real Estate Services",
                                                       "Retail Trade and Accommodation",
                                                       "Total All Industries",
                                                       "Transport, Postal and Warehousing",
                                                       "Wholesale Trade"),
                                                 new=c("Forestry and Mining",
                                                       "Arts, Recreation and Other Services",
                                                       "Construction",
                                                       "Education and Training",
                                                       "Electricity, Gas, Water and Waste Services",
                                                       "Financial and Insurance Services",
                                                       "Health Care and Social Assistance",
                                                       "Information Media and Telecommunications",
                                                       "Manufacturing",
                                                       "Forestry and Mining",
                                                       "Professional, Scientific, Technical, Administrative and Support Services",
                                                       "Public Administration and Safety",
                                                       "Rental, Hiring and Real Estate Services",
                                                       "Retail Trade and Accommodation",
                                                       "Total All Industries",
                                                       "Transport, Postal and Warehousing",
                                                       "Wholesale Trade"))
      Capital_Input <- merge(Capital_Input,
                             Capital_Input_Recode,
                             by = c("variable"),
                             all = TRUE)
      Capital_Input <- with(Capital_Input,
                         aggregate(list(Capital = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = new),
                                   sum, 
                                   na.rm = FALSE)
                                   )
     ##
     ##  Now for the Labour_Input
     ##
         Labour_Input_Recode <- data.frame(variable = c("Accommodation and Food Services",
                                                       "Arts, Recreation and Other Services",
                                                       "Construction",
                                                       "Education and Training",
                                                       "Electricity, Gas, Water and Waste Services",
                                                       "Financial and Insurance Services",
                                                       "Forestry and Mining",
                                                       "Health Care and Social Assistance",
                                                       "Information Media and Telecommunications",
                                                       "Manufacturing",
                                                       "Professional, Scientific, Technical, Administrative and Support Services",
                                                       "Public Administration and Safety",
                                                       "Rental, Hiring and Real Estate Services",
                                                       "Retail Trade",
                                                       "Total All Industries",
                                                       "Transport, Postal and Warehousing",
                                                       "Wholesale Trade"),
                                                 new=c("Retail Trade and Accommodation",
                                                       "Arts, Recreation and Other Services",
                                                       "Construction",
                                                       "Education and Training",
                                                       "Electricity, Gas, Water and Waste Services",
                                                       "Financial and Insurance Services",
                                                       "Forestry and Mining",
                                                       "Health Care and Social Assistance",
                                                       "Information Media and Telecommunications",
                                                       "Manufacturing",
                                                       "Professional, Scientific, Technical, Administrative and Support Services",
                                                       "Public Administration and Safety",
                                                       "Rental, Hiring and Real Estate Services",
                                                       "Retail Trade and Accommodation",
                                                       "Total All Industries",
                                                       "Transport, Postal and Warehousing",
                                                       "Wholesale Trade"))
         Labour_Input <- merge(Labour_Input,
                               Labour_Input_Recode,
                               by = c("variable"),
                               all = TRUE)                                                       
         Labour_Input <- with(Labour_Input,
                         aggregate(list(Labour = value),
                                   list(TimePeriod = TimePeriod,  
                                        variable = new),
                                   sum, 
                                   na.rm = FALSE)
                                   )
     
     ##
     ##  Merge everything together
     ##
         Analytical_Set <- merge(Gross_Output,
                                 Capital_Input,
                                 by = c("TimePeriod", "variable"),
                                 all = TRUE)
         Analytical_Set <- merge(Analytical_Set,
                                 Labour_Input,
                                 by = c("TimePeriod", "variable"),
                                 all = TRUE)
         Analytical_Set <- Analytical_Set[month(Analytical_Set$TimePeriod) == 3,]                       
         Analytical_Set <- Analytical_Set[!is.na(Analytical_Set$Output),]
         Analytical_Set <- Analytical_Set[Analytical_Set$Output > 0,]
         Analytical_Set <- Analytical_Set[!is.na(Analytical_Set$Capital),]
         Analytical_Set <- Analytical_Set[!is.na(Analytical_Set$Labour),]
      save(Analytical_Set, file = "Data_Intermediate/Analytical_Set.rda")

##
##    And We're done :)
##


