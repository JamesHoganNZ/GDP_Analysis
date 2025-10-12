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
   ##    Load some generic functions and colour palattes
   ##
      source("R/themes.r")

   ##
   ##    Load data from somewhere  
   ##
      load("Data_Intermediate/Downloaded_Files.rda")                
      load("Data_Output/CurrentPrice_SA_Qtr_HCE_Published20251001.rda")
      load("Data_Output/Institutional_Sector_Accounts.rda")
     
      This_Qtr_HCE <- CurrentPrice_SA_Qtr_HCE_Published20251001
     
   ##
   ##    Check the QGDP HCE sums to the ISA HCE value - Seasonally adjusted values - Yep they do :) Good on you Linzorama!
   ##
      QGDP_HCE <- This_Qtr_HCE[,
                               list(Value = sum(Value, na.rm = TRUE)),
                               by = list(Period)]      

      ISA_HCE <- Institutional_Sector_Accounts[(#(Actual_Seasadj == "Actual") &
                                                (Actual_Seasadj == "Seasonally_Adjusted") &
                                                (SNA_Account    == "Income and Outlay") & 
                                                (Sector_name    == "Households") & 
                                                (Transaction_Label == "Final consumption expenditure")),
                                               list(Value = sum(Value, na.rm = TRUE)),
                                               by = list(Period)]      
                                               
data.frame(QGDP_HCE)
data.frame(ISA_HCE)

tail(CurrentPrice_SA_Qtr_HCE_Published20251001, 20)
                                                

   ##
   ##    Ok, lets pull through the income source structure from the ISA, and add the expenditure detail from the QGDP
   ##
   ##       Unfortunately, there's confidentialised item which needs to derived from the totals :(
   ##

   
   
      ISA_HCE <- Institutional_Sector_Accounts[(Actual_Seasadj == "Seasonally_Adjusted") &
                                               (SNA_Account    == "Income and Outlay") & 
                                               (Sector_name    == "Households"),
                                               list(Value = sum(Value, na.rm = TRUE)),
                                               by = list(Period,
                                                         Transaction_Group = ifelse(Transaction_Label %in% c("Compensation of employees receivable"),                                               "Compensation of Employees", 
                                                                             ifelse(Transaction_Label %in% c("Gross surplus","Gross entrepreneurial income"),                                      "Profit from Production",
                                                                             ifelse(Transaction_Label %in% c("Interest receivable","Dividends receivable",
                                                                                                             "Investment income attributable to insurance and pension policyholders receivable",
                                                                                                             "Other overseas investment income receivable"),                                       "Property Income Receivable",
                                                                             ifelse(Transaction_Label %in% c("Social assistance benefits in cash receivable",
                                                                                                             "Social security pension benefits receivable",
                                                                                                             "Social security non-pension benefits in cash receivable"),                           "Government Transfers to Hhlds",
                                                                             ifelse(Transaction_Label %in% c("Adjustment for the change in pension entitlements"),                                 "Pension Funds Revaluation Gains",
                                                                             ifelse(Transaction_Label %in% c("Non-life insurance premiums and claims receivable",
                                                                                                             "Miscellaneous current transfers receivable"),                                        "Private Sector Transfers to Hhlds", 
                                                                             
                                                                             ifelse(Transaction_Label %in% c("Interest payable"),                                                                  "Property Income Payable", 
                                                                             ifelse(Transaction_Label %in% c("Consumption of fixed capital (-)"),                                                  "Consumption of Fixed Capital", 
                                                                             ifelse(Transaction_Label %in% c("Taxes on income paid","Other current taxes payable"),                                "Hhld Transfers to Government", 
                                                                             ifelse(Transaction_Label %in% c("Households' actual pension contributions payable",
                                                                                                             "Miscellaneous current transfers payable",
                                                                                                             "Non-life insurance premiums and claims payable",                                                                                                             
                                                                                                             "Households' actual non-pension contributions payable"),                              "Hhld Transfers to Private Sector",
                                                                             ifelse(Transaction_Label %in% c("Total income receivable"),                                                           "Total income receivable",
                                                                             ifelse(Transaction_Label %in% c("Total income payable"),                                                              "Total income payable","Other")))))))))))))]      

      ISA_HCE <- ISA_HCE[ISA_HCE$Transaction_Group != "Other",]
      
      ISA_HCE$Income_Components <- ifelse(ISA_HCE$Transaction_Group %in% c("Compensation of Employees", "Profit from Production","Property Income Receivable","Government Transfers to Hhlds","Private Sector Transfers to Hhlds", "Pension Funds Revaluation Gains"), "Income Components",
                                   ifelse(ISA_HCE$Transaction_Group %in% c("Property Income Payable","Hhld Transfers to Government","Hhld Transfers to Private Sector", "Consumption of Fixed Capital"), "Outlay Components",
                                   ifelse(ISA_HCE$Transaction_Group %in% c("Total income receivable"), "Income Total",
                                   ifelse(ISA_HCE$Transaction_Group %in% c("Total income payable"),    "Outlay Total",ISA_HCE$Transaction_Group))))

   ##
   ##    Derived the confidentialised "Hhld Transfers to Private Sector"
   ##
      ISA_HCE_Totals <- data.table::dcast(ISA_HCE,
                                          Period ~ Transaction_Group,
                                          value.var = "Value")
                                          
      ISA_HCE_Totals$confidentialised_Component <- with(ISA_HCE_Totals,(`Total income payable` - (`Hhld Transfers to Government` + `Hhld Transfers to Private Sector` + `Property Income Payable`)))                           
      ISA_HCE_Totals$`Hhld Transfers to Private Sector` <- ISA_HCE_Totals$`Hhld Transfers to Private Sector` + ISA_HCE_Totals$confidentialised_Component                                    
      ISA_HCE <- data.table::melt(ISA_HCE_Totals,
                                 id.var = c("Period"),
                                 value.name = "Value",
                                 variable.factor = FALSE)
      ISA_HCE <- ISA_HCE[!is.na(ISA_HCE$Value),]
      ISA_HCE$HCE_Item <- ISA_HCE$variable
   ##
   ##    Now add the QGDP_HCE component
   ##
      This_Qtr_HCE$variable <- "Household Consumption Expenditure"

      Budget_Constraint <- rbind(ISA_HCE, 
                                 This_Qtr_HCE)
   ##
   ##    I've been doing error checking nside the spreadsheet Adhoc_Queries/Budget_Constraint.xlsx 
   ##       and there's a difference between QGDP's HCE and ISA's HCE - Ask Lindsay
   ##

      ISA_HCE <- data.frame(Institutional_Sector_Accounts[(Actual_Seasadj == "Seasonally_Adjusted") &
                                                          (SNA_Account    == "Income and Outlay") & 
                                                          (Sector_name    == "Households") &
                                                          (Transaction_Label == "Final consumption expenditure")]   )
      QGDP_HCE <- This_Qtr_HCE[,
                               list(Value = sum(Value, na.rm = TRUE)),
                               by = list(Period)]      

      HCE_Difference <- merge(ISA_HCE[, c("Period","Value")],
                              QGDP_HCE[,c("Period","Value")],
                              by = c("Period"))
      HCE_Difference$Value <- HCE_Difference$Value.x - HCE_Difference$Value.y
      HCE_Difference$variable <- "HCE Difference between ISA and QGDP"
      HCE_Difference$HCE_Item <- "HCE Difference between ISA and QGDP"
      
      Budget_Constraint <- rbind(Budget_Constraint, 
                                 HCE_Difference[,c("Period", "variable", "Value", "HCE_Item")])
      
   ##
   ##    Nice! Make the derived totals - "Total Income", "Disposable Income", "HCE", "Savings"
   ##
      Budget_Constraint$Derived_Total <- ifelse(Budget_Constraint$HCE_Item %in% c("Compensation of Employees","Profit from Production","Government Transfers to Hhlds",
                                                                                  "Private Sector Transfers to Hhlds", "Property Income Receivable", "Pension Funds Revaluation Gains"), "Total Income",
                                         ifelse(Budget_Constraint$HCE_Item %in% c("Hhld Transfers to Government", "Hhld Transfers to Private Sector", "Property Income Payable","Consumption of Fixed Capital"), "Payments",
                                         ifelse(Budget_Constraint$HCE_Item %in% c("Alcoholic beverages, tobacco and illicit drugs","Clothing and footwear","Communication",
                                                                                  "Food and nonalcoholic beverages","Household contents and services","Housing and household utilities","Miscellaneous goods and services",
                                                                                  "Recreation and culture","Restaurants and hotels","Transport","Imports of low value goods purchased directly by households","HCE Difference between ISA and QGDP"), "HCE", "OTHER")))
      Totals <- Budget_Constraint[,
                               list(Value = sum(Value, na.rm = TRUE)),
                               by = list(Period, Derived_Total)]                                          


      Totals <- data.table::dcast(Totals,
                                  Period ~  Derived_Total,
                                  value.var = "Value")

      Totals$`Disposable Income` <- Totals$`Total Income` - Totals$Payments
      Totals$Saving <- Totals$`Disposable Income` - Totals$HCE

   ##
   ##    Put these back into the budget constraint data frame
   ##
     
      Budget_Constraint <- rbind(Budget_Constraint,
                                 data.frame(Period        = Totals$Period,
                                            Derived_Total = "HCE",
                                            HCE_Item      = "HCE",
                                            variable      = "HCE",
                                            Value         = Totals$HCE),
                                 data.frame(Period        = Totals$Period,
                                            Derived_Total = "Total Income",
                                            HCE_Item      = "Total Income",
                                            variable      = "Total Income",
                                            Value         = Totals$`Total Income`),
                                 data.frame(Period        = Totals$Period,
                                            Derived_Total = "Disposable Income",
                                            HCE_Item      = "Disposable Income",
                                            variable      = "Disposable Income",
                                            Value         = Totals$`Disposable Income`),
                                 data.frame(Period        = Totals$Period,
                                            Derived_Total = "Saving",
                                            HCE_Item      = "Saving",
                                            variable      = "Saving",
                                            Value         = Totals$Saving))
                                            
      Lookie <- data.table::dcast(Budget_Constraint,
                                             Period ~  HCE_Item,
                                             value.var = "Value")

      Lookie <- Lookie[,c("Period","Compensation of Employees", "Profit from Production", "Government Transfers to Hhlds", "Private Sector Transfers to Hhlds", "Property Income Receivable", "Pension Funds Revaluation Gains","Total Income",
                                   "Hhld Transfers to Government", "Hhld Transfers to Private Sector", "Property Income Payable", "Consumption of Fixed Capital", "Disposable Income",
                                   "Alcoholic beverages, tobacco and illicit drugs","Clothing and footwear","Communication","Food and nonalcoholic beverages","Household contents and services","Housing and household utilities","Miscellaneous goods and services",
                                   "Recreation and culture","Restaurants and hotels","Transport","Imports of low value goods purchased directly by households","HCE Difference between ISA and QGDP", "HCE","Saving")]
      data.frame(Lookie[Period == "2025-06-30"])


   ##
   ##    Clean up and make tidy
   ##
      Budget_Constraint <- Budget_Constraint[,c("Period", "Derived_Total", "HCE_Item", "Value")]
      Budget_Constraint <- Budget_Constraint[Budget_Constraint$Derived_Total != "OTHER",]
      names(Budget_Constraint) <- c("Period", "Derived_Total", "Measure", "Value")
   ##
   ## Save files our produce some final output of something
   ##
      save(Budget_Constraint, file = 'Data_Output/Budget_Constraint.rda')
      write.table(Budget_Constraint, file = "Adhoc_Queries/Budget_Constraint.csv", sep = ",", row.names=FALSE)
##
##    And we're done
##

                          
  png("Final_Output/Where does the Money Go.png", w = 15.7, h = 8.3, res = 600, units = "in")
   grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow = 4,
                                             ncol = 1)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    grid.rect()
     p1 <- ggplot(Budget_Constraint[!(Budget_Constraint$Measure %in% c("Total Income", "HCE")) & 
                                     (year(Budget_Constraint$Period) > 2015) &
                                     (Budget_Constraint$Derived_Total == "Total Income"),], 
                   aes(x = Period, 
                       y = Value, 
                       colour=Measure))     +
                   geom_line() +
                   geom_point() +
                   theme(legend.position  = "right")
                   
     p2 <- ggplot(Budget_Constraint[!(Budget_Constraint$Measure %in% c("Total Income", "HCE")) & 
                                     (year(Budget_Constraint$Period) > 2015) &
                                     (Budget_Constraint$Derived_Total == "Payments"),], 
                   aes(x = Period, 
                       y = Value, 
                       colour=Measure))     +
                   geom_line() +
                   geom_point() +
                   theme(legend.position  = "right")
                   
     p3 <- ggplot(Budget_Constraint[!(Budget_Constraint$Measure %in% c("Total Income", "HCE")) & 
                                     (year(Budget_Constraint$Period) > 2015) &
                                     (Budget_Constraint$Derived_Total == "HCE"),], 
                   aes(x = Period, 
                       y = Value, 
                       colour=Measure))     +
                   geom_line() +
                   geom_point() +
                   theme(legend.position  = "right")
                   
     p4 <- ggplot(Budget_Constraint[!(Budget_Constraint$Measure %in% c("Total Income", "HCE")) & 
                                     (year(Budget_Constraint$Period) > 2015) &
                                     (Budget_Constraint$Derived_Total == "Saving"),], 
                   aes(x = Period, 
                       y = Value, 
                       colour=Measure))     +
                   geom_line() +
                   geom_point() +
                   theme(legend.position  = "right")
                   
   print(p1, vp=vplayout(1,1))
   print(p2, vp=vplayout(2,1))
   print(p3, vp=vplayout(3,1))
   print(p4, vp=vplayout(4,1))
   dev.off() 

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2,2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  grid.rect()
  print(Maori,   vp = vplayout(1,1))
  print(Pacific, vp = vplayout(1,2))
  print(Asian,   vp = vplayout(2,1))
  print(Other,   vp = vplayout(2,2))



ggplot(Budget_Constraint[!(Budget_Constraint$Measure %in% c("Total Income")) & 
                          (year(Budget_Constraint$Period) > 2015) ,], 
       aes(x = Period, 
           y = Value, 
           colour=Measure))     +
       geom_line() +
       geom_point() +
       theme(legend.position  = "right")  
       
       
       
       +
       labs(title="New Zealand Production\nGross Domestic Product Measure - Actual and Estimated\n") +
       ylab("Gross Domestic Product\n$(Mill)") +
       scale_colour_manual(values = c("#7b1244","#0094c5"), name="Actual or Expected") +
       xlab("Time Period\n") +
       theme_bw(base_size=12, base_family =  "Calibri") %+replace%
       theme(legend.title.align=0.5,
             plot.margin = unit(c(1,3,1,1),"mm"),
             panel.border = element_blank(),
             strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
             strip.text = element_text(colour = "white", 
                                       size   = 8,
                                       family = "MyriadPro-Bold",
                                       margin = margin(0.0,0.0,0.0,0.0, unit = "mm")),
             panel.spacing = unit(1, "lines"),                                              
             legend.text   = element_text(size = 20, family = "MyriadPro-Regular"),
             plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
             plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
             axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "bottom")  

