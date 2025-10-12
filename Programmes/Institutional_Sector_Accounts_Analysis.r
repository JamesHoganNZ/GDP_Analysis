##
##    Programme:  Institutional_Sector_Accounts_Analysis.r
##
##    Objective:  Since 2021 New Zealand's been economically slammed. But as the production functions is showing
##                its not happening on the supply side. Its on the demand side. Someone say fucking Reserve Bank...
##
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
      load("Data_Output/ISA_Consolidated_Accounts.rda")
      load("Data_Output/Institutional_Sector_Accounts.rda")
      load("Data_Output/ISA_SupplementaryTable15A.rda")
      load("Data_Output/ISA_SupplementaryTable15B.rda")
      load("Data_Output/ISA_Consolidated_Accounts.rda")
      load("Data_Output/Output_Gap.rda")
                     
   ##
   ## Step 1: Lets have a look at household wealth
   ##
         
         ggplot(ISA_SupplementaryTable15A[ISA_SupplementaryTable15A$Group == "As a percentage of household disposable income",],
                aes(x = Period, 
                    y = Value/100,
                    colour = Series_name))     +
                geom_line(size = 1) +

                scale_y_continuous(breaks=seq(0,12,1), labels=paste0(seq(0,1200,100),"%")) + 
                scale_x_date(date_breaks = "1 year", date_labels = "%Y")+                
                scale_colour_manual(values = SPCColours(), name="Components") +

                ylab("Percentage\nHousehold Disposable Income\n") +
                xlab("\nPeriod") +                
                labs(title="Household Balance Sheet",
                     subtitle = "Relative To Income\n")  +         
         
                theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                theme(legend.title.align=0.5,
                      plot.margin = unit(c(1,3,1,1),"mm"),
                      panel.border = element_blank(),
                      strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                      strip.text = element_text(colour = "white", 
                                                size   = 13,
                                                family = "MyriadPro-Bold",
                                                margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                      panel.spacing = unit(1, "lines"),                                              
                      legend.text   = element_text(size = 20, family = "MyriadPro-Regular"),
                      plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      plot.subtitle = element_text(size = 14, colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                      axis.text.y   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(1, "cm"),
                      legend.margin = margin(10, 10, 10, 10),
                      legend.position  = "bottom")
         ggsave("Graphical_Output/Household Balance Sheet Relative To Income.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

   ##
   ## Step 2: Ok, that was interesting - lets rebase it... 
   ##
      hhrelinc <- ISA_SupplementaryTable15A[(ISA_SupplementaryTable15A$Group == "As a percentage of household disposable income"),]
      hhrelinc <- merge(hhrelinc,  
                        hhrelinc[(min(hhrelinc$Period) == hhrelinc$Period), c("Series_name", "Value")],
                        by = c("Series_name"))
      hhrelinc$Index <- hhrelinc$Value.x / hhrelinc$Value.y
    
      ggplot(hhrelinc,
             aes(x = Period, 
                 y = Index,
                 colour = Series_name))     +
             geom_line(size = 1) +

             scale_x_date(date_breaks = "1 year", date_labels = "%Y")+                
             scale_colour_manual(values = SPCColours(), name="Components") +

             ylab("Relative Change\nBase March 2000") +
             xlab("\nPeriod") +                
             labs(title="Change in Household Balance Sheet",
                  subtitle = "Relative To Income\n")  +         
      
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 20, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")
         ggsave("Graphical_Output/Growth in Household Balance Sheet Relative To Income.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


   ##
   ## Step 2: Lets add household net wealth to the output gap
   ##
      ##
      ##    Estimate growth
      ##
   
      Output_Gap <- merge(Output_Gap,
                          ISA_SupplementaryTable15A[(ISA_SupplementaryTable15A$Group == "As a percentage of household disposable income") &
                                                    (ISA_SupplementaryTable15A$Series_name == "Household net wealth"),c("Period","Value")],
                          by.x = "TimePeriod",
                          by.y = "Period")
                          
      names(Output_Gap)[names(Output_Gap) == 'Value'] = "Household_net_wealth"
      
      Output_Gap$Change_in_HHld_Wealth <- NA
      Output_Gap$Change_in_Unemployment <- NA
      for(i in 5:nrow(Output_Gap))
      {
         Output_Gap$Change_in_HHld_Wealth[i] <- (Output_Gap$Household_net_wealth[i] / Output_Gap$Household_net_wealth[(i-4)])-1
         Output_Gap$Change_in_Unemployment[i] <- (Output_Gap$Unemployment_Rate[i] - Output_Gap$Unemployment_Rate[(i-4)])-1
      }
                          
   
      ggplot(Output_Gap, aes(x=Change_in_HHld_Wealth, y=Change_in_Unemployment))     +
             geom_smooth(method=lm) +
             geom_path(size = 1, linejoin = "mitre", lineend = "butt", colour = c("#7b1244")) +
             geom_point(size = 1.5, colour = c("#0094c5")) +
             geom_text(aes(label=format(TimePeriod, "%Y")), size=7, nudge_x = 0.003) 
             
             

        ##
        ##     Regress it...
        ##
         OLS_Unemployment  <- lm(Change_in_Unemployment ~ Output_Gap, data=Output_Gap)
         OLS_Unemployment1 <- lm(Change_in_Unemployment ~ Output_Gap + Change_in_HHld_Wealth, data=Output_Gap)
         OLS_Unemployment2 <- lm(Change_in_Unemployment ~ Output_Gap + Change_in_HHld_Wealth +  CPI_Quarterly_Inflation, data=Output_Gap)

         anova(OLS_Unemployment, OLS_Unemployment1)
         anova(OLS_Unemployment, OLS_Unemployment2)

         summary(OLS_Unemployment2)

         GLS_Unemployment <- gls(log(Unemployment_Rate) ~ log(Output_Gap), 
                                  data=Output_Gap[!is.na(Output_Gap$Output_Gap),],
                                  correlation = corAR1(form = ~ TimePeriod))
         summary(GLS_Unemployment)             
   
   ##
   ## Step 3: 
   ##

   ##
   ## Step 4: 
   ##         
   ##





   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
