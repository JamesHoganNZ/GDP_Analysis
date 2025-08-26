##
##    Programme:  New_Zealand_Price_Dynamics.r
##
##    Objective:  
##
##
##    https://medium.com/@marc.jacobs012/cointegration-of-time-series-in-r-a6543dacf66e
##    https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
##
##
##
##
##
##
##
##
##    Author:     James Hogan, started 20 August 2025
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
      load("Data_Output/Money_Demand_Analytical_Set.rda")
   ##
   ##    Restrict to Housing
   ##
      Money_Demand_Analytical_Set <- Money_Demand_Analytical_Set[Money_Demand_Analytical_Set$Industry == "Households - Housing",]
                                                      

      ggplot(Money_Demand_Analytical_Set, 
             aes(x = Period, 
                 y = Loans_By_Industry, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "$Mill\n", 
                  #title="Loans to the Household Sector for Housing\n",
                  caption = "RBNZ: 'Assets - Loans & Repos by Industry - S34'") +
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
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/Loans to the Household Sector for Housing.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


      ggplot(Money_Demand_Analytical_Set, 
             aes(x = Period, 
                 y = Fisher_Ideal_House_Prices, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Index Value\n", 
                  #title="House Prices - Fisher Ideal\n",
                  caption = "Urban Development: https://catalogue.data.govt.nz/dataset/urban-development/resource/3bf60abe-ee4c-4b61-bc81-c38852817a1c") +
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
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/Fisher_Ideal_House_Prices.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

      ggplot(Money_Demand_Analytical_Set, 
             aes(x = Period, 
                 y = Real_Loans_By_Industry, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Index Value\n", 
                  #title="Deflated Mortgages - Housing Demand Measure\n",
                  caption = "Urban Development and RBNZ Data") +
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
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/Real_Loans_By_Industry.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


      ggplot(Money_Demand_Analytical_Set, 
             aes(x = Period, 
                 y = Floating_First_Mortgage_Interest_Rate, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "%\n", 
                  #title="Floating First Mortgage Interest Rate\n",
                  caption = "RBNZ hb3: Retail interest rates: Weighted average lending and deposits - B3") +
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
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/Floating_First_Mortgage_Interest_Rate.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


      ggplot(Money_Demand_Analytical_Set, 
             aes(x = Period, 
                 y = Labour_Paid_Hours_Worked, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             #facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "%\n", 
                  #title="Paid Hours Worked\n",
                  caption = "Stats NZ QES: Total Paid Hours by Industry") +
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
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "none")                      
         ggsave("Graphical_Output/Labour_Paid_Hours_Worked.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))


