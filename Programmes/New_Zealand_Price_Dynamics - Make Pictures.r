##
##    Programme:  New_Zealand_Price_Dynamics.r
##
##    Objective:  
##
##
##    https://medium.com/@marc.jacobs012/cointegration-of-time-series-in-r-a6543dacf66e
##
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
                                                      

      ggplot(Real_Loans_By_Industry[Industry == "Households - Total",], 
             aes(x = Period, 
                 y = Real_Loans_By_Industry, 
                 colour = Industry))     +
             geom_line(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Constant Price Household Debt\nBase - March 2017\n", 
                  title="Constant Price Household Loans\n",
                  caption = "RBNZ: 'Assets - Loans & Repos by Industry - S34' deflated by CPI") +
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
                      
         ggsave("Graphical_Output/Household Loans - Qty_Fisher_Ideal_Prices.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

      ggplot(Real_Loans_By_Industry[Industry == "Households - Total",], 
             aes(x = Period, 
                 y = Quarterly_PC, 
                 colour = Industry))     +
             geom_smooth(size =1, colour = SPCColours("Red"), span = 0.3) +
             #geom_smooth(size =1, colour = SPCColours("Red")) +
             geom_point(size =1, colour = SPCColours("Purple")) +
             geom_hline(yintercept = 0, size = 1,  colour = SPCColours("Green")) +
             scale_y_continuous(labels = scales::label_percent(),breaks = seq(from=-2.0, to=2.0, by=.10)) +
             scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
             facet_grid(~Industry, scales="free") +
             labs(x = "\nTime Period", 
                  y = "Monthly Percentage Change", 
                  title="Monthly Growth in Constant Price Household Loans\n",
                  caption = "RBNZ: 'Assets - Loans & Repos by Industry - S34' deflated by CPI") +
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

                      
         ggsave("Graphical_Output/Household Loans - Monthly Change_Fisher_Ideal_Prices.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))



