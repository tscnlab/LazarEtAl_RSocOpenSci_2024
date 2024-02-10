###preparing environment: unloaded prior packages, loading new ones----

#unload packages that were loaded before (run function twice to "catch" all pkgs)
#this is a workaround to avoid masking problems when running the scripts successively
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# Libraries
library(tidyverse)
library(ggthemes)
library(cowplot)
library(scales);
library(ggpmisc)
library(ggpubr)
library(gridExtra)
library(grid);
library(gridtext)
library(gt)

set.seed(20230703) 


### [1] Loading subdatasets---------------------------------------------------

load(file="./05_analysis/subdata/conf_subdata.rda")

### [2] Source ggplot functions-------------------------------------------------------

#use ggplot functions defined in the functions-script
source("./05_analysis/ggplot_functions.R")


### [3] Weather & light conditions------------------------------------------

#Figure 3 (bottom panel)
#using the ggweather function to plot the weather data across all trials
#x-axis: weather (categorical);  flipped Y-axis: mEDI flipped 
#violin plots with boxplots included
#remove axis titles for combining it with histogram 
Fieldweatherplot <- ggweather(weatherdata)+
  labs(x=NULL, 
       y = NULL)

Fieldweatherplot


#Figure 3 (top panel)
#using the ggweather_hist function to plot the weather data across all trials
# x-axis: mEDI; y-axis: probability density 
#density plot
#remove axis titles for combining it with the violin plots
Fieldweatherhist <- ggweather_hist(weatherdata, "Mel_EDI")+
  labs(x=NULL)

Fieldweatherhist



#align the plot axes vertically for combining them stacked on top of each other
info_plots <- align_plots(Fieldweatherhist, Fieldweatherplot,
                          align = "v",
                          axis = 't')

#X-axis label
 bottom4 <- textGrob("Melanopic EDI [lx]", 
                     gp = gpar(fontsize = 11),
                    #vjust=-2, 
                    hjust = 0.7
                    )#, col="red"))
 
#create layout for plotting tehse subplots stacked over each other
lay4 <- rbind(
  c(1),
  c(2))

#Title Figure
top4 <- text_grob("Distribution of light intensity", 
                  just = "centre",
                  size=12,
                  face = "bold",
                  col="white",
                  vjust = 0.5 )

#combine the weather subplots into one plot (Figure 3)
info_panel <- grid.arrange(info_plots[[1]], info_plots[[2]], 
                           nrow=2,
                           bottom = bottom4,
                           top = top4
                           )

#save Figure 3
#in case you encounter problems during the pdf saving process, this could be
#related to the cairo_pdf device (especially the case for MACOS users)
#In that case try deleting "device=cairo_pdf" in following code bit below
ggsave("06_output/weath_panels.pdf", plot = info_panel ,
       width = 90, height = 100, units = "mm",
       bg = "white", device=cairo_pdf)



#add figure comparing alpha-opic values (Suppl. Figure 2) ----------------------

modified_labels <- c("L-cone-opic", "M-cone-opic", "S-cone-opic EDI")

ao_EDI <- ggalphaopic(weatherdata, xvar1="LCone_EDI",xvar2= "MCone_EDI",xvar3="SCone_EDI",)+
scale_fill_discrete(labels = modified_labels)+
  labs(x= "Light level [lx]")


modified_labels <- c("mEDI", "Illuminance", "rhodopic EDI"
                     )

medivsillu <- ggalphaopic(weatherdata, xvar1="Mel_EDI",xvar2= "phot_lux", xvar3="Rod_EDI" 
                          )+
  scale_fill_manual(labels = modified_labels,
                    values = c("gold", "purple","turquoise"
                                                         ))+
  labs(x= "Light level [lx]")
  

SupplFig_density<- grid.arrange(medivsillu, ao_EDI, 
                           nrow=1#,
                           #bottom = bottom4,
                           #top = top4
)

ggsave("06_output/suppl/SupplFig2.pdf", plot = SupplFig_density ,
       width = 200, height = 100, units = "mm",
       bg = "white", device=cairo_pdf)

ggsave("06_output/suppl/SupplFig2.png", plot = SupplFig_density ,
       width = 200, height = 100, units = "mm",
       bg = "white")




### [4] Case data age comparison-------------------------------------------------------------

#Figure 6: comparing age case data, dose response
#use the agecomp dataset for comparing a typical 18-year old & 87-year-old subject
#x-axis: mEDI; y-axis: pupil size (mm) for a single participant
# horizontal lines give maximum pupil range

#create top labels for the subplots
wraplabs <- c(`SP038`="18-year-old participant",
              `SP076`="87-year-old participant")

#create horizontal lines with maximum - minimum pupil range
hline_dat = data.frame(id=c(id="SP076","SP038"),
                       Minpup = c(min(agecomp$diameter_3d[agecomp$id == "SP076"]),
                                  min(agecomp$diameter_3d[agecomp$id == "SP038"])),
                       Maxpup =c(max(agecomp$diameter_3d[agecomp$id == "SP076"]),
                                 max(agecomp$diameter_3d[agecomp$id == "SP038"]))
)
#create the plot via the ggdr_mel function and add the horizontal lines
agecomp_plot <- ggdr_mel(agecomp)+
  geom_hline(aes(yintercept=Maxpup, colour = id), data=hline_dat)+
  geom_hline(aes(yintercept=Minpup, colour = id), data=hline_dat)+
  labs(x="Melanopic EDI [lx]")
agecomp_plot

ggsave("06_output/agecomp_plot.pdf", plot = agecomp_plot ,
       width = 159.2 *(2.998/5), height = 80, units = "mm",
       bg = "white")

#Suppl Figure 7: comparing age case data, dose response
#use the agecomp dataset for comparing a typical 18-year old & 87-year-old subject
#x-axis: phot. illum. ; y-axis: pupil size (mm) for a single participant
# horizontal lines give maximum pupil range

#create the plot via the ggdr_mel function and add the horizontal lines
agecomp_plot_lux <- ggdr_lux(agecomp)+
  geom_hline(aes(yintercept=Maxpup, colour = id), data=hline_dat)+
  geom_hline(aes(yintercept=Minpup, colour = id), data=hline_dat)+
  labs()
agecomp_plot_lux

ggsave("06_output/agecomp_plot_lux.pdf", plot = agecomp_plot_lux ,
       width = 159.2 *(2.998/5), height = 80, units = "mm", 
       bg = "white")


### [5] Age effect across light conditions--------------------------------------

#Figure 7 E
#using ggagelight function on the different light intensity sub-datasets 
#(median pupil size per id under these conditions)
# and annotating text

# median pupil size in field data over 1k phot lux as a function of age

plot_over1klux <- ggagelight(Fieldage_over1klux)+
  labs(x=NULL, y=NULL, title = "E",
       subtitle = "Field: >1000 lux")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(color = "black"))


plot_over1klux

#run linear regression with the median pupil size predicted by age
lm_over1klux <- lm(Median_pupil ~ age, Fieldage_over1klux)

lm_over1klux
#save confidence intervals for the linear regression
ci_lm_over1klux<- confint(lm_over1klux)

#Figure 7 D (100 lx to 1k lx mEDI)
#using ggagelight function on the different light intensity sub-datasets 
#(median pupil size per id under these conditions)
# and annotating text
#remove labs for later combination with other subplots

plot_to1klux <- ggagelight(Fieldage_to1klux)+
  labs(x=NULL, y=NULL, title = "D",
       subtitle = "Field: >100 & \u22641000 lx")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(color = "black"))

plot_to1klux

#run linear regression with the median pupil size predicted by age
lm_to1klux <- lm(Median_pupil ~ age, Fieldage_to1klux)

lm_to1klux
#save confidence intervals for the linear regression
ci_lm_to1klux<- confint(lm_to1klux)


#Figure 7 C (10 lx to 100 lx mEDI)
#using ggagelight function on the different light intensity sub-datasets 
#(median pupil size per id under these conditions)
# and annotating text
#remove labs for later combination with other subplots

plot_to100lux <- ggagelight(Fieldage_to100lux)+
  # annotate("text", x = 55, y = 8, 
  #label = "Condition: 10 - 100 lux")+
  labs(x=NULL, y=NULL, title = "C",
       subtitle = "Field: >10 & \u2264100 lx")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(color = "black"))

plot_to100lux

lm_to100lux <- lm(Median_pupil ~ age, Fieldage_to100lux)

#run linear regression with the median pupil size predicted by age
lm_to100lux
#save confidence intervals for the linear regression
ci_lm_to100lux<- confint(lm_to100lux)

#Figure 7 B (1 lx to 10 lx mEDI)
#using ggagelight function on the different light intensity sub-datasets 
#(median pupil size per id under these conditions)
# and annotating text
#remove labs for later combination with other subplots

plot_to10lux <- ggagelight(Fieldage_to10lux)+
  labs(x=NULL, y=NULL, title = "B",
       subtitle = "Field: >1 & \u226410 lux")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(color = "black"))

plot_to10lux

#run linear regression with the median pupil size predicted by age
lm_to10lux <- lm(Median_pupil ~ age, Fieldage_to10lux)

lm_to10lux
#save confidence intervals for the linear regression
ci_lm_to10lux<- confint(lm_to10lux)

#Figure 7 A (10-min dark-adaptation data - positive control data)
#using ggagelight function on the different light intensity sub-datasets 
#(median pupil size per id under these conditions)
# and annotating text
#remove labs for later combination with other subplots
plot_Darkage <- ggagelight(Darkage)+
  labs(x=NULL, y=NULL, title = "A",
       subtitle = "10-min dark adaptation")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(color = "black"))

plot_Darkage

# computing linear fit 
lm_darkage <- lm(Median_pupil ~ age, Darkage)

lm_darkage
#save confidence intervals for the linear regression
ci_lm_darkage<- confint(lm_darkage)


#plot all panels (A-E) in one large Figure 7

#create layout for panels
lay <- rbind( 
  c(1, 2, 3, 4, 5)
  
)    

#align the subplots as panels of one Figure
Winn_plot1 <- align_plots(plot_Darkage, plot_to10lux, plot_to100lux,
                          plot_to1klux, plot_over1klux,
                          align = "hv", axis = 'b')

#Y-Axis Label for Figure 7
yleft <- textGrob("Median pupil diameter [mm]", 
                  rot = 90, gp = gpar(fontsize = 11))#, vjust = 2)

#X-Axis label for Figure 7
bottom <- textGrob("Age [years]", 
                   #vjust=-10,
                   gp = gpar(fontsize = 11),
                   )#, col="red"))

#Figure title
top<- text_grob("Pupil diameter as a function of age across light conditions", 
                  just = "centre",
                  size=12,
                  #vjust = 5
                  face = "bold",
                  col="white"
                  )

#arrange the panels to one large Figure
Winn_panel1 <- grid.arrange(Winn_plot1[[1]], Winn_plot1[[2]],
                            Winn_plot1[[3]],  Winn_plot1[[4]],
                            Winn_plot1[[5]],
                            layout_matrix = lay, 
                            left = yleft, bottom = bottom, top = top)


#Din A 4 Landscape
#in case you encounter problems during the pdf saving process, this could be
#related to the cairo_pdf device (especially the case for MACOS users)
#In that case try deleting "device=cairo_pdf" in following code bit below
ggsave("06_output/age_panels1.pdf", plot = Winn_panel1 ,
       width = 246.2, height = 80, units = "mm", 
       bg = "white", device=cairo_pdf)


#Saving the age slope data in a data frame

#initiate Slope data.frame
Slopedata <- data.frame(light_intensity = c(NA,NA,NA,NA,NA),
                        slope = c(NA,NA,NA,NA,NA),
                        intercept =c(NA,NA,NA,NA,NA),
                        slci_low =c(NA,NA,NA,NA,NA),
                        slci_up =c(NA,NA,NA,NA,NA),
                        intci_low =c(NA,NA,NA,NA,NA),
                        intci_up =c(NA,NA,NA,NA,NA)
                        ) 

#create arbitrary light data in 1 log unit steps
Slopedata$light_intensity <- c(0.1, 1,10,
                               100, 1000)


#fill slope values derived from the linear fit data
Slopedata$slope <- c(lm_darkage[["coefficients"]][["age"]]*10,
                     lm_to10lux[["coefficients"]][["age"]]*10,
                     lm_to100lux[["coefficients"]][["age"]]*10,
                     lm_to1klux[["coefficients"]][["age"]]*10,
                     lm_over1klux[["coefficients"]][["age"]]*10)

#fill intercept values derived from the linear fit data
Slopedata$intercept <- c(lm_darkage[["coefficients"]][["(Intercept)"]],
                         lm_to10lux[["coefficients"]][["(Intercept)"]],
                         lm_to100lux[["coefficients"]][["(Intercept)"]],
                         lm_to1klux[["coefficients"]][["(Intercept)"]],
                         lm_over1klux[["coefficients"]][["(Intercept)"]])

#fill lower confidence interval values derived from the linear fit data: slopes
Slopedata$slci_low <- c(ci_lm_darkage[2,1]*10,
                        ci_lm_to10lux[2,1]*10,
                        ci_lm_to100lux[2,1]*10,
                        ci_lm_to1klux[2,1]*10,
                        ci_lm_over1klux[2,1]*10)

#fill upper confidence interval values derived from the linear fit data: slopes
Slopedata$slci_up <- c(ci_lm_darkage[2,2]*10,
                        ci_lm_to10lux[2,2]*10,
                        ci_lm_to100lux[2,2]*10,
                        ci_lm_to1klux[2,2]*10,
                        ci_lm_over1klux[2,2]*10)

#fill lower confidence interval values derived from the linear fit data: intercepts
Slopedata$intci_low <- c(ci_lm_darkage[1,1],
                        ci_lm_to10lux[1,1],
                        ci_lm_to100lux[1,1],
                        ci_lm_to1klux[1,1],
                        ci_lm_over1klux[1,1])

#fill upper confidence interval values derived from the linear fit data: intercepts
Slopedata$intci_up <- c(ci_lm_darkage[1,2],
                         ci_lm_to10lux[1,2],
                         ci_lm_to100lux[1,2],
                         ci_lm_to1klux[1,2],
                         ci_lm_over1klux[1,2])


# Figure 8 A Plotting the slope data across the clustered light intensities

ageslopeplot <- ggageslope(Slopedata)+
labs(x = NULL,
     y = "Pupil size reduction [mm/decade]",
     title = "A",
     subtitle = "Age effect slopes"
)
ageslopeplot


# Figure 8 B Plotting the intercept data across the clustered light intensities

ageintplot <- ggageintercept(Slopedata)+
  labs( x=NULL,
        y = "Pupil size intercept [mm]",
        title = "B",
      subtitle = "Age effect intercepts"
  )
ageintplot


#parameters & CIs for the pupilrange plot data
pupilrange_lm <- lm(Range_pupil ~ age, Pupilage)
confint(pupilrange_lm)

# Figure 8 C Plotting the pupil range data

rangeplot <- ggpupilrange(Pupilage)+
  labs( #x=NULL,
        #y=NULL,
        x = "Age [years]",
        y = "Max.- min. pupil size [mm]",
        title = "C",
        subtitle = "Pupil size range"
  )


#align the axes of the panels for Figure 8

Winn_plot2 <- align_plots(ageslopeplot, ageintplot, rangeplot,
                          align = "hv", axis = 'b')

#Din A 4 page distibution
246.2*(3/5)

#create layout
lay2b <- rbind(c(1, 2,3)
              
)

#create x Axis for Figure 8 A & B
bottom2b <- textGrob("Melanopic EDI range [lx]", gp = gpar(fontsize = 11),
                    vjust=-1.7, 
                    hjust = 1 )# hjust = 0.4, col="red"))

#Combine the subplots to 1 large Figure 8
Winn_panel2b <- grid.arrange(Winn_plot2[[1]], 
                            Winn_plot2[[2]],
                            Winn_plot2[[3]],
                            layout_matrix = lay2b,
                            bottom = bottom2b
)

#save Figure 8 adapted to the size of the axes of the other plots
#in case you encounter problems during the pdf saving process, this could be
#related to the cairo_pdf device (especially the case for MACOS users)
#In that case try deleting "device=cairo_pdf" in following code bit below
ggsave("06_output/age_panels2.pdf", plot = Winn_panel2b ,
       width = 246.2*(3.4389/5), height = 85, units = "mm", 
       bg = "white", device = cairo_pdf)



### [6] Autocorrelation ------------------
#Suppl Figure 8 - Autocorrelations

#using the ggacf helper function to create Suppl Figure 4 A
#get rid of axis labels for subplot for larger Figure
mel_acf_plot<- ggacf(mel_acf)+ 
  labs(x = "", 
       y = "",
       title = "A",
       subtitle = "Autocorr. melanopic EDI")

#using the ggacf helper function to create Suppl Figure 4 B

pupil_acf_plot <- ggacf(pupil_acf)+ 
  labs(x = "", 
       y = "",
       title = "B",
       subtitle = "Autocorr. pupil size")

#create layout
lay3 <- rbind( 
  c(1, 2)
  
)    

#align the axes of the subplots 
autocor_plots <- align_plots(mel_acf_plot, pupil_acf_plot,
                             align = "hv", axis = 'b')


#Y-Axis Label for Suppl. Figure 8
yleft3 <- textGrob("Autocorrelation", 
                   rot = 90, 
                   gp = gpar(fontsize = 11),
                   vjust= 1.5)

#X-Axis Label for Suppl. Figure 8
bottom3 <- textGrob("Sample lag, t0 - [minute]",
                    gp = gpar(fontsize = 11),
                    vjust= -1.5
)



#Combine the subplots to one Figure (Suppl Figure 8)

autocor_panel <- grid.arrange(autocor_plots[[1]], autocor_plots[[2]],
                              layout_matrix = lay3, 
                              left = yleft3, bottom = bottom3)


#41.5 mm x 41.5 mm subplot axes
ggsave("06_output/suppl/autocor_panels.pdf", plot = autocor_panel ,
       width = 159.2*(2.28242/3), height = 80, units = "mm", 
       bg = "white")


### [7] Data loss threshold--------------------------------------------------------------
#Figure 4 Data loss plot
#create a vector with varying data loss thresholds from 0 to 100% (in 5% steps)
data_loss_thresvec <- seq(0,1, 0.05)


#create empty "dataloss over threshold" vector
lossover_thrsvec <- NA

load(file="./02_rawdata/uniqloss.rda")


#create a for loop that goes through all participants' data loss values
#and checks whether each value is exceeding each given data loss threshold (0 to 100%)
#and then sums up the number of participants that are exceeding each threshold

for (i in 1:length(data_loss_thresvec))
{
  lossover_thrsvec[i] <- length(uniqloss[uniqloss > data_loss_thresvec[i]])
}

#match the number of values over threshold with the threshold values
dataloss_rel <- data.frame(data_loss_thresvec,lossover_thrsvec)

# this is used for Figure 4 for  plotting the data loss thresholds vs.
#number of participants over threshold

#The adjusted threshold should be placed at the location over 50%, 
#which comes before the smallest increase of excluded participants 
#across possible thresholds in 5% steps
# In this data the smallest increase of people over the threshold
#is given in the step from 75% to 80% (+0 subjects over threshold)
#thus  leading to an adjusted threshold of 75, #yielding n=4 excluded 
#participants and retaining n=83.

dataloss_plot <- ggplot(data=dataloss_rel, aes(x=data_loss_thresvec, y=lossover_thrsvec))+
  scale_x_continuous(limits=c(0,1), expand = c(0, 0),
                     breaks =seq(0.1,1,0.2),
                               labels=seq(10,100,20)
                               )+
  scale_y_continuous(breaks = seq(0,90,15), limits = c(0,90), expand = c(0, 0))+
  geom_rect(mapping=aes(xmin=0, xmax=1, ymin=0, ymax=4),
            fill="salmon", alpha=0.1)+
  geom_line(size=0.8)+
  geom_vline(xintercept = 0.75, linetype= "dashed" , colour="salmon")+
  geom_vline(xintercept = 0.5, linetype= "dashed" , colour="dodgerblue1")+
  labs(#title = "Determining the pupil data loss threshold"  
       x = "Data loss threshold [%]",
       y = "Subjects over threshold [n]"
       )+
  #annotate("text", x = 0.75, y = 2, label = "n=4", size=8/.pt )+
  geom_label(x=0.62, y=30, label="n=24", size=8/.pt, fill="dodgerblue1")+
  geom_label(x=0.85, y=12, label="n=4", size=8/.pt, fill="salmon")+
  theme_classic()+
  theme(legend.position = "none", 
        plot.title = element_text(size = 11, 
                                  face = "bold"),
        plot.subtitle = element_text(color = "black"),
        panel.background = element_rect(fill = "white",
                                        size = 2, 
                                        linetype = "solid"),
        aspect.ratio = 1/1
  )

dataloss_plot

#save the plot
#Axes of the plot are 41.5 mm x 41.5 mm
ggsave("06_output/dataloss_plot.pdf", plot = dataloss_plot ,
       width = 159.2/2.92, height = 60, units = "mm",
       bg = "white")


### [8] Light condition comparison -------------------------------------------------

#Figure 5 Light data comparison


#create new variable for Labdata (MP_ratio group)
Labdata$MPratio_group <- NA 
#label Laboratory data by means of MPratio values
Labdata$MPratio_group[Labdata$phot_lux > 3 & Labdata$MPratio<=0.35] <-"red"
Labdata$MPratio_group[Labdata$phot_lux > 3 & Labdata$MPratio>0.35 & Labdata$MPratio<=0.96 ] <-"green"
Labdata$MPratio_group[Labdata$phot_lux > 3 & Labdata$MPratio>0.96 & Labdata$MPratio<=2 ] <-"white"
Labdata$MPratio_group[Labdata$phot_lux > 3 & Labdata$MPratio>8] <-"blue"
Fielddata$MPratio_group <- "field"

#create colour vector for distinguishing the different laboratory light conditions
#(part of the positive control data)
# my_colors <- c("green","red", "steelblue", "white")
my_colors <- c("steelblue","green", "red", "white")

#compute correlation coefficient for the lab data (default method="pearson")
#Panel A for Figure 5
cor_coef1 <- cor(Labdata$Mel_EDI, Labdata$phot_lux)

#create Panel A for Figure 5 
lab_lux_vs_Mel <- gglightcor(Labdata)+
  geom_point(size=2.25, alpha=0.6, shape=21)+
  scale_fill_manual(values = my_colors)+
  geom_segment(
    aes(x = 15, y = 3000, xend = 120, yend = 2000, col="steelblue"),
    arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x=15, y=3000, label="B LED"), size=8/.pt,
             fill="steelblue")+
  #arrow for green
  geom_segment(
    aes(x = 5000, y = 15, xend = 600, yend = 15, col="green"),
    arrow = arrow(length = unit(0.03, "npc")))+
  
  #black arrow for white
  geom_segment(
    aes(x = 11000, y = 1500, xend = 1200, yend = 900),
    arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x=11000, y=1500, label="W LED"), size=8/.pt,
             fill="white")+
  #red arrow and label
  geom_segment(
    aes(x = 4500, y = 300, xend = 1100, yend = 500, col="red"),
    arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x=11000, y=15, label="R LED"), size=8/.pt,
             fill="red")+
  #label for green
  geom_label(aes(x=11000, y=300, label="G LED"), size=8/.pt,
             fill="green")+
  labs(x = "", 
       y = "",
       title = "A",
       subtitle = "Laboratory conditions")+
  geom_text(aes(label = paste("r=",round(cor_coef1,3))),
            x = Inf, y = -Inf, hjust = 1, vjust = -0.5)


#compute correlation coefficient for the field data (default method="pearson")
#Panel B for Figure 5
cor_coef2 <- cor(Fielddata$Mel_EDI, Fielddata$phot_lux)

#create Panel A for Figure 5
field_lux_vs_Mel <- gglightcor(Fielddata)+
  scale_fill_manual(values = "gray95")+
  labs(x = "", 
       y = "",
       title = "B",
       subtitle = "Field conditions")+
  geom_text(aes(label = paste0("r=",round(cor_coef2,3))),
            x = Inf, y = -Inf, hjust = 1, vjust = -0.5)

#align the subplots A & B
lightcomp_plots <- align_plots(lab_lux_vs_Mel, field_lux_vs_Mel,
                               align = "hv", axis = 'b')


#Figure 5 Y-axis label
yleft_lightcomp <- textGrob("Melanopic EDI [lx]", 
                            rot = 90, 
                            gp = gpar(fontsize = 11),
                            vjust= 1.5)

#Figure 5 X-axis label
bottom_lightcomp <- textGrob("Photopic Illuminance [lx]",
                             gp = gpar(fontsize = 11),
                             vjust= -1.5
)

#create layout for plotting
lay3 <- rbind( 
  c(1, 2)
  
)    

#create Figure 5 from Panel A & B
lightcomp_panel <- grid.arrange(lightcomp_plots[[1]], lightcomp_plots[[2]],
                                layout_matrix = lay3, 
                                left = yleft_lightcomp, bottom = bottom_lightcomp)
#Save Figure 5
#41.5 mm x 41.5 mm subplot axes
ggsave("06_output/lightcomp_panels.pdf", plot = lightcomp_panel,
       width = 159.2*(2.28242/3), height = 70.5, units = "mm", 
       bg = "white")



### [9] Data tables------------------------------------------------------------------------
#Supplementary Tables 3-9

# summarizing each participant's data separated between field & positive control data
#Separate tables for pupil size & melanopic EDI

#Suppl. Table 3
# Subject ID
#Age
# data loss %
# min Pupil diameter
# Mmd Pupil diameter
# max Pupil diameter

#Suppl. Table 4
# Subject ID
#Age
# min mEDI
# med mEDI
# max mEDI



#create summary table for pupil size  from the field data
summary_data_field_ps <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Age"= mean(age, na.rm = T),
            "Data loss ratio [%]"  = round(100*(mean(data_loss, na.rm = T)), 2),
            "Min. pupil size [mm]" = round(min(diameter_3d, na.rm = T), 2),
            "Med. pupil size [mm]" = round(median(diameter_3d, na.rm = T), 2),
            "Max. pupil size [mm]" = round(max(diameter_3d, na.rm = T), 2)
  )

#check values for minimum, maximum, median 
min(summary_data_field_ps$`Data loss ratio [%]`)
max(summary_data_field_ps$`Data loss ratio [%]`)
median(summary_data_field_ps$`Data loss ratio [%]`)
sd(summary_data_field_ps$`Data loss ratio [%]`)

#create summary table for pupil size  from the positive control data (dark and lab data)

summary_data_lab_ps <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. pupil size [mm] " = round(min(diameter_3d, na.rm = T), 2),
            "Med. pupil size [mm] " = round(median(diameter_3d, na.rm = T), 2),
            "Max. pupil size [mm] " = round(max(diameter_3d, na.rm = T), 2)
  )

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_ps <- cbind(summary_data_field_ps,summary_data_lab_ps[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_ps <- suppl_table_ps %>% gt()  %>%  cols_align("right")%>%
  tab_spanner(
  label = 'Field data',
  columns = c(4, 5, 6)
  ) %>%  tab_spanner(
  label = 'Positive control data (lab. & dark-adapt.)',
  columns = c(7, 8, 9)
)
# modify the format of the table 
suppl_table_ps <- opt_table_font(data= suppl_table_ps, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_ps,        # save table as pdf
       filename = "06_output/suppl/sum_table_ps.pdf")


#
#create summary table for mEDI from the field data
summary_data_field_medi <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Age"= mean(age, na.rm = T),
            "Min. mEDI [lx]" = round(min(Mel_EDI, na.rm = T), 0),
            "Med. mEDI [lx]" = round(median(Mel_EDI, na.rm = T), 0),
            "Max. mEDI [lx]" = round(max(Mel_EDI, na.rm = T), 0)
  )


#create summary table for mEDI  data from the positive control data (dark & lab data)

summary_data_lab_medi <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Med. mEDI [lx] " = round(median(Mel_EDI, na.rm = T), 0),
            "Max. mEDI [lx] " = round(max(Mel_EDI, na.rm = T), 0)
  #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. mEDI [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_medi %>% relocate("Min. mEDI [lx] " , .after = ID) -> summary_data_lab_medi

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_medi <- cbind(summary_data_field_medi,summary_data_lab_medi[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_medi <- suppl_table_medi %>% gt() %>%  cols_align("right") %>%  
tab_spanner(
  label = 'Field data',
  columns = c(3, 4, 5)
) %>%  tab_spanner(
  label = 'Positive control data (lab. & dark adapt.)',
  columns = c(6, 7, 8)
)
# modify the format of the table 
suppl_table_medi <- opt_table_font(data= suppl_table_medi, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_medi,        # save table as pdf
       filename = "06_output/suppl/sum_table_mEDI.pdf")



#create summary table for S-cone-opic light data -------------------------

#
#create summary table for SCone EDI from the field data
summary_data_field_SCone <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. S-cone-opic EDI [lx]" = round(min(SCone_EDI, na.rm = T), 0),
            "Med. S-cone-opic EDI [lx]" = round(median(SCone_EDI, na.rm = T), 0),
            "Max. S-cone-opic EDI [lx]" = round(max(SCone_EDI, na.rm = T), 0)
  )



#create summary table for SCone EDI data from the positive control data (dark & lab data)

summary_data_lab_SCone <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise( "Med. S-cone-opic EDI [lx] " = round(median(SCone_EDI, na.rm = T), 0),
             "Max. S-cone-opic EDI [lx] " = round(max(SCone_EDI, na.rm = T), 0)
            #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. S-cone-opic EDI [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_SCone %>% relocate("Min. S-cone-opic EDI [lx] " , .after = ID) -> summary_data_lab_SCone

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_SCone <- cbind(summary_data_field_SCone,summary_data_lab_SCone[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_SCone <- suppl_table_SCone %>% gt() %>%  cols_align("right") %>%  
  tab_spanner(
    label = 'Field data',
    columns = c(2, 3, 4)
  ) %>%  tab_spanner(
    label = 'Positive control data (lab. & dark adapt.)',
    columns = c(5, 6, 7)
  )
# modify the format of the table 
suppl_table_SCone <- opt_table_font(data= suppl_table_SCone, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_SCone,        # save table as pdf
       filename = "06_output/suppl/sum_table_SCone.pdf")



#create summary table for rhodopic light data -------------------------

#
#create summary table for Rod_EDI from the field data
summary_data_field_rod <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. rhodopic EDI [lx]" = round(min(Rod_EDI, na.rm = T), 0),
            "Med. rhodopic EDI [lx]" = round(median(Rod_EDI, na.rm = T), 0),
            "Max. rhodopic EDI [lx]" = round(max(Rod_EDI, na.rm = T), 0)
  )



#create summary table for SCone_EDI  data from the positive control data (dark & lab data)

summary_data_lab_rod <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise( "Med. rhodopic EDI [lx] " = round(median(Rod_EDI, na.rm = T), 0),
             "Max. rhodopic EDI [lx] " = round(max(Rod_EDI, na.rm = T), 0)
             #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. rhodopic EDI [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_rod %>% relocate("Min. rhodopic EDI [lx] " , .after = ID) -> summary_data_lab_rod

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_rod <- cbind(summary_data_field_rod,summary_data_lab_rod[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_rod <- suppl_table_rod %>% gt() %>%  cols_align("right") %>%  
  tab_spanner(
    label = 'Field data',
    columns = c(2, 3, 4)
  ) %>%  tab_spanner(
    label = 'Positive control data (lab. & dark adapt.)',
    columns = c(5, 6, 7)
  )
# modify the format of the table 
suppl_table_rod <- opt_table_font(data= suppl_table_rod, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_rod,        # save table as pdf
       filename = "06_output/suppl/sum_table_rod.pdf")



#create summary table for M-cone-opic light data -------------------------

#
#create summary table for MCone_EDI from the field data
summary_data_field_MCone <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. M-cone-opic EDI [lx]" = round(min(MCone_EDI, na.rm = T), 0),
            "Med. M-cone-opic EDI [lx]" = round(median(MCone_EDI, na.rm = T), 0),
            "Max. M-cone-opic EDI [lx]" = round(max(MCone_EDI, na.rm = T), 0)
  )



#create summary table for MCone_EDI  data from the positive control data (dark & lab data)

summary_data_lab_MCone <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise( "Med. M-cone-opic EDI [lx] " = round(median(MCone_EDI, na.rm = T), 0),
             "Max. M-cone-opic EDI [lx] " = round(max(MCone_EDI, na.rm = T), 0)
             #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. M-cone-opic EDI [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_MCone %>% relocate("Min. M-cone-opic EDI [lx] " , .after = ID) -> summary_data_lab_MCone

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_MCone <- cbind(summary_data_field_MCone,summary_data_lab_MCone[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_MCone <- suppl_table_MCone %>% gt() %>%  cols_align("right") %>%  
  tab_spanner(
    label = 'Field data',
    columns = c(2, 3, 4)
  ) %>%  tab_spanner(
    label = 'Positive control data (lab. & dark adapt.)',
    columns = c(5, 6, 7)
  )
# modify the format of the table 
suppl_table_MCone <- opt_table_font(data= suppl_table_MCone, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_MCone,        # save table as pdf
       filename = "06_output/suppl/sum_table_MCone.pdf")





#create summary table for L-cone-opic light data -------------------------

#
#create summary table for LCone_EDI from the field data
summary_data_field_LCone <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. L-cone-opic EDI [lx]" = round(min(LCone_EDI, na.rm = T), 0),
            "Med. L-cone-opic EDI [lx]" = round(median(LCone_EDI, na.rm = T), 0),
            "Max. L-cone-opic EDI [lx]" = round(max(LCone_EDI, na.rm = T), 0)
  )



#create summary table for LCone_EDI  data from the positive control data (dark & lab data)

summary_data_lab_LCone <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise( "Med. L-cone-opic EDI [lx] " = round(median(LCone_EDI, na.rm = T), 0),
             "Max. L-cone-opic EDI [lx] " = round(max(LCone_EDI, na.rm = T), 0)
             #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. L-cone-opic EDI [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_LCone %>% relocate("Min. L-cone-opic EDI [lx] " , .after = ID) -> summary_data_lab_LCone

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_LCone <- cbind(summary_data_field_LCone,summary_data_lab_LCone[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_LCone <- suppl_table_LCone %>% gt() %>%  cols_align("right") %>%  
  tab_spanner(
    label = 'Field data',
    columns = c(2, 3, 4)
  ) %>%  tab_spanner(
    label = 'Positive control data (lab. & dark adapt.)',
    columns = c(5, 6, 7)
  )
# modify the format of the table 
suppl_table_LCone <- opt_table_font(data= suppl_table_LCone, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_LCone,        # save table as pdf
       filename = "06_output/suppl/sum_table_LCone.pdf")



#create summary table for photopic illuminance light data -------------------------

#
#create summary table forIlluminance from the field data
summary_data_field_phot_lux <- Fielddata %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise("Min. Photopic illuminance [lx]" = round(min(phot_lux, na.rm = T), 0),
            "Med. Photopic illuminance [lx]" = round(median(phot_lux, na.rm = T), 0),
            "Max. Photopic illuminance [lx]" = round(max(phot_lux, na.rm = T), 0)
  )

#create summary table for Illuminance data from the positive control data (dark & lab data)

summary_data_lab_phot_lux <- Darklab %>% rename ("ID" = "id" ) %>%
  group_by(ID)  %>%
  summarise( "Med. Photopic illuminance [lx] " = round(median(phot_lux, na.rm = T), 0),
             "Max. Photopic illuminance [lx] " = round(max(phot_lux, na.rm = T), 0)
             #Min mEDI corresponds. to light intensity in dark adaptation phase ~ 0      
  )%>% mutate("Min. Photopic illuminance [lx] " = rep("~0",times=83) )
#relocate the min mEDI column to after ID
summary_data_lab_phot_lux %>% relocate("Min. Photopic illuminance [lx] " , .after = ID) -> summary_data_lab_phot_lux

#merge the 2 tables (leave out the duplicated ID column)
suppl_table_phot_lux <- cbind(summary_data_field_phot_lux,summary_data_lab_phot_lux[,2:4])

#create table spanners two differentiate between field and positive control data
suppl_table_phot_lux <- suppl_table_phot_lux %>% gt() %>%  cols_align("right") %>%  
  tab_spanner(
    label = 'Field data',
    columns = c(2, 3, 4)
  ) %>%  tab_spanner(
    label = 'Positive control data (lab. & dark adapt.)',
    columns = c(5, 6, 7)
  )
# modify the format of the table 
suppl_table_phot_lux <- opt_table_font(data= suppl_table_phot_lux, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(13), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4))


gtsave(suppl_table_phot_lux,        # save table as pdf
       filename = "06_output/suppl/sum_table_phot_lux.pdf")


### [10] Linear regression assumptions (Suppl. Figures 3-6) ---------------------------------------------------
#load new packages
library(performance)
library(see)
library(patchwork)

#create a linear model for testing the untransformed light data (mEDI)
model1 <- lm(diameter_3d ~ Mel_EDI + age, data = Fielddata)

#use the check model function to test the linear assumptions in the model
#log10 transformed data
assumptest_lin <- check_model(model1, theme="ggplot2::theme_classic")
#show the plot
#assumptest_lin

#save as pdf manually: landscape full din A4 (width = 11.69 in, height = 8.27 in)
#(ggsave function did not work here)

#create  a linear model for testing the log10-transformed light data (mEDI)
model2 <- lm(diameter_3d ~ log_Mel_EDI + age, data = Fielddata)

#use the check model function to test the linear assumptions in the model
#untransformed data
assumptest_log <- check_model(model2, theme="ggplot2::theme_classic")
#show the plot
#assumptest_log

#create a linear model for testing the untransformed light data (phot. illum.)
model3 <- lm(diameter_3d ~ phot_lux + age, data = Fielddata)

#create a linear model for testing the untransformed light data (phot. illum.)
model4 <- lm(diameter_3d ~  log_phot_lux + age, data = Fielddata)


assumptest_lin_lux <- check_model(model3, theme="ggplot2::theme_classic")

#show the plot
#assumptest_lin_lux

assumptest_log_lux <- check_model(model4, theme="ggplot2::theme_classic")

#show the plot
#assumptest_log_lux


#save as pdf manually: (ggsave function did not work with these plots)
#use large screen and extend "Plot" window as far as possible
# then use "Export" - Save as PDF"
# format: landscape full din A4 (width = 11.69 in, height = 8.27 in)
