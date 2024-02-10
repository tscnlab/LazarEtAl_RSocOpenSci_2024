### gg Plotting functions-----------------------------------------------------------------------------------------------------------

#plotting function for mEDI vs. pupil Size data
#ggdr_mel was used in producing Figure 6

ggdr_mel <- function(dataset) {
  ggplot(dataset,
         aes(x = Mel_EDI, y = diameter_3d, fill = id
         ))+
    #geom_hline(yintercept= max(y))+
    facet_wrap(~ id, labeller = as_labeller(wraplabs)
    )+
    scale_x_log10(limits=c(1,55000), 
                  breaks =c(1,10,100,1000,10000, 50000),
                  labels =c(1,10,100,"1k","10k", "50k"))+
    scale_y_continuous(breaks = seq(1,9,2), limits = c(1,9))+
    geom_point(size=2.25, alpha=0.6, shape=21)+
    labs(x = "Melanopic EDI [lux]",
         y = "Pupil diameter [mm]",
    )+
    geom_smooth(method = "lm", se=T, color="black", formula = my.formula,
                fullrange = T) +
    stat_poly_eq(formula = my.formula,
                 size = 2.81,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., 
                                   sep = "*plain(\",\")~")), 
                 parse = TRUE, label.x = 0.5, label.y = 0.018
    ) +
    theme_classic()+
    theme(legend.position = "none",  aspect.ratio = 1/1)
}


#plotting function for mEDI vs. pupil Size data
#ggdr_mel was used in producing Figure 6

ggdr_lux <- function(dataset) {
  ggplot(dataset,
         aes(x = phot_lux, y = diameter_3d, fill = id
         ))+
    #geom_hline(yintercept= max(y))+
    facet_wrap(~ id, labeller = as_labeller(wraplabs)
    )+
    scale_x_log10(limits=c(1,55000), 
                  breaks =c(1,10,100,1000,10000, 50000),
                  labels =c(1,10,100,"1k","10k", "50k"))+
    scale_y_continuous(breaks = seq(1,9,2), limits = c(1,9))+
    geom_point(size=2.25, alpha=0.6, shape=21)+
    labs(x = "Photopic Illuminance [lux]",
         y = "Pupil diameter [mm]",
    )+
    geom_smooth(method = "lm", se=T, color="black", formula = my.formula,
                fullrange = T) +
    stat_poly_eq(formula = my.formula,
                 size = 2.81,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., 
                                   sep = "*plain(\",\")~")), 
                 parse = TRUE, label.x = 0.5, label.y = 0.018
    ) +
    theme_classic()+
    theme(legend.position = "none",  aspect.ratio = 1/1)
}


#plotting function for light data comparison: phot. lux vs. mEDI
#gglightcor was used in producing Figure 5 

gglightcor <- function(dataset) {
  ggplot(dataset,
         aes(x = phot_lux , y = Mel_EDI, fill= MPratio_group)
         )+
    #geom_hline(yintercept= max(y))+
    #facet_wrap(~ exp_phase, labeller = as_labeller(wraplabs)
    #)+
    scale_x_log10(limits=c(1,55000),
                  breaks =c(1,10,100,1000,10000, 50000),
                  labels =c(1,10,100,"1k","10k", "50k"))+
    scale_y_log10(limits=c(1,55000),
                  breaks =c(1,10,100,1000,10000, 50000),
                  labels =c(1,10,100,"1k","10k", "50k"))+
    geom_abline(intercept=0, slope=1)+
    geom_point(size=2.25, alpha=0.6, shape=21#, fill="gray"
               )+
    #stat_cor(method = "pearson", label.x = 10, label.y = 50000)+
    labs(y = "Melanopic EDI [lx]",
         x = "Photopic illumimance [lx]",
    )+
    theme_classic()+
    theme(legend.position = "none", 
          plot.title = element_text(size = 11, 
                                    face = "bold"),
          aspect.ratio = 1/1)
}


#Plotting function for  age vs. median pupil size 
#dataset as input varible
#ggagelight was used in producing Figure 7 A-E


ggagelight <- function(dataset) {
  ggplot(dataset,
         aes(x = age, y = Median_pupil
         ))+ 
    scale_x_continuous(breaks = seq(10,90,10), limits = c(15,90)
    )+
    geom_linerange(aes(ymin = Q25, ymax = Q75,
                       x = age), #color ="cyan",
                   size = 1.5, alpha = 0.3, position = dodge) +
    scale_y_continuous(breaks = seq(1,9,2), limits = c(0,9))+
    geom_point(shape=22, size=2.25, alpha=0.6, colour="black", 
               aes(fill = `id` ))+
    geom_smooth(method = "lm", se=T, color="black", formula = my.formula,
                fullrange = T) +
    stat_poly_eq(formula = my.formula,
                 size = 2.81,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., 
                                   sep = "*plain(\",\")~")), 
                 parse = TRUE, label.x = 0.5, label.y = 0.018
    ) +
    labs(x = expression(paste(" Age [years]")),
         y = "Mean pupil diameter [mm]")+
    theme_classic()+
    theme(legend.position = "none",  aspect.ratio = 1/1)
}

#slope function that extracts the slope of the lm function
#used for preparing data for Figure 8
slope <- function(x, y) coef(lm(y ~ x))[[2]]

#incercept function that extracts the slope of the lm function
#used for preparing data for Figure 8
intercept <- function(x, y) coef(lm(y ~ x))[[1]]


#plotting function for plotting weather vs. mEDI
# as violin plots 
#ggweather was used in producing Figure 3 (bottom panel)

ggweather <- function(dataset) {
  ggplot(dataset, aes(x=weather, y=Mel_EDI, fill=weather))+ 
    geom_violin(trim=T)+
    scale_y_log10(limits=c(1,100000), 
                  breaks =c(1,10,100, 1000,10000,100000),
                  labels =c(1,10,100,"1k","10k", "100k"))+
    scale_x_discrete(position = "top")+
    geom_boxplot(width=0.1) +
    geom_hline(yintercept = 250, colour = "gray", linetype = "dashed"
    )+
    geom_hline(yintercept = 1000, colour = "gray"
    )+
    labs(x = " ",# Weather conditions during trial",
         y = "Melananopic EDI [lux]")+
    theme_classic()+
    coord_flip()+
    theme(legend.position = "none", 
          plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
          #plot.title = element_text(size = 11, 
                                    #face = "bold"),
          #plot.subtitle = element_text(color = "black"),
          panel.background = element_rect(fill = "white",
                                          size = 2, 
                                          linetype = "solid"),
          aspect.ratio = 1/1.09)+
    scale_fill_brewer(palette="Reds", aesthetics = "fill")
}


# plotting function for weather histogram with prob. density 
#bin breaks correspond to 0.5 log10 unit steps mEDI
#ggweather_hist was used in producing Figure 3 (top panel)

ggweather_hist <- function(dataset, x_var) {
  ggplot(dataset, aes(x = !!rlang::sym(x_var))) +
    geom_histogram(aes(y = ..density..),
                   fill = "white",
                   color = "black",
                   breaks = c(1, 10^0.5, 10, 10^1.5, 100, 10^2.5, 1000, 10^3.5, 10000,
                              10^4.5, 100000),
                   position = "identity", alpha = 0.5) +
    geom_density(alpha = .2, fill = "gold") +
    scale_x_log10(limits = c(1, 100000),
                  breaks = c(1, 10, 100, 1000, 10000, 100000),
                  labels = c(1, 10, 100, "1k", "10k", "100k")) +
    labs(x = paste("Melanopic EDI [", x_var, "]"),
         y = "Probability density") +
    geom_vline(xintercept = 250, colour = "gray", linetype = "dashed") +
    geom_vline(xintercept = 1000, colour = "gray") +
    theme_classic() +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.background = element_rect(fill = "white",
                                          size = 2,
                                          linetype = "solid"),
          aspect.ratio = 1/1)
}


#plotting Alpha-opic var with prob. density 


ggalphaopic <- function(dataset,
                        xvar1 = NULL,
                        xvar2 = NULL,
                        xvar3 = NULL,
                        xvar4 = NULL,
                        xvar5 = NULL) {

  p <- ggplot(dataset)

  if (!is.null(xvar1)) {
    p <- p + geom_density(alpha = .2, aes(x = !!rlang::sym(xvar1), fill = xvar1))
  }

  if (!is.null(xvar2)) {
    p <- p + geom_density(alpha = .2, aes(x = !!rlang::sym(xvar2), fill = xvar2))
  }

  if (!is.null(xvar3)) {
    p <- p + geom_density(alpha = .2, aes(x = !!rlang::sym(xvar3), fill = xvar3))
  }

  if (!is.null(xvar4)) {
    p <- p + geom_density(alpha = .2, aes(x = !!rlang::sym(xvar4), fill = xvar4))
  }

  if (!is.null(xvar5)) {
    p <- p + geom_density(alpha = .2, aes(x = !!rlang::sym(xvar5), fill = xvar5))
  }

  p +
    scale_x_log10(limits = c(1, 100000),
                  breaks = c(1, 10, 100, 1000, 10000, 100000),
                  labels = c(1, 10, 100, "1k", "10k", "100k")) +
    labs(x = paste("α-opic EDI [lx]"),
         y = "Probability density") +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank(),  # Remove legend title
          plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
          panel.background = element_rect(fill = "white",
                                          size = 2,
                                          linetype = "solid"),
          aspect.ratio = 1/1)
}




# plotting function for  age vs. pupil range 
#ggpupilrange was used in producing Figure 8 C

ggpupilrange <- function(dataset) {
  ggplot(dataset,
         aes(x = age, y = Range_pupil
         ))+ 
    scale_x_continuous(breaks = seq(10,90,10), limits = c(15,90)
    )+
    scale_y_continuous(breaks = seq(1,9,2), limits = c(0,9))+
    geom_point(shape=25, size=2.25, alpha=0.6, colour="black", 
               aes(fill = `id` ))+
    geom_smooth(method = "lm", se=T, color="black", formula = my.formula,
                fullrange = T) +
    stat_poly_eq(formula = my.formula,
                 size = 2.81,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., 
                                   sep = "*plain(\",\")~")), 
                 parse = TRUE, label.x = 0.5, label.y = 0.018
    ) +
    #labs(#title = "A",
         #subtitle = "Pupil dilation range as a function of age",
         #x = "Age [years]",
         #y = "Pupil diameter range [mm]"
         #)+
    theme_classic()+
    theme(legend.position = "none", 
          plot.title = element_text(size = 11, 
                                    face = "bold"),
          plot.subtitle = element_text(color = "black"),
          panel.background = element_rect(fill = "gray95",
                                          size = 2, 
                                          linetype = "solid"),
          aspect.ratio = 1/1)
}


#plotting function for age effect slope vs. light intensity cluster
#ggageslope was used in producing Figure 8 A
ggageslope <- function(dataset) {
  
  ggplot(dataset,
         aes(x = light_intensity, y = slope))+
    geom_linerange(aes(ymin=slci_low , ymax=slci_up), #width=.2,
                   colour="black")+
    geom_point(shape=22, size=2, alpha=1, colour="black", fill = "steelblue")+
    scale_y_continuous(breaks = seq(0,-0.5,-0.1), limits = c(0,-0.55),
                       trans = "reverse")+
    scale_x_log10(limits=c(0.05,1000), 
                  breaks =c(0.1, 1,10, 100, 1000),
                  labels =c("dark", ">1 & \u226410",">10 & \u2264100",
                            ">100 & \u22641k",">1k")#,
                  #guide = guide_axis(n.dodge = 2)
                  )+
      theme_classic()+
    theme(legend.position = "none", 
          plot.title = element_text(size = 11, 
                                    face = "bold"),
          plot.subtitle = element_text(color = "black"),
          panel.background = element_rect(fill = "gray95",
                                          size = 2, 
                                          linetype = "solid"),
          aspect.ratio = 1/1,
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}  

#plotting function for age effect intercept vs. light intensity cluster
#ggageintercept was used in producing Figure 8 B

ggageintercept <- function(dataset) {
  ggplot(dataset,
         aes(x = light_intensity, y = intercept))+
    geom_linerange(aes(ymin=intci_low , ymax=intci_up), #width=.2,
                   colour="black")+
    geom_point(shape=21, size=1.5, alpha=1, colour="black", fill = "steelblue")+
    scale_y_continuous(breaks = seq(1,9,2), limits = c(0,9))+
    scale_x_log10(limits=c(0.05,1000), 
                  breaks =c(0.1, 1,10, 100, 1000),
                  #labels =c("Dark", ">1",">10",">100",">1k")
                  #labels =c("Dark", ">1 & \U2264 10",">10 & \U2264 100",">100 & \U2264 1k",">1k")
                  labels =c("dark", ">1 & \u226410",">10 & \u2264100",
                            ">100 & \u22641k",">1k")
                  )+
        #labs(x = "Melanopic EDI range [lx]",
        # y = "Pupil size intercept [mm]",
         #title = "C",
         #subtitle = "Pupillary age effect intercept per grouped light intensity")+
    theme_classic()+
    theme(legend.position = "none", 
          plot.title = element_text(size = 11, 
                                    face = "bold"),
          plot.subtitle = element_text(color = "black"),
          panel.background = element_rect(fill = "gray95",
                                          size = 2, 
                                          linetype = "solid"),
          aspect.ratio = 1/1,
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
} 


#plotting function for auto correlations (pupil size & mEDI)
#ggacf was used in producing Suppl. Figure 4 
ggacf <- function(dataset) {
  ggplot(dataset, mapping = aes(x = lag, y = cor))+
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes( xend = lag, yend = 0))+
    scale_x_continuous(breaks=seq(from=0, to=18, by=6),
                       labels=seq(0,3,1))+
    labs(x="Sample lag, t0 - [minute]", y = "Autocorrelation [r]")+
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
} 

#creating "my formula" var for plotting linear fits
my.formula <- y ~ x

#create a dodge position matrix
dodge <- position_dodge(0) # in this case dodge=0

#empty plot as filler for workarounds
fillerplot <- ggplot() + theme_void()

