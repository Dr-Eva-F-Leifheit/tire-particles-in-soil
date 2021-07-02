data2<-read.csv("/Users/eva/box.fu/Studis/Hanna Kissener/Excel-Dateien/datatable_forR.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
summary(data2)
library(rlang)
##########################################################################
#      Supplementary Material: Leifheit et al. (2021)
#      "Tire abrasion particles negatively affect plant growth
#        even at low concentrations and alter soil biogeochemical cycling"
#
#        Author of this script:  Masahiro Ryo (last update: 29.10.2020)
##########################################################################

setwd("... specify your own local directory where it contains the csv dataset ...")
setwd("C:/Users/Eva/box.fu/Studis/Hanna Kissener/Excel-Dateien/")

# reading libraries
library(mgcv)
library(tidyverse)
library(itsadug)
library(patchwork)
theme_set(theme_bw()) # set ggplot theme globally


# reading & transforming the dataset
df <- read.csv("datatable_forR.csv", header = TRUE, sep = ",", dec = ".") %>% 
  pivot_longer(cols= !!colnames(.)[3:8], names_to = "response", values_to = "value")
df$response <- as.factor(df$response)
response.names <- unique(df$response) %>% 
  factor(., levels = c("shootweight","rootweight","litterweightloss","pH","zinc", "CO2")) %>% 
  levels()


# model fit ----------------------
gam.summary <- list()
ggplots <- list()
color_style <- c("forestgreen","forestgreen","orange4","orange4","orange4","orange4")
response.label <- list("shoot biomass weight (mg)", "root biomass weight (mg)", "litter weight loss (%)", "soil pH", c(as.expression(bquote('bioavailable zinc (mg' ~kg^-1*'soil)'))),
                                              c(as.expression(bquote('soil respiration (µg' ~CO[2]~ g^-1~h^-1*')'))))

for(i_response in response.names){
  i <- which(i_response == response.names)
  gam.fit <- gam(data=df %>% filter(response==i_response), 
                 formula= value ~ s(concentration_tp), method = "REML")
  
  # obtaining the fitted curve+se values
  fit.term <- get_modelterm(gam.fit,as.data.frame = T, select=1) %>%  
    mutate(fit=fit+gam.fit$coefficients[1],
           upr=fit+se.fit, lwr=fit-se.fit)
  # result & data viz -----------------------
  gam.summary[[i_response]] <- summary(gam.fit)
  
  ggplots[[i_response]] <-
    ggplot(data=fit.term, aes(x=concentration_tp, y=fit)) +
    geom_line()+
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, fill=color_style[i]) +
    geom_point(data=df %>% filter(response==i_response), 
               aes(x= concentration_tp, y= value, alpha=0.8),na.rm=TRUE) +
    theme(legend.position = 'none', text = element_text(size=9, color="grey20")) +
    xlab(expression("tire particle addition (mg g"^-1*" soil)")) +
    ylab(response.label[[i]]) 
}
gam.summary

# plot all using patchwork
ggplots[["shootweight"]] + ggplots[["rootweight"]] + ggplots[["litterweightloss"]]+
  ggplots[["pH"]] + ggplots[["zinc"]] + ggplots[["CO2"]]+ # second column
  plot_layout(ncol=3, byrow=T) +  
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 10, color="black"))

ggsave("figure_zinc_rearranged.tiff", units="mm", dpi=600, width=200, height=120)
