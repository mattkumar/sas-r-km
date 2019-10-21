library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(patchwork)




#Read in data
surv <- read.csv("surv_data.csv", header=T)

#Clean up survival times
surv = surv %>% tidyr::fill(SURVIVAL) 

#Set theme
ggthemr('flat dark', type = 'outer')

#Base plot
km <- ggplot(surv, aes(group=Group, y=SURVIVAL,x=T,colour=Group)) +
  geom_step() +
  geom_point(show.legend=FALSE, aes(shape=as.factor(X_CENSOR_))) +
  scale_shape_manual("", values=c(32,3)) +
  scale_y_continuous(breaks=seq(0,1,0.2),    limits=c(0,1)   ) +
  scale_x_continuous(breaks=seq(0,2500,500), limits=c(0,2500)) +
  xlab("\nTime (Days)") +
  ylab("\nProbability of Survival") +
  theme(legend.position="top",legend.spacing.x = unit(.7, 'cm')) 

#At Risk plot
atrisk <- ggplot(subset(surv, !is.na(NumberAtRisk)), aes(color=Group)) +
  geom_text(aes(label=NumberAtRisk, x=Timelist,y=Group), show.legend=FALSE) +
  scale_x_continuous(breaks=seq(0,2500,500), limits=c(0,2500)) +
  ylab("") +
  xlab("\nNumber at Risk\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Add them together
km2 <- km + atrisk + plot_layout(ncol=1, heights=c(1,0.20)) 
km2

#Save as high-res image
ggsave("stand_alone_km.pdf", width=12, height=6)
