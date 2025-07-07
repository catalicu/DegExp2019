#title: "FigS7.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure S7 and performs the associated statistical tests.

# Load libraries
library(ggplot2)
library(gridExtra)
library(car)
library(lme4)

# Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# Load tables
qpcr.data=read.csv('input_data/Degradation_16Sgene_qPCR_calculations.csv', header=TRUE)

# set up the metadata
qpcr.data$dilution=substr(qpcr.data$Sample, 1,1)
qpcr.data$series=substr(qpcr.data$Sample, 2,2)
qpcr.data$replicate=substr(qpcr.data$Sample, 3,3)
qpcr.data$dataset=substr(qpcr.data$Sample, 2,2)

# metadata edits
qpcr.data$dilution_factor=as.numeric(qpcr.data$dilution) # as continuous but calculating the dilution factor:
# dilution factors for each tube
qpcr.data[which(qpcr.data$dilution_factor==1),which(colnames(qpcr.data)=='dilution_factor')]=0.1
qpcr.data[which(qpcr.data$dilution_factor==2),which(colnames(qpcr.data)=='dilution_factor')]=0.01
qpcr.data[which(qpcr.data$dilution_factor==3),which(colnames(qpcr.data)=='dilution_factor')]=0.001
qpcr.data[which(qpcr.data$dilution_factor==4),which(colnames(qpcr.data)=='dilution_factor')]=0.0001
qpcr.data[which(qpcr.data$dilution_factor==5),which(colnames(qpcr.data)=='dilution_factor')]=0.00001
qpcr.data[which(qpcr.data$dilution_factor==0),which(colnames(qpcr.data)=='dilution_factor')]=1
qpcr.data$dilution_factor=as.numeric(qpcr.data$dilution_factor)
head(qpcr.data)

# separate datasets
qpcr.data[c(which(qpcr.data$dataset==1), which(qpcr.data$dataset==2), which(qpcr.data$dataset==3)),which(colnames(qpcr.data)=='dataset')]='Experiment'
qpcr.data$dataset=factor(qpcr.data$dataset, levels=c('Experiment', 'C', 'P'))

# remove controls for stats
qpcr.data.wc=qpcr.data[-which(qpcr.data$series=='C'),]
qpcr.data.wpc=qpcr.data.wc[-which(qpcr.data.wc$series=='P'),]

# STATISTICAL ANALYSES
# Comparing experiments to controls
mod.cntr=glm(Quantity ~ dataset, data=qpcr.data)
summary(mod.cntr)
Anova(mod.cntr)

# Effect of dilution on 16S rRNA copy number, account for series as random
mod1=lmer(Quantity ~ dilution_factor + (1|series), data=qpcr.data.wpc)
summary(mod1)
Anova(mod1)
mod2=glm(Quantity ~ dilution_factor*series, data=qpcr.data.wpc)
summary(mod2)
Anova(mod2)
# PLOTS
# Comparing experiments to controls
expvscontrol=ggplot(qpcr.data, aes(dataset, Quantity)) + 
  geom_boxplot(aes(fill=series)) +
  Theme2 + ylab('16S rRNA gene copy number') + 
  scale_fill_manual(values=c('yellow', 'brown', 'darkgreen', 'grey', 'white'))
# Effect of dilution and series
exp16S=ggplot(qpcr.data.wpc, aes(dilution, Quantity)) + 
  geom_boxplot(aes(fill=series)) +
  Theme2 + facet_grid(.~ series) + 
  scale_fill_manual(values=c('yellow', 'brown', 'darkgreen'))
# Merge the plots
Abund.plots=arrangeGrob(expvscontrol, exp16S +ylab(''), ncol=2)
plot(Abund.plots)
