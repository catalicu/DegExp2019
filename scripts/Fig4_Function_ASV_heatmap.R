#title: "Fig4_Function_ASV_heatmap.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure 4 and performs the associated statistical tests.
# Figure 4 is a heatmap depicting the relationships between each bacterial family and each narrow function.

# SETUP
## Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(dplyr)
library(plyr)

## set saving parameters
OUTPATH=paste(getwd(), "/output_tables", sep='')
date=Sys.Date()

## Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

## Load tables
#XXXX from taxa4_run ## MAKE IT
select.model.results.all=read.table('output_tables/ASVfundiv.select.all2025-04-15.txt', header=TRUE)

## Correct function names
select.model.results.all$funct_name=as.character(select.model.results.all$funct_name)
select.model.results.all[which(select.model.results.all$funct_name=='alpha_keto_butyric_acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='a-ketobutyric acid'
select.model.results.all[which(select.model.results.all$funct_name=='alpha_cyclodextrin'),
                         which(colnames(select.model.results.all)=='funct_name')]='alpha-cyclodextrin (a-CD)'
select.model.results.all[which(select.model.results.all$funct_name=='Beta_methyl_D_glucoside'),
                         which(colnames(select.model.results.all)=='funct_name')]='Beta-methyl-D-glucoside'
select.model.results.all[which(select.model.results.all$funct_name=='I_Erytritol'),
                         which(colnames(select.model.results.all)=='funct_name')]='Erythritol'
select.model.results.all[which(select.model.results.all$funct_name=='four_hydroxy_benzoic_Acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='4-hydroxybenzoic acid'
select.model.results.all[which(select.model.results.all$funct_name=='D_galactouronic_acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='D-galactouronic acid (GalA)'
select.model.results.all[which(select.model.results.all$funct_name=='D_galactonic_acid_gama_lactone'),
                         which(colnames(select.model.results.all)=='funct_name')]='D-galactonic acid gamma-lactone'
select.model.results.all[which(select.model.results.all$funct_name=='D_L_alpha_glycerol_phosphate'),
                         which(colnames(select.model.results.all)=='funct_name')]='DL-a-glycerol phosphate'
select.model.results.all[which(select.model.results.all$funct_name=='D_malic_Acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='D-malic acid'
select.model.results.all[which(select.model.results.all$funct_name=='Gamma_hydroxybutyric_Acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='Gamma-hydroxybutyric acid (GHB)'
select.model.results.all[which(select.model.results.all$funct_name=='Glucose_1_phosphate'),
                         which(colnames(select.model.results.all)=='funct_name')]='Glucose-1-phosphate (G-1-P)'
select.model.results.all[which(select.model.results.all$funct_name=='Itaconic_Acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='Itaconic acid'
select.model.results.all[which(select.model.results.all$funct_name=='L_Arginine'),
                         which(colnames(select.model.results.all)=='funct_name')]='L-Arginine'
select.model.results.all[which(select.model.results.all$funct_name=='L_threonine'),
                         which(colnames(select.model.results.all)=='funct_name')]='L-threonine'
select.model.results.all[which(select.model.results.all$funct_name=='N_acetyl_D_glucosamine'),
                         which(colnames(select.model.results.all)=='funct_name')]='N-acetyl-D-glucosamine'
select.model.results.all[which(select.model.results.all$funct_name=='two_hydroxy_benzoic_Acid'),
                         which(colnames(select.model.results.all)=='funct_name')]='2-hydroxybenzoic acid'
select.model.results.all[which(select.model.results.all$funct_name=='W_Change'),
                         which(colnames(select.model.results.all)=='funct_name')]='Weight loss'
select.model.results.all[which(select.model.results.all$funct_name=='D_xylose'),
                         which(colnames(select.model.results.all)=='funct_name')]='D-xylose'

## Order 
select.model.results.all$funct_name=factor(select.model.results.all$funct_name, 
                                           levels=c('Weight loss', '2-hydroxybenzoic acid',
                                                    'Itaconic acid', 
                                                    'alpha-cyclodextrin (a-CD)',
                                                    'Beta-methyl-D-glucoside',
                                                    'D-galactouronic acid (GalA)',
                                                    'D-malic acid',
                                                    'D-galactonic acid gamma-lactone',    
                                                    'Tween80',
                                                    'Gamma-hydroxybutyric acid (GHB)', 
                                                    'Erythritol',
                                                    'a-ketobutyric acid',
                                                    '4-hydroxybenzoic acid',
                                                    'Glucose-1-phosphate (G-1-P)',
                                                    'N-acetyl-D-glucosamine',
                                                    'D-xylose',
                                                    'Glycogen',
                                                    'D_L_alpha_glycerol_phospate',
                                                    'L-Arginine',
                                                    'L-threonine'))

# PLOT
select.model.results.family = ddply(select.model.results.all, .(funct_name, Family), 
                                    summarise,  meanR2=mean(as.numeric(R2)), 
                                    meanSlope=mean(Slope_Estimate))
full_grid <- expand_grid(
  funct_name = unique(select.model.results.family$funct_name),
  Family = unique(select.model.results.family$Family))

complete_data <- full_grid %>%
  left_join(select.model.results.family, by = c("funct_name", "Family"))

fig_funct_ASVfam_labels=ggplot(data=complete_data, aes(x=funct_name, y=Family))+ 
  geom_tile(aes(fill=meanSlope), color='black') + 
  Theme + 
  scale_fill_gradient2(name='Slope \n estimate \n coefficient', low='blue', mid='yellow', high='red', na.value='white') +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), 
        axis.text.y=element_text(angle=0, vjust=0.5, hjust=0.95),
        legend.key.size=unit(10,'pt'), 
        legend.text=element_text(size=7),
        legend.title.align=0.5) + 
  xlab('Function name') +
  geom_vline(aes(xintercept=1.5), color='red', linetype='dashed') +
  geom_text(aes(label=round(as.numeric(meanR2), digits=3)), size=3) 

plot(fig_funct_ASVfam_labels)  
