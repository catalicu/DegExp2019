#DegExp v1: Diversity analyses: effects of dilution
# author: ccg
# date: 9/27/2024

library(plyr)

setwd("/Users/cc349/Library/CloudStorage/GoogleDrive-cc349@humboldt.edu/My Drive/CGLab_private drive/Projects/Degradation Functional dynamics_comp/DegExp1_dilutions1/DegExp1_analysis_full_clean_dec2023")
fundiv=read.table('output_files/fundiv.txt', header=TRUE)
head(fundiv)

Div.summary.dataset=ddply(fundiv, .(dataset), summarize, meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness), minrich=min(richness), maxrich=max(richness), mineve=min(evenness), maxeve=max(evenness))
#write.table(Div.summary.dataset, 'diversity_summary_dataset.txt', sep='\t')

Div.summary.dilution=ddply(fundiv, .(dilution_tube), summarize, meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
Div.summary.dilution
#write.table(Div.summary.dilution, 'diversity_summary_dilution.txt', sep='\t')

Div.summary.full=ddply(fundiv, .(dilution_tube, series, dataset), summarize, meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
#write.table(Div.summary.dataset, 'diversity_summary_full.txt', sep='\t')
