#title: "Fig5_NMDS.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure 5 and performs the associated statistical tests.

# SETUP
## Load libraries
library(dplyr)
library(ggplot2)
library(vegan)

## set saving parameters
OUTPATH=paste(getwd(), "/output_tables", sep='')
date=Sys.Date()

## Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

## Load tables
ASVtable=read.table('output_tables_fromPhyloseq/ASVtable_DegExp2024-09-26.txt', header=TRUE)
TAXtable=read.table('output_tables_fromPhyloseq/TAXtable_DegExp2024-09-26.txt', header=TRUE)
meta.table=read.table('output_tables_fromPhyloseq/METAtable_DegExp2024-09-26.txt', header=TRUE)
#diversity
meta.div=read.table('output_files/MetaDiv_table_DegExp2024-09-26.txt', header=TRUE)
#function and diversity
fundiv=read.table('output_files/fundiv.txt', header=TRUE)
# Ecoplate data
Plate.table=read.table('input_data/Plate.table.full_Dec23.txt', header=TRUE)

## Edit tables
meta.table[which(!(meta.table$Sampleid%in%fundiv$Sampleid)),1]
meta.table2=fundiv# [-which(fundiv$Sampleid=='sample313__'),]
ASVtable2=ASVtable[-which(meta.table$Sampleid=='sample313__'),]
meta.div3=meta.table2[-which(meta.table2$dataset=='post'),]
ASVtable3=ASVtable2[-which(meta.table2$dataset=='post'),]

## finish transfering. from DegExp1_multivAnalysis