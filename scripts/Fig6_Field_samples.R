#title: "Fig6_Field_samples.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure 6 and performs the associated statistical tests.

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
## Field data
ASVtable.field=read.table('output_tables_fromPhyloseq/ASVtable_DegExp1_field2024-12-10.txt', header=TRUE)
TAXtable.field=read.table('output_tables_fromPhyloseq/TAXtable_DegExp1_field2024-12-10.txt', header=TRUE)
meta.table.field=read.table('output_tables_fromPhyloseq/METAtable_DegExp1_field2024-12-10.txt', header=TRUE)
## Experimental data
ASVtable=read.table('output_tables_fromPhyloseq/ASVtable_DegExp2025-04-01.txt', header=TRUE)
TAXtable=read.table('output_tables_fromPhyloseq/TAXtable_DegExp2025-04-01.txt', header=TRUE)
meta.table=read.table('output_tables_fromPhyloseq/METAtable_DegExp2025-04-01.txt', header=TRUE)



## finish transfering. from DegExp1_Div field. will need to recreate phyloseq files.