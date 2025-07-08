#title: "Fig2_BEF_estimates_narrow.R"
#author: "Dr CG"
#date:"7/3/2025"

# Description:
# This script generates Figure S1 and performs the associated statistical tests.

# Load libraries
library(ggplot2)
library(gridExtra)

# Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# Load tables
meta.div=read.table('output_tables/MetaDiv_table_DegExp2025-04-01.txt', 
                    header=TRUE)
weight.table=read.table('input_data/DegExp_WeightChange_data_edits.txt', 
                        header=TRUE) # edits: added the sample id values manually
  # further edit:
  weight.table$Sampleid=weight.table$Tube_name # make sure Sample ID match with meta div table:

# Merge the diversity table, the degradation weight change table and the ecoplate data tabl
fundiv=left_join(meta.div, weight.table)

####
# Statistical models
mod.null=lm(W_Change~1, data=fundiv)
mod.fx=lm(W_Change~log(richness), data=fundiv)
mod.mx2=lmer(W_Change ~ log(richness) + (1|Series),  data=fundiv)
mod.mx3=lmer(W_Change ~ log(richness) + (dilution|dataset),  data=fundiv)
  summary(mod.mx3)
  Anova(mod.mx3)

# compare models:
AIC(mod.fx, mod.mx2, mod.mx3)
```