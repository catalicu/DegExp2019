#title: "Fig2_BEF_estimates_narrow.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure 2 and performs the associated statistical tests.

## Load libraries
library(dplyr)
library(ggplot2)

## set saving parameters
OUTPATH=paste(getwd(), "/output_tables", sep='')
date=Sys.Date()

## Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

## Load tables
meta.div=read.table('output_tables/MetaDiv_table_DegExp2025-04-01.txt', header=TRUE)
weight.table=read.table('input_data/DegExp_WeightChange_data_raw.txt', header=TRUE)
Plate.table=read.table('input_data/Plate.table.full.txt', header=TRUE)

## Edit tables
### sampleid column to identify samples uniquely
weight.table$Sampleid=weight.table$Tube_name
### dilution as continuous (1:5) using the column meta.table$dilution
weight.table$dilution1=factor(weight.table$dilution, levels=c('5', '4', '3', '2', '1')) # as factor
### create a Description column that corresponds to the div.table and meta.table
weight.table$Description=paste(weight.table$dilution, weight.table$Series, weight.table$Rep, sep='_')
### dilution factors for each tube
weight.table$dilution.factor=weight.table$dilution # as continuous but calculating the dilution factor:
weight.table[which(weight.table$dilution.factor==1),13]=10^-4
weight.table[which(weight.table$dilution.factor==2),13]=10^-3
weight.table[which(weight.table$dilution.factor==3),13]=10^-2
weight.table[which(weight.table$dilution.factor==4),13]=10^-1
weight.table[which(weight.table$dilution.factor==5),13]=1
### convert g to mg
weight.table$W_Change_mg=1000*weight.table$W_Change
### Edit Ecoplate tables
Plate.table$Sampleid=Plate.table$Sample
Plate.table$Series=as.character(Plate.table$Series)
Plate.table[(Plate.table<=0.00001)]=0

## Merge datasets
pre=meta.div[which(meta.div$dataset=='pre'),]

## Separate dataset
fun.table.pre=left_join(pre, Plate.table, by=c('Sampleid'))
fun.table.pre2=inner_join(fun.table.pre, weight.table[which(weight.table$dataset=='pre'),], by=c('Sampleid'))

# First model
## set up
names.funct.pre=colnames(fun.table.pre2)[20:50]
names.funct.pre=sort(names.funct.pre)
data=fun.table.pre2
model.results.funct.pre=c()
## loop
for (i in 1:length(names.funct.pre)) {
  funct1=names.funct.pre[i]
  # linear generalized models
  mod.lm.null=lm(get(funct1)~1, data=data)
  mod.lm.rich=lm(get(funct1)~richness, data=data) 
  # mixed: none were significant, removed 
  # evaluate models with AIC
  aic.models=AIC(mod.lm.null, mod.lm.rich) #, mod.glmm.ser, mod.glmm.rich.ser, mod.glmm.null)
  mod.select=(aic.models[which(aic.models$AIC==min(aic.models$AIC)),])
  mod.select.table=c(mod.select=rownames(mod.select), 
                     df=mod.select$df, 
                     aic=mod.select$AIC,
                     R2=(summary(get(rownames(mod.select))))$adj.r.squared, 
                     Function_name=funct1)
  mod.results=summary(mod.lm.rich)
  mod.results.coef=mod.results$coefficients[2,]
  mod.select.table2=c(mod.select.table, mod.results.coef)
  model.results.funct.pre=rbind(mod.select.table2, model.results.funct.pre)
}
## cleaning table
rownames(model.results.funct.pre)=model.results.funct.pre[,5]
model.results.funct.pre=data.frame(model.results.funct.pre)
colnames(model.results.funct.pre)[9]='p.value'
model.results.funct.pre$p.adj=p.adjust(model.results.funct.pre$p.value, method='BH')
## select and identify significant relationships
model.results.funct.pre$significant=model.results.funct.pre$p.adj
model.results.funct.pre[which(model.results.funct.pre$p.adj<=0.05),
                        which(colnames(model.results.funct.pre)=='significant')]='yes'
model.results.funct.pre[which(model.results.funct.pre$p.adj>=0.051),
                        which(colnames(model.results.funct.pre)=='significant')]='no'
model.results.funct.pre$slope=model.results.funct.pre$Estimate
model.results.funct.pre[which(model.results.funct.pre$slope>=0),
                        which(colnames(model.results.funct.pre)=='slope')]='positive'
model.results.funct.pre[which(model.results.funct.pre$slope<=0),
                        which(colnames(model.results.funct.pre)=='slope')]='negative'
## edit the table
model.results.funct.pre$Estimate=as.numeric(model.results.funct.pre$Estimate)
model.results.funct.pre$p.adj=as.numeric(model.results.funct.pre$p.adj)
model.results.funct.pre$R2=as.numeric(model.results.funct.pre$R2)
## save the table
model.results.functpre.path = paste(OUTPATH, '/glmm_funct_rich', date,'.txt', sep='')
write.table(model.results.funct.pre,  model.results.functpre.path , sep="\t")

# PLOT
model.results.funct.pre$standarderr=as.numeric(model.results.funct.pre$Std..Error)
plot.estimatepre=ggplot(data=model.results.funct.pre, 
                        aes(x=Function_name, y=Estimate)) + 
  geom_point(shape=21, aes(fill=significant)) + 
  geom_errorbar(data=model.results.funct.pre,
                aes(ymin=(Estimate-standarderr), ymax=(Estimate+standarderr))) +
  geom_hline(yintercept = 0, color = 'red', linewidth =1) +
  Theme2 + ylim(-0.002, 0.005) +
  labs(y='Function') +
  coord_flip()
plot(plot.estimatepre)
