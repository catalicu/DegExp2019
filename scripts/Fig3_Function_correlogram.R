#title: "Fig3_Function_correlogram.R"
#author: "Dr CG"
#date:"7/7/2025"

# Description:
# This script generates Figure 3 and performs the associated statistical tests. 
# Additionally, it generates individual plots for supplementary material:
# * individual BEF plots to explore each relationships, featured in Fig S3, and
# * individual narrow function-broad function relationships, featuerd in Fig4. 

# SETUP (same set up as in Fig2_BEF_estimates_narrow.R)
## Load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(car)

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

# PLOTS
## 1. Correlogram
### select function columns
fun.table.pre3=fun.table.pre2[,c(20:50, 61)] # all functions
fun.table.pre3b=fun.table.pre2[,c(29, 33,34,37,43, 61)] # select functions
fun.table.pre4=fun.table.pre3[-44,] #revove row with NAs
### calculate correlation matrix
Matrix.funct.pre=cor(fun.table.pre4)
### evaluate significance
testRes.pre = cor.mtest(fun.table.pre4, conf.level = 0.95)

corrplot(Matrix.funct.pre, method='ellipse', type='upper',
         col=brewer.pal(n=8, name='RdYlBu'), 
         tl.cex=0.5, tl.col='black', order='hclust',
         p.mat=testRes.pre$p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', 
         diag=FALSE)

## 2. Individual BEF plots
### set up the line to match the figure 
p.pre1=ggplot(fun.table.pre2, aes(richness, two_hydroxy_benzoic_Acid)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + geom_smooth(method='lm', se=FALSE, color='black') +
  scale_fill_manual(values=c('black', 'grey', 'white'))
p.pre2=ggplot(fun.table.pre2, aes(richness, Tween80)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + geom_smooth(method='lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'grey', 'white'))
p.pre3=ggplot(fun.table.pre2, aes(richness, Phenylethyl_amine)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + #geom_smooth(method='lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'grey', 'white'))
p.pre4=ggplot(fun.table.pre2, aes(richness, Glycyl_L_glutamic_Acid)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + #geom_smooth(method='lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'grey', 'white'))
p.pre5=ggplot(fun.table.pre2, aes(richness, D_L_alpha_glycerol_phospate)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + #geom_smooth(method='lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'grey', 'white'))
p.pre6=ggplot(fun.table.pre2, aes(richness, I_Erytritol)) + 
  geom_jitter(color='black',shape=21, size=3, aes(fill=factor(series))) + 
  Theme2 + #geom_smooth(method='lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'grey', 'white'))
prefunct.plots=arrangeGrob(p.pre1,p.pre2,p.pre3,p.pre4,p.pre5,p.pre6,ncol=2)
plot(prefunct.plots)

## 3. Narrow function - broad function relationships
p.prevs1=ggplot(fun.table.pre2, aes(x=Tween80, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2+ geom_smooth(method='lm', se=FALSE, col='black') +
  labs(y='Weight Change (mg)')
p.prevs2=ggplot(fun.table.pre2, aes(x=D_cellobiose, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2+ geom_smooth(method='lm', se=FALSE, col='black')+
  labs(y='Weight Change (mg)')
p.prevs3=ggplot(fun.table.pre2, aes(x=N_acetyl_D_glucosamine, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2+ geom_smooth(method='lm', se=FALSE, col='black')+
  labs(y='Weight Change (mg)')
p.prevs4=ggplot(fun.table.pre2, aes(x=D_galactonic_acid_gama_lactone, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2+ geom_smooth(method='lm', se=FALSE, col='black')+
  labs(y='Weight Change (mg)')

p.prevs5=ggplot(fun.table.pre2, aes(x=Pyruvic_acid_methyl_esther, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2 + #+ geom_smooth(method='lm', se=FALSE)+
  labs(y='Weight Change (mg)')
p.prevs6=ggplot(fun.table.pre2, aes(x=L_serine, y=W_Change_mg, color=richness)) + 
  geom_point() + Theme2+ geom_smooth(method='lm', se=FALSE, col='black')+
  labs(y='Weight Change (mg)')
p.prevs.plots=arrangeGrob(p.prevs1,p.prevs2,p.prevs3, p.prevs4,p.prevs5,p.prevs6, ncol=2)
plot(p.prevs.plots)

# STATISTICS
# this model tests the significance of narrow function - broad function relationships
names.functvs.pre=model.results.funct.pre$Function_name
model.results.functvs.pre=c()

for (i in 1:length(names.functvs.pre)) {
  funct1=names.functvs.pre[i]
  # linear generalized models
  mod.glm.vs=glm(W_Change_mg~get(funct1), data=fun.table.pre2)
  # evaluate models with summary
  mod.vs.summary=summary(mod.glm.vs)
  mod.vs.Anova=Anova(mod.glm.vs)
  mod.select.table=c(functionN=(names.functvs.pre[i]),
                     mod.vs.summary$coefficients[1,],
                     mod.vs.summary$coefficients[2,],
                     Chisq=mod.vs.Anova[1],
                     df=mod.vs.Anova[2],
                     pval=mod.vs.Anova[3])
  
  model.results.functvs.pre=rbind(mod.select.table, model.results.functvs.pre)
}
model.results.functvs.pre
rownames(model.results.functvs.pre)=model.results.functvs.pre[,1]
model.results.functvs.pre=data.frame(model.results.functvs.pre)
colnames(model.results.functvs.pre)=c('Function_name', 'Intercept_estimate', 'Intercept_stde', 'Intercept_tval', 'Intercept_pval', 'slope_estimate', 'slope_stde', 'slope_tval', 'slope_pval', 'Chisq', 'df', 'pval')
model.results.functvs.pre$adj.p=p.adjust(model.results.functvs.pre$pval, method='BH')
model.results.functvs.pre = apply(model.results.functvs.pre, 2, as.character)
# save the outputs
model.results.functvs.path = paste(OUTPATH, '/glmm_functvs', date,'.txt', sep='')
write.table(model.results.functvs,  model.results.functvs.path, sep="\t")
