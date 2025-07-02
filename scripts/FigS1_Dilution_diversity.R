#title: "FigS1_BEF_Dilution diversity.R"
#author: "Dr CG"
#date:"7/3/2025"

# Description:
# This script generates Figure S1 and performs the associated statistical tests.

# Load libraries
library(ggplot2)
library(gridExtra)
library(lme4)
library(car)

# Set plotting parameters
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +
  theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# Load tables
meta.div=read.table('output_tables/MetaDiv_table_DegExp2025-04-01.txt', 
                    header=TRUE)

# Plot: richness and evenness changes with dilution treatments
rich.plot=ggplot(meta.div, aes(log(dilution_factor), richness)) + 
  geom_point(size=3, aes(shape=factor(series), fill=dataset))+ 
  Theme2 + scale_fill_manual(values=c( 'black', 'white')) + 
  scale_shape_manual(values=c(21,22, 24)) + 
  #geom_smooth(method='lm', se=FALSE, ) + 
  xlab('log(Dilution factor)') + ylab('ASV Richness') +
  scale_x_reverse() + annotate('text', label='a.', x=0, y=200)

even.plot=ggplot(meta.div, aes(log(dilution_factor), evenness)) + 
  geom_point(size=3,  aes(shape=factor(series), fill=dataset)) + 
  Theme2 + scale_fill_manual(values=c( 'black', 'white')) + 
  scale_shape_manual(values=c(21,22, 24)) + 
  xlab('log(Dilution factor)') + ylab('ASV Evenness') +
  scale_x_reverse() + annotate('text', label='b.', x=0, y=0.8)

uni.div=arrangeGrob(rich.plot, even.plot, ncol=2)  
#quartz()
plot(uni.div)

# Summary values
  # range of richness 
print(paste('richness range', 
            range(meta.div$richness)[1], '-', 
            range(meta.div$richness)[2], ', mean', mean(meta.div$richness), 
            'standard deviation', sd(meta.div$richness)))
print(paste('evenness range', range(meta.div$evenness)[1], '-', 
            range(meta.div$evenness)[2], ', mean', mean(meta.div$evenness), 
            'standard deviation', sd(meta.div$evenness)))

# Stats: models to test dilution treatment and series effect on richness and evenness

# RICHNESS
# null model (no relationship)
mod.null=lm(richness~1, data=meta.div)
# null model (only random)
mod.null.rand=glmer(richness~1 + (1|series), data=meta.div)
# linear model
mod.rich.lm=lm(richness~log(dilution_factor+1) + dataset, data=meta.div)
summary(mod.rich.lm)
# mixed model
mod.rich.lmei=lmer(richness~log(dilution_factor+1) + dataset + (1|series), data=meta.div)
summary(mod.rich.lmei)
AIC(mod.null, mod.null.rand,mod.rich.lm, mod.rich.lmei)
Anova(mod.rich.lmei)

# EVENNESS
# null model (no relationship)
mod.null.e=lm(evenness~1, data=meta.div)
# null model (only random)
mod.null.rand.e=glmer(evenness~1 + (1|series), data=meta.div)
# linear model
mod.rich.lm.e=lm(evenness~log(dilution_factor+1) + dataset, data=meta.div)
summary(mod.rich.lm)
# mixed model
mod.rich.lmei.e=glmer(evenness~log(dilution_factor+1) + dataset + (1|series), data=meta.div)
summary(mod.rich.lmei)
AIC(mod.null.e, mod.null.rand.e, mod.rich.lm.e, mod.rich.lmei.e)
Anova(mod.rich.lm.e)