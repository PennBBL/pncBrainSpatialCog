### This script creates (hopefully) the final ggseg figure
###
### Ellyn Butler
### November 3, 2020

library('ggseg') # v 1.1.5
library('ggplot2') # v 3.3.2
library('dplyr') # v 1.0.2
library('ggpubr') # v 0.4.0
library('tidyverse') # v 1.3.0
library('R.utils') # v 2.10.1
library('scales') # v 1.1.1

load('~/Documents/ggsegExtra/data/micCort.rda')
eff_df <- read.csv('~/Documents/pncBrainSpatialCog/results/loadings/volume_model_output_HiLo2.csv')
eff_df <- eff_df[,c('Region', 'StdEffectRescale', 'Score')]
names(eff_df)[names(eff_df) == 'Region'] <- 'region'

eff_df$region <- gsub('vol_', '', eff_df$region)
eff_df$region <- recode(eff_df$region, 'Hippocampus'='Hipp', 'Amygdala'='Amy',
  'Pallidum'='Pall', 'Putamen'='Put', 'Accumbens_Area'='NA', 'Caudate'='Cau',
  'Thalamus_Proper'='Thal')

eff_df$region <- as.character(eff_df$region)
eff_df$region <- recode(eff_df$region, 'PIns'='Pins')

micCort <- micCort %>%
   rename(region = area) %>%
   mutate(ggseg = map(ggseg, ~ mutate(.x, .subid = 1)))

base_atlas = micCort %>%
  select(region, hemi) %>%
  distinct() %>%
  na.omit()

micCort2 <- as_ggseg_atlas(micCort)

eff_df$region <- recode(eff_df$region, 'Hipp'='hippocampus',
  'Thalamus'='thalamus proper', 'Put'='putamen', 'Amy'='amygdala',
  'Pall'='pallidum', 'Cau'='caudate')

subcortical <- c('hippocampus', 'thalamus proper', 'putamen', 'amygdala',
  'pallidum', 'caudate')

eff_df <- eff_df[eff_df$region %in% c(micCort2$region, subcortical), ]

eff_df[eff_df$Score == 'F1_Exec_Comp_Cog_Accuracy', 'StdEffectRescale'] <- rescale(eff_df[eff_df$Score == 'F1_Exec_Comp_Cog_Accuracy', 'StdEffectRescale'])
eff_df[eff_df$Score == 'F2_Social_Cog_Accuracy', 'StdEffectRescale'] <- rescale(eff_df[eff_df$Score == 'F2_Social_Cog_Accuracy', 'StdEffectRescale'])
eff_df[eff_df$Score == 'F3_Memory_Accuracy', 'StdEffectRescale'] <- rescale(eff_df[eff_df$Score == 'F3_Memory_Accuracy', 'StdEffectRescale'])

eff_df$Score <- recode(eff_df$Score, 'F1_Exec_Comp_Cog_Accuracy'='Complex Cognition',
  'F2_Social_Cog_Accuracy'='Social Cognition', 'F3_Memory_Accuracy'='Memory')

eff_df2 <- eff_df[eff_df$region %in% micCort2$region,]

labels <- micCort2[micCort2$hemi == "left" & micCort2$region %in% eff_df2$region, ] %>%
  unnest(cols = ggseg) %>%
  group_by(region) %>%
  summarise(.lat =  mean(.lat), .long = mean(.long))

bigstdeffectrescale_compcog <- eff_df[eff_df$StdEffectRescale > .5 & eff_df$Score == 'Complex Cognition', 'region']
bigstdeffectrescale_soccog <- eff_df[eff_df$StdEffectRescale > .5 & eff_df$Score == 'Social Cognition', 'region']
bigstdeffectrescale_mem <- eff_df[eff_df$StdEffectRescale > .5 & eff_df$Score == 'Memory', 'region']

labels_compcog <- labels[labels$region %in% bigstdeffectrescale_compcog, ]
labels_soccog <- labels[labels$region %in% bigstdeffectrescale_soccog, ]
labels_mem <- labels[labels$region %in% bigstdeffectrescale_mem, ]

p_compcog_cort <- ggseg(eff_df2[eff_df2$Score == 'Complex Cognition', ], atlas='micCort2',
    mapping=aes(fill=StdEffectRescale), hemisphere='left', size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4') +
  ggtitle('Complex Cognition') +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank(), legend.position='none') +
  ggrepel::geom_label_repel(data = labels_compcog, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))

p_soccog_cort <- ggseg(eff_df2[eff_df2$Score == 'Social Cognition', ], atlas='micCort2',
    mapping=aes(fill=StdEffectRescale), hemisphere='left', size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4') +
  ggtitle('Social Cognition') +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank(), legend.position='none') +
  ggrepel::geom_label_repel(data = labels_soccog, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))

p_mem_cort <- ggseg(eff_df2[eff_df2$Score == 'Memory', ], atlas='micCort2',
    mapping=aes(fill=StdEffectRescale), hemisphere='left', size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4') +
  ggtitle('Memory') +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank(), legend.position='none') +
  ggrepel::geom_label_repel(data = labels_mem, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))


#############################################################################

##### Subcortical
aseg_data <- aseg
aseg_data <- aseg_data[aseg_data$region %in% subcortical,]

eff_df3 <- eff_df[eff_df$region %in% subcortical, ]

labels <- aseg[aseg$hemi == "right" & aseg$region %in% eff_df3$region, ] %>%
  unnest(cols = ggseg) %>%
  group_by(region) %>%
  summarise(.lat =  mean(.lat), .long = mean(.long))

labels_compcog <- labels[labels$region %in% bigstdeffectrescale_compcog, ]
labels_soccog <- labels[labels$region %in% bigstdeffectrescale_soccog, ]
labels_mem <- labels[labels$region %in% bigstdeffectrescale_mem, ]

p_compcog <- ggseg(eff_df3[eff_df3$Score == 'Complex Cognition', ], atlas='aseg',
    hemisphere=c('left', 'right'), mapping=aes(fill=StdEffectRescale), size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4', limits=c(0, 1)) +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank()) +
  ggrepel::geom_label_repel(data = labels_compcog, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))

p_soccog <- ggseg(eff_df3[eff_df3$Score == 'Social Cognition', ], atlas='aseg',
    hemisphere=c('left', 'right'), mapping=aes(fill=StdEffectRescale), size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4', limits=c(0, 1)) +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank()) +
  ggrepel::geom_label_repel(data = labels_soccog, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))

p_mem <- ggseg(eff_df3[eff_df3$Score == 'Memory', ], atlas='aseg',
    hemisphere=c('left', 'right'), mapping=aes(fill=StdEffectRescale), size=.1, colour='black') +
  scale_fill_gradient(low = 'cornsilk', high = 'red4', limits=c(0, 1)) +
  theme(text=element_text(size=14), axis.title.x=element_blank(),
    axis.text.x=element_blank()) +
  ggrepel::geom_label_repel(data = labels_mem, inherit.aes = FALSE, size=3,
    mapping = aes(x = .long, y=.lat, label=region))

p <- ggarrange(ggarrange(p_compcog_cort, p_compcog, nrow=2), ggarrange(p_mem_cort,
  p_mem, nrow=2), ggarrange(p_soccog_cort, p_soccog, nrow=2), ncol=3)

pdf(file='~/Documents/pncBrainSpatialCog/plots/cogFactorsBrainReScale_both.pdf', width=17.7, height=5.512)
p
dev.off()















# QUESTIONS:
# 1.) How to deal with the fact that red4 should only be for std effect size 1,
# while those regions are part of cortical figure? Right now, whatever is highest
# in a given subcortical plot will get colored red4, but I want the color to be
# relative to all of the regions
# 2.) How to get NAs to not split off into another facet in the subcortical plot?


    #
