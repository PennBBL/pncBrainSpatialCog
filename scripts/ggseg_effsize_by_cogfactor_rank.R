### This script creates a figure of the quantile rank of effect sizes by cognitive
### factor score on the brain
###
### Ellyn Butler
### November 3, 2020 - January 29, 2021

library('ggseg') # v 1.1.5 >>> 1.6.00
library('ggplot2') # v 3.3.2 >>> 3.3.2
#library('dplyr') # v 1.0.2 >>> 1.0.2
library('ggpubr') # v 0.4.0 >>> 0.4.0
library('tidyverse') # v 1.3.0 >>> 1.3.0
library('R.utils') # v 2.10.1 >>> 2.10.1
library('scales') # v 1.1.1 >>> 1.1.1



modalities <- c('CBF', 'GMD', 'NBack', 'ReHo', 'Volume')
substrings <- c('vol_', 'as\\.matrix\\(srcbf\\)pcasl_jlf_cbf_', '_assr_SelfReg',
  'as\\.matrix\\(srgmd\\)mprage_jlf_gmd_', 'as\\.matrix\\(srreho\\)rest_jlf_reho_',
  'as\\.matrix\\(srNBACK\\)sigchange_contrast4_2back0back_mean_miccai_ave_', '1', '2')

for (modal in modalities) {
  load('~/Documents/ggsegExtra/data/micCort.rda')
  eff_df <- read.csv(paste0('~/Documents/pncBrainSpatialCog/results/loadings/', modal, '_model_output_HiLo2.csv'))

  if ('X' %in% names(eff_df)) { names(eff_df)[names(eff_df) == 'X'] <- 'Region' }
  if ('score' %in% names(eff_df)) { names(eff_df)[names(eff_df) == 'score'] <- 'Score' }
  eff_df <- eff_df[,c('Region', 'StdEffectRescale', 'Score')]
  names(eff_df)[names(eff_df) == 'Region'] <- 'region'

  for (substr in substrings) {
    eff_df$region <- gsub(substr, '', eff_df$region)
  }
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

  compcog_df <- eff_df[eff_df$Score == 'F1_Exec_Comp_Cog_Accuracy', ]
  row.names(compcog_df) <- 1:nrow(compcog_df)
  soccog_df <- eff_df[eff_df$Score == 'F2_Social_Cog_Accuracy', ]
  row.names(soccog_df) <- 1:nrow(soccog_df)
  mem_df <- eff_df[eff_df$Score == 'F3_Memory_Accuracy', ]
  row.names(mem_df) <- 1:nrow(mem_df)

  compcog_df$StdEffectRescale <- rescale(compcog_df$StdEffectRescale)
  soccog_df$StdEffectRescale <- rescale(soccog_df$StdEffectRescale)
  mem_df$StdEffectRescale <- rescale(mem_df$StdEffectRescale)

  compcog_df <- compcog_df %>% mutate(quantile = ntile(StdEffectRescale, 5))
  soccog_df <- soccog_df %>% mutate(quantile = ntile(StdEffectRescale, 5))
  mem_df <- mem_df %>% mutate(quantile = ntile(StdEffectRescale, 5))

  compcog_df$quantile <- ordered(compcog_df$quantile, 1:5)
  soccog_df$quantile <- ordered(soccog_df$quantile, 1:5)
  mem_df$quantile <- ordered(mem_df$quantile, 1:5)


  labels <- micCort2[micCort2$hemi == "left" & micCort2$region %in% eff_df$region, ] %>%
    unnest(cols = ggseg) %>%
    group_by(region) %>%
    summarise(.lat =  mean(.lat), .long = mean(.long))

  bigstdeffectrescale_compcog <- compcog_df[compcog_df$StdEffectRescale > .5, 'region']
  bigstdeffectrescale_soccog <- soccog_df[soccog_df$StdEffectRescale > .5, 'region']
  bigstdeffectrescale_mem <- mem_df[mem_df$StdEffectRescale > .5, 'region']

  labels_compcog <- labels[labels$region %in% bigstdeffectrescale_compcog, ]
  labels_soccog <- labels[labels$region %in% bigstdeffectrescale_soccog, ]
  labels_mem <- labels[labels$region %in% bigstdeffectrescale_mem, ]



  p_compcog_cort <- ggseg(compcog_df[compcog_df$region %in% micCort2$region, ], atlas='micCort2',
      mapping=aes(fill=quantile), hemisphere='left', size=.1, colour='black') +
    #scale_fill_gradient(low = 'cornsilk', high = 'red4') +
    scale_fill_brewer(palette=8, na.translate=FALSE) +
    ggtitle(paste0(modal, ': Complex Cognition')) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank(), legend.position='none') +
    ggrepel::geom_label_repel(data = labels_compcog, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5))

  p_soccog_cort <- ggseg(soccog_df[soccog_df$region %in% micCort2$region, ], atlas='micCort2',
      mapping=aes(fill=quantile), hemisphere='left', size=.1, colour='black') +
    #scale_fill_gradient(low = 'cornsilk', high = 'red4') +
    scale_fill_brewer(palette=8, na.translate=FALSE) +
    ggtitle(paste0(modal, ': Social Cognition')) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank(), legend.position='none') +
    ggrepel::geom_label_repel(data = labels_soccog, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5))

  p_mem_cort <- ggseg(mem_df[mem_df$region %in% micCort2$region, ], atlas='micCort2',
      mapping=aes(fill=quantile), hemisphere='left', size=.1, colour='black') +
    #scale_fill_gradient(low = 'cornsilk', high = 'red4') +
    scale_fill_brewer(palette=8, na.translate=FALSE) +
    ggtitle(paste0(modal, ': Memory')) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank(), legend.position='none') +
    ggrepel::geom_label_repel(data = labels_mem, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5))


  #############################################################################

  ##### Subcortical
  aseg <- as_ggseg_atlas(aseg)
  aseg_data <- aseg
  aseg_data <- as_ggseg_atlas(aseg_data[aseg_data$region %in% subcortical,])

  labels <- aseg_data[aseg_data$hemi == "right" & aseg_data$region %in% subcortical, ] %>%
    unnest(cols = ggseg) %>%
    group_by(region) %>%
    summarise(.lat =  mean(.lat), .long = mean(.long))

  labels_compcog <- labels[labels$region %in% bigstdeffectrescale_compcog, ]
  labels_soccog <- labels[labels$region %in% bigstdeffectrescale_soccog, ]
  labels_mem <- labels[labels$region %in% bigstdeffectrescale_mem, ]

  compcog_subcort_df <- compcog_df[compcog_df$region %in% subcortical, ]
  soccog_subcort_df <- soccog_df[soccog_df$region %in% subcortical, ]
  mem_subcort_df <- mem_df[mem_df$region %in% subcortical, ]

  #compcog_subcort_df$quantile <- ordered(compcog_subcort_df$quantile, 1:5)
  #soccog_subcort_df$quantile <- ordered(soccog_subcort_df$quantile, 1:5)
  #mem_subcort_df$quantile <- ordered(mem_subcort_df$quantile, 1:5)
  #levels(compcog_subcort_df$quantile) <- 1:5 # Doesn't translate to plot
  #levels(soccog_subcort_df$quantile) <- 1:5
  #levels(mem_subcort_df$quantile) <- 1:5

  p_compcog <- ggseg(compcog_subcort_df, atlas='aseg',
      hemisphere=c('left', 'right'), mapping=aes(fill=quantile), size=.1, colour='black') +
    scale_fill_brewer(palette=8, na.translate=FALSE, guide=guide_legend(reverse=TRUE), limits=1:5) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank()) +
    ggrepel::geom_label_repel(data = labels_compcog, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5)) #+
    #geom_sf_label(aes(label = subcortical),
    #  alpha = .8, show.legend = FALSE, fill = alpha(c("white"),0.5))

  p_soccog <- ggseg(soccog_subcort_df, atlas='aseg',
      hemisphere=c('left', 'right'), mapping=aes(fill=quantile), size=.1, colour='black') +
    scale_fill_brewer(palette=8, na.translate=FALSE, guide=guide_legend(reverse=TRUE), limits=1:5) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank()) +
    ggrepel::geom_label_repel(data = labels_soccog, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5))

  p_mem <- ggseg(mem_subcort_df, atlas='aseg',
      hemisphere=c('left', 'right'), mapping=aes(fill=quantile), size=.1, colour='black') +
    scale_fill_brewer(palette=8, na.translate=FALSE, guide=guide_legend(reverse=TRUE), limits=1:5) +
    theme(text=element_text(size=14), axis.title.x=element_blank(),
      axis.text.x=element_blank()) +
    ggrepel::geom_label_repel(data = labels_mem, inherit.aes = FALSE, size=3,
      mapping = aes(x = .long, y=.lat, label=region), label.size = NA,
      label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.5))

  p <- ggarrange(ggarrange(p_compcog_cort, p_compcog, nrow=2), ggarrange(p_mem_cort,
    p_mem, nrow=2), ggarrange(p_soccog_cort, p_soccog, nrow=2), ncol=3)

  pdf(file=paste0('~/Documents/pncBrainSpatialCog/plots/', modal, '_cogFactorsBrainReScale_quantile.pdf'), width=17.7, height=5.512)
  print(p)
  dev.off()
}












# QUESTIONS:
# 1.) How to deal with the fact that red4 should only be for std effect size 1,
# while those regions are part of cortical figure? Right now, whatever is highest
# in a given subcortical plot will get colored red4, but I want the color to be
# relative to all of the regions
# 2.) How to get NAs to not split off into another facet in the subcortical plot?


    #
