### This script attempts to recreate the ggseg object without holes
###
### Ellyn Butler
### December 7, 2020 - December 14, 2020

library(ggseg) # v 1.6.00
library(ggsegExtra) # v 1.5.31
library(ggseg3d) # 1.6.02
library(tidyverse) # v 1.3.0

# convert miccai to fsaverage5
mri_surf2surf_rereg(subject = 'fsaverage', annot = 'mic', hemi = 'lh',
  target_subject='fsaverage5', output_dir='/Applications/freesurfer/7.1.1/subjects/fsaverage5/label')
mri_surf2surf_rereg(subject = 'fsaverage', annot = 'mic', hemi = 'rh',
  target_subject='fsaverage5', output_dir='/Applications/freesurfer/7.1.1/subjects/fsaverage5/label')

# Make 3d miccai atlas
mic_3d <- make_aparc_2_3datlas(annot = 'mic',
  output_dir = '~/Documents/pncBrainSpatialCog/ggseg_atlas')

# View atlas
#ggseg3d(atlas = mic_3d)

# Look at atlas data contents
#unnest(mic_3d, ggseg_3d)

# Inspect unique values of "region"
#unnest(mic_3d, ggseg_3d) %>%
#  select(region) %>%
#  unique() %>%
#  unlist()

# Edit out "
mic_3d <- unnest(mic_3d, ggseg_3d) %>%
    mutate(region = gsub("\"", "", region), label = gsub("\"", "", label),
      annot = gsub("\"", "", annot), atlas = "mic_3d") %>%
  nest_by(atlas, surf, hemi, .key = "ggseg_3d") %>%
  as_ggseg3d_atlas()

# View atlas (still has some holes, and some interweaving)
#ggseg3d(atlas = mic_3d, hemisphere = "left", surface = "inflated")
#ggseg3d(atlas = mic_3d, hemisphere = "right", surface = "inflated")

#ggseg3d(atlas = mic_3d, hemisphere = "left", surface = "LCBC")
#ggseg3d(atlas = mic_3d, hemisphere = "right", surface = "LCBC")

#ggseg3d(atlas = mic_3d, hemisphere = "left", surface = "white")
#ggseg3d(atlas = mic_3d, hemisphere = "right", surface = "white")


################################### 2D Atlas ###################################

mic <- make_ggseg3d_2_ggseg(mic_3d, steps = 1:7, smoothness = 2, tolerance = .5,
  output_dir = '~/Documents/pncBrainSpatialCog/ggseg_atlas')
