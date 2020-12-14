### This script attempts to recreate the ggseg object without holes
###
### Ellyn Butler
### December 7, 2020

library(ggseg) # v 1.6.00
library(ggsegExtra) # v 1.5.31
library(ggseg3d) # 1.6.02
library(tidyverse) # v 1.3.0

# convert miccai to fsaverage5
mri_surf2surf_rereg(subject = "fsaverage", annot = "mic", hemi = "lh")
mri_surf2surf_rereg(subject = "fsaverage", annot = "mic", hemi = "rh")

# Make 3d miccai atlas
mic_3d <- make_aparc_2_3datlas(annot = "mic",
  output_dir = "~/Documents/pncBrainSpatialCog/data/")
