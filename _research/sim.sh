#!/bin/sh
#$ -l mem=7G,time=3:00:00
cd lcm
Rscript=/nfs/apps/R/4.0.3/bin/Rscript
export R_LIBS_USER=/ifs/home/msph/epi/ntw2117/R_4.0
${Rscript} _research/sim.R $1 $2
