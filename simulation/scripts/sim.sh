#!/bin/sh
#$ -l mem=4G,time=:20:
cd lcm
Rscript=/nfs/apps/R/4.0.3/bin/Rscript
export R_LIBS_USER=/ifs/home/msph/epi/ntw2117/R_4.0
${Rscript} simulation/scripts/sim.R $1
