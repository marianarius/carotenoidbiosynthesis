#!/usr/bin/env bash

#SBATCH --job-name=sf.99.iqtree
#SBATCH --output=sf.99.iqtree.log
#SBATCH --ntasks-per-node=28
#SBATCH --nodes=1
#SBATCH --time=168:00:00
#SBATCH -p extended-28core

 module unload R/3.6.0
 module load anaconda/2
 source activate phylo


 #IQ-TREE
 iqtree -s OG5_131363_physy_sf_1.hmmal.cT.derep.V4.hmmal.cT.derep.V4.HMMAL.99.fas -safe -bb 1000 -alrt 1000 -m WAG+F+R10 -nt AUTO

 conda deactivate
 module unload anaconda/2

