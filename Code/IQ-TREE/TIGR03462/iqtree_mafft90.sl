#!/usr/bin/env bash

#SBATCH --job-name=iqtree_mafft90
#SBATCH --output=iqtree_mafft90.log
#SBATCH --ntasks-per-node=24
#SBATCH --nodes=1
#SBATCH --time=168:00:00
#SBATCH -p extended-24core

module load shared
module load anaconda/2

source activate phylo
iqtree -s lycyc_2x.HMMAL.all_1.cT.derep.V4.COR.dg.mafft.90.fas -safe -bb 1000 -alrt 1000 -m VT+F+R6 -nt AUTO
conda deactivate
module unload anaconda/2
