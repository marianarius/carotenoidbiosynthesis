#!/usr/bin/env bash

#SBATCH --job-name=phydes_mafft.90
#SBATCH --output=phydes_mafft.90.log
#SBATCH --ntasks-per-node=28
#SBATCH --nodes=1
#SBATCH --time=168:00:00
#SBATCH -p extended-28core

cd /gpfs/projects/RestGroup/mariana/carot/TIGR02734/mafft

# iqtree
module unload R/3.6.0
module load anaconda/2
source activate phylo

iqtree -s ../OG5_135453_phydes_sf_1.hmmal.cT.derep.V4.hmmal.cT.derep.V4.HMMAL.seq.mafft.90.fas -safe -bb 1000 -alrt 1000 -m LG+F+R10 -nt AUTO

conda deactivate
module unload anaconda/2

