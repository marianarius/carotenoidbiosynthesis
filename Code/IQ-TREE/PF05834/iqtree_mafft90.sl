#!/usr/bin/env bash

#SBATCH --job-name=iqtree_mafft90
#SBATCH --output=iqtree_mafft90.log
#SBATCH --ntasks-per-node=40
#SBATCH --nodes=1
#SBATCH --time=12:00:00
#SBATCH -p medium-40core

module load shared
module load anaconda/2

source activate phylo
iqtree -s OG5_143063_oxyrlyc_1.fasta.HMMAL.seq.SF.hmmal.cT.derep.V4.hmmal.cT.derep.V4.hmmal.seq.mafft.90.fas -safe -bb 1000 -alrt 1000 -m WAG+F+R9 -nt AUTO
conda deactivate
module unload anaconda/2
