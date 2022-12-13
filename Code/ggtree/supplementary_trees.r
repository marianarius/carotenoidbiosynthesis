#interactive node
module load R/3.4.4 #packages not installed
module load R/3.6.0 #Same error
module load R/3.6.2 #packages not installed

sinfo | grep idle
srun -N 1 -t 8:00:00 -p gpu --pty bash
srun -N 1 -t 8:00:00 -p p100 --pty bash

#### NEW R version attempt
### R 4.1.1
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

install.packages("ggtree")
install.packages("phytools")
install.packages("ggplot2")
install.packages("colorspace")
install.packages("treeio")
install.packages("ape")
install.packages("phangorn")

####SUPPL TREE LOOP
library("phytools")
library("ggtree")
library("ggimage")
library("ggplot2")
library("colorspace")
library("treeio")
library("ape")
library("phangorn")


##################################
other attached packages:
  [1] treeio_1.18.1       ggplot2_3.3.5       phytools_1.0-3
[4] maps_3.4.0          ape_5.5             ggtree_3.2.1
[7] BiocManager_1.30.16

loaded via a namespace (and not attached):
  [1] phangorn_2.7.1          tidyselect_1.1.1        purrr_0.3.4
[4] lattice_0.20-44         ggfun_0.0.6             colorspace_2.0-2
[7] vctrs_0.3.8             generics_0.1.0          expm_0.999-6
[10] utf8_1.2.2              gridGraphics_0.5-1      rlang_0.4.11
[13] pillar_1.6.3            withr_2.4.3             glue_1.4.2
[16] DBI_1.1.1               lifecycle_1.0.1         munsell_0.5.0
[19] combinat_0.0-8          gtable_0.3.0            codetools_0.2-18
[22] coda_0.19-4             parallel_4.1.1          fansi_0.5.0
[25] Rcpp_1.0.7              scales_1.1.1            plotrix_3.8-2
[28] clusterGeneration_1.3.7 scatterplot3d_0.3-41    jsonlite_1.7.2
[31] tmvnsim_1.0-2           fastmatch_1.1-3         mnormt_2.0.2
[34] aplot_0.1.3             dplyr_1.0.7             numDeriv_2016.8-1.1
[37] grid_4.1.1              quadprog_1.5-8          tools_4.1.1
[40] yulab.utils_0.0.4       magrittr_2.0.1          lazyeval_0.2.2
[43] patchwork_1.1.1         tibble_3.1.5            crayon_1.4.1
[46] tidyr_1.1.4             pkgconfig_2.0.3         ellipsis_0.3.2
[49] MASS_7.3-54             Matrix_1.3-4            tidytree_0.3.9
[52] ggplotify_0.1.0         assertthat_0.2.1        R6_2.5.1
[55] igraph_1.2.6            nlme_3.1-152            compiler_4.1.1

##################################

options(stringsAsFactors = F)
setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
treeloop<-c("ttbcd_suppl_PG", 
            "lycyc2x_suppl_PG", 
            "physy_suppl_PG", 
            "phydes_suppl_PG", 
            "aurlibcd_suppl_PG", 
            "oxylycyc_suppl_PG")

#include address in treefiles hmm
treefiles<-c("/gpfs/projects/RestGroup/mariana/carot/PF15461/PF15461_sf_1.hmmal.cT.derep.V4.hmmal.seq.mafft.90.fas.treefile",
             "/gpfs/projects/RestGroup/mariana/carot/TIGR03462/lycyc_2x.HMMAL.all_1.cT.derep.V4.COR.dg.mafft.90.fas.treefile", 
             "/gpfs/projects/RestGroup/mariana/carot/PF00494/OG5_131363_physy/OG5_131363_physy_sf_1.hmmal.cT.derep.V4.hmmal.cT.derep.V4.HMMAL.seq.mafft.99.fas.treefile", 
             "/gpfs/projects/RestGroup/mariana/carot/TIGR02734/OG5_135453_phydes_sf_1.hmmal.cT.derep.V4.hmmal.cT.derep.V4.HMMAL.seq.mafft.90.fas.treefile", 
             "/gpfs/projects/RestGroup/mariana/carot/PF03055/OG5_128633_aurlibcd_1.HMMAL.seq.SF.hmmal.cT.derep.V4.hmmal.cT.derep.V4.hmmal.90.fas.treefile", 
             "/gpfs/projects/RestGroup/mariana/carot/PF05834/OG5_143063_oxyrlyc_1.fasta.HMMAL.seq.SF.hmmal.cT.derep.V4.hmmal.cT.derep.V4.hmmal.seq.mafft.90.fas.treefile")


###adding colors
umck1<-read.csv("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/final.col.key.csv")
umck<-umck1[,2:ncol(umck1)]

##############################################
### START OF MANUAL LOOP
# ttbcd, lycyc2x, physy, phydes, aurlibcd, oxylycyc
jj <- 1 #ttbcd

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

#domain specific tree graphing parameters
if(jj == 1){ #ttbcd
  si <- 2.5 #tiplabel and bootrap value size 
  li <- 0.5 #line size
  nps <- 1.3 #nodepoint size
  ww <- 8.5 #page width
  hh <-11 #page height
  xlimm <- 7
}

tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}


tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}
#scalebar
sc.a <- 0.4
sc.b <- (length(tree2B$tip.label)*0.97)

if(jj == 1){
  sc.b <- 30
}

#redo with accession
tree2A<-tree2
tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

#tree
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si+0.5, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")


pdf(paste(treeloop[jj],"redo.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
p
dev.off()

###################################################
jj <- 2 #lycyc2x

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

if(jj == 2){ #lycyc2x
  si <- 1.3 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 1.1 #nodepoint size
  ww <- 8.5 #page width
  hh <-11 #page height
  xlimm <- 6
}

tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}


tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}
#scalebar
sc.a <- 0.4
sc.b <- (length(tree2B$tip.label)*0.97)
if(jj == 2){
  sc.b <- (sc.b-12)
}

#tree
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

### lycyc2x redo with accession
tree2A<-tree2
tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

#scalebar
if(jj == 2){ #lycyc2x
  si <- 1.3 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 1.1 #nodepoint size
  ww <- 8.5 #page width
  hh <-11 #page height
  xlimm <- 6
}

sc.a <- 0.4
sc.b <- (length(tree2B$tip.label)*0.97)
if(jj == 2){
  sc.b <- (sc.b-22)
}
#tree
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
pdf(paste(treeloop[jj],"redo.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
if(jj == 2){
  p4 <- collapse(p, node=392) 
  p5 <- collapse(p4, node=413) 
  p6<-p5 + geom_tiplab(aes(subset=(node==c(392)),color="#A1D4D5", label= tree2B$tip.label[153]),size=si, fontface="italic") +
    geom_tiplab(aes(subset=(node==c(413)),color="#A1D4D5", label= tree2B$tip.label[159]),size=si, fontface="italic") 
  p6
} 
dev.off()

###################################################
jj <- 3 #physy

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

if(jj == 3){ #physy
  si <- 1 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 0.8 #nodepoint size
  ww <- 8.5 #page width
  hh <- 44 #page height
  xlimm <- 6.5
}


tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}

tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

### SUPPL with accession
si <- 1 #tiplabel and bootrap value size 
li <- 0.3 #line size
nps <- 0.8 #nodepoint size
ww <- 8.5 #page width
hh <- 44 #page height
xlimm <- 6.5
sc.a <- 0.4
sc.b <- (length(tree2B$tip.label)*0.97)
#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
pdf(paste(treeloop[jj],"redo.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
p
dev.off()

###################################################
jj <- 4 #phydes

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

if(jj == 4){ #phydes
  si <- 1 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 1.3 #nodepoint size
  ww <- 8.5 #page width
  hh <- 33 #page height
  xlimm <- 4
}

tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}

tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

### redo tree with branch swap and accessions
tree2A<-tree2
tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
pdf(paste(treeloop[jj],"redo_flip.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
gridExtra::grid.arrange(flip(p, 826, 1145), ncol = 1)
dev.off()


###################################################
jj <- 5 #aurlibcd

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

if(jj == 5){ #aurlibcd
  si <- 1 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 1 #nodepoint size
  ww <- 8.5 #page width
  hh <- 22 #page height
  xlimm <- 4.5
}

tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}


tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
pdf(paste(treeloop[jj],"redo.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
p
dev.off()


###################################################
jj <- 6 #oxylycyc

tree1 <- read.tree(treefiles[jj])
#tree <- midpoint.root(tree1) # default to ggtree midpoint root which does not actually midpoint root
tree <- phangorn::midpoint(tree1)

if(jj == 6){ #oxylycyc
  si <- 2 #tiplabel and bootrap value size 
  li <- 0.3 #line size
  nps <- 1 #nodepoint size
  ww <- 8.5 #page width
  hh <- 22 #page height
  xlimm <- 6
}


tree2<-tree
na<-c()
nb<-c()
nc<-c()
nd<-c()
for(i in 1:length(tree2$tip.label)) {
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
}

ss<- data.frame(name=tree2$tip.label,a=na,b=nb, x=nc,d=nd)

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

#renaming tips
tree2B$tip.label<-ss$b
xn<-as.list(unique(ss$b))
names(xn)<-unique(ss$b)
for (i in 1:length(xn)) {
  xn[[i]]<-grep(xn[[i]],tree2B$tip.label, value = T)
}

#with corresponding color
xn1<-as.list(unique(umck$label))
names(xn1)<-unique(umck$label)
for (i in 1:length(xn1)) {
  taxa.names<-umck$X[umck$label==names(xn1)[i]]
  tip.names<-tree2B$tip.label[tree2B$tip.label %in% taxa.names]
  xn1[[i]]<-tip.names
}
#
sxs<-length(tree2B$tip.label)
nn<-c()
uuu<-c()
for (i in 1:sxs) {
  u.n<-c(i+(sxs-1))
  u<-getDescendants(tree2B,u.n)
  uu<-unique(ss$b[u[which(u<(sxs+1))]])
  if (length(uu)==1) {
    nn<-c(nn, u.n)
    uuu<-c(uuu,uu)
  }
}
#
p.u<-unique(uuu)
culm<-c()
nculm<-c()
for (i in 1:length(p.u)) {
  q<-nn[grep(p.u[i],uuu)]
  #pp  <- min(q)
  culm<-c(culm,q)
  nculm<-c(nculm,rep(p.u[i],(length(q))))
}

# 
xn1a<-xn1[lengths(xn1) > 0]
umck2<-unique(umck[,3:4])
umck2a<-umck2[umck2$label %in% names(xn1a),]
umck2b<-umck2a[order(umck2a$label),]
library(plyr)
t2btl<-data.frame(X=tree2B$tip.label)
everytip<-join(t2btl, umck)
tip.colours<-everytip$color

#
toco<-data.frame(name=nculm,node=as.numeric(culm))
d0 <- data.frame(node=1:(Nnode(tree2B)+length(tree2B$tip.label)), color = c(tip.colours, rep("black", Nnode(tree2B))))
d01<-d0
for (i in 1:nrow(toco)) {
  d0$color[toco$node[i]]<- umck$color[grep(toco$name[i],umck$X)]
}

#genus and species names
na<-c()
nb<-c()
nc<-c()
nd<-c()
ne<-c()
nf<-c()
ng<-c()
nh<-c()
nj<-c()
nk<-c()
nl<-c()
nm<-c()
nn<-c()
accession<-c()
for(i in 1:length(tree2$tip.label))
{
  aa<-strsplit(tree2$tip.label[i], "_")[[1]]
  acc<-aa[1]
  a<-aa[2]
  b<-aa[3]
  x<-aa[4]
  d<-aa[5]
  e<-aa[6]
  f<-aa[7]
  g<-aa[8]
  h<-aa[9]
  j<-aa[10]
  k<-aa[11]
  l<-aa[12]
  m<-aa[13]
  n<-aa[14]
  accession<-c(accession,acc)
  na<-c(na,a)
  nb<-c(nb,b)
  nc<-c(nc,x)
  nd<-c(nd,d)
  ne<-c(ne,e)
  nf<-c(nf,f)
  ng<-c(ng,g)
  nh<-c(nh,h)
  nj<-c(nj,j)
  nk<-c(nk,k)
  nl<-c(nl,l)
  nm<-c(nm,m)
  nn<-c(nn,n)
}

ss<- data.frame(name=tree2$tip.label,accession = accession, a=na,b=nb, x=nc,d=nd, e=ne,f=nf, g=ng,h=nh,j=nj,k=nk,l=nl,m=nm,n=nn)

for (i in 1:nrow(ss)) {
  q<-which(is.na(ss[i,]))
  sp<-ss[i,min(q)-1]
  ge<-ss[i,min(q)-2]
  ss$gesp[i]<-paste(ge,sp,collapse="_")
}

#strain
strain <- c()
spp <- c()
gee <- c()
index <- c()
for (i in 1:nrow(ss)) {
  q <- which(is.na(ss[i,]))
  qp <- c()
  sp <- NULL
  ge <- NULL
  str <- NULL
  p2 <- strsplit(ss$name[i], split="_")[[1]]
  d <- which(p2 == "sp.")
  tryCatch({  
    if (d>0) {
      ge<-ss[i,(d)]
      sp<-ss[i,(d+1)]
      str <- ss[i,(d+2)]
    } else{
      sp<-ss[i,min(q)-1]
      ge<-ss[i,min(q)-2]
      str <- NA
    }
  }, error=function(e){})
  if (all(is.na(ge))) {
    
  } else{
    strain<-c(strain,str)
    spp<-c(spp,sp)
    gee<-c(gee,ge)
    index <- c(index,i)
  }
}

for (i in 1:length(index)) {
  ss$gesp[index[i]]<-paste(gee[i],spp[i], strain[i],collapse="_")
} 

#order
nrow(ss)

ales <- grep("ales",ss[,1])
ss$or <- c(0)

for(i in ales){
  oy <- grep("ales", ss[i,])[2]
  ss$or[i] <- ss[i,oy]
}

for(i in c(1:nrow(ss))[-ales]){
  oy <- ss$x[i]
  if(is.na(oy)){
    oy <- ss$b[i]
  }
  if(oy == strsplit(ss$gesp[i],split = " ")[[1]][1]){
    oy <- ss$b[i]
  }
  ss$or[i] <- oy
}


tree2A<-tree2
#tree2A$tip.label<-ss$gesp
tree2A$tip.label<-paste(ss$gesp," (",ss$or,")", sep="")

#remove "Root" node label
tree2A$node.label[1]<-""
tree2B<-tree2A

xn<-as.list(unique(ss$a))
names(xn)<-unique(ss$a)
for (i in 1:length(xn)) {
  xn[[i]]<-ss$gesp[grep(xn[[i]],ss$a)]
}

#remove "Root" node label
tree2A<-tree2
tree2A$node.label[1]<-""
tree2B<-tree2A

tree2A$tip.label<-paste(ss$gesp," (",ss$or, ", ", ss$accession,")", sep="")
p0<- ggtree(tree2A) %<+% d0 + aes(color=I(color))
p <- p0 + 
  theme(legend.position="none") +
  geom_treescale(sc.a,sc.b) + 
  xlim(0,xlimm) +
  geom_nodepoint(aes(subset = as.numeric(sub("/.*", "", label)) >= 80 | as.numeric(sub(".*/", "", label)) >= 95), size=nps, color="black") +
  geom_tiplab(aes(color=I(color)),size=si, fontface="italic",linesize= li) + 
  geom_text2(aes(subset = ! isTip, label=label),hjust = -0.2, vjust = 0.5, size = si, col="black")

setwd("/gpfs/projects/RestGroup/mariana/carot/synthesis/final/supl/")
pdf(paste(treeloop[jj],"redo.pdf", sep="-"), width=ww, height=hh) #nodepoint if SH-aLRT >=80 OR uFB >= 95 
p
dev.off()

