##head file
head<-"/gpfs/projects/RestGroup/mariana/carot/PF05834/complete_PF05834_1.cT_1.out"
##tree file
tree<-"/gpfs/projects/RestGroup/mariana/carot/PF05834/OG5_143063_oxyrlyc_1.fasta.HMMAL.fasta_cT.treefile"
##out
out<-"/gpfs/projects/RestGroup/mariana/carot/PF05834/OG5_143063_oxyrlyc_1.fasta.HMMAL.fasta_cT.derep.txt"

Hmm <- read.delim(head,header=FALSE,sep="",stringsAsFactors=FALSE)
colnames(Hmm) <- c("eval","score","bias","best1eval","best1score","best1bias","expdom","Ndom","name")
#rownames(Hmm) <- Hmm$name

#read in tree
library(phytools)
Sq1 <- read.newick(file=tree)
numdropped <- 1

while(numdropped > 0){ 
  tips<- Sq1$tip.label
  
  #find all sisters on the tree (note: all sisters are repeated twice…)
  sisters <- as.data.frame(matrix(NA, ncol=6, nrow=length(Sq1$tip.label))) # empty matrix
  sisters[,1] <- Sq1$tip.label # populate first column with tip.labels  
  for(i in 1:length(Sq1$tip.label)){
    tmp <- phytools::getSisters(Sq1, Sq1$tip.label[i], mode="label") #the sister of the current node
    
    if(!is.null(tmp$tips)){
      tmp1 <- unique(sapply(strsplit(Sq1$tip.label[i],'/'),function(x) x[1])) #strips out the name before "/" for the current node
      tmp2 <-unique(sapply(strsplit(tmp$tips,'/'),function(x) x[1])) # the sister node, with the name strippped
      
      
      if(length(Hmm[tmp2,"best1score"]) > 1) #{print(paste(i,length(Hmm[tmp2,"best1score"])))}
      {
        tmp$tips <- tmp$tips[which.max(Hmm[grep(tmp1, Hmm$name),"best1score"])]  
        #if there are multiple sisters because there is a polytomy, identify the best score of those
        #restrip the sister, now that we identified the preferred in case of a polytomy
        tmp2 <-unique(sapply(strsplit(tmp$tips,'/'),function(x) x[1])) # the sister node, with the name strippped
      }
      
      sisters[i,2] <- tmp$tips #sister OR sister in the polytomy with the best score
      sisters[i,3] <- tail(strsplit(tmp1,"_")[[1]],n=2)[2] #genus of the current node
      sisters[i,4] <- tail(strsplit(tmp2,"_")[[1]],n=2)[2] #genus of the sister, for only one in the case of a polytomy
      sisters[i,5] <- as.numeric(max(Hmm[grep(tmp1, Hmm$name),"best1score"])) #score of the current node
      sisters[i,6] <- as.numeric(max(Hmm[grep(tmp2, Hmm$name),"best1score"])) #score of the sister
    }
  }
  colnames(sisters) <- c("node1","node2","gen1","gen2","node1score","node2score")
  saveDeepSister <- sisters[which(is.na(sisters[,2])),] #save those nodes that don't both two terminal tips
  ###### removal of polytomy tips
  sistersa<-sisters
  sistersb <- sistersa[-which(is.na(sistersa[,2])),]
  dnodes<-duplicated(sistersb$node2) # identifies duplicates, but not original
  #sum(as.numeric(dnodes)) #176
  dnodes2<-grep("TRUE", dnodes) #converts TRUE to coordinate
  #not identified in dnodes is the original that is duplicated
  ######IF statement in case there are no more duplicates?
  if (length(dnodes2) > 0) {
    dnodes3<-sistersb$node2[dnodes2] 
    
    dnodes4<-c()
    for(i in 1:length(dnodes3))
    {
      h<-grep(dnodes3[i], sistersb$node2)
      dnodes4<-c(dnodes4,h)
    }
    sistersc<-sistersb[-dnodes4,]
    sistersd<- rbind(saveDeepSister, sistersc)
    #find all sisters on the tree (note: all sisters are repeated twice…)
    sisters <- as.data.frame(matrix(NA, ncol=6, nrow=length(sistersd$node1))) # empty matrix
    sisters[,1] <- sistersd$node1
    
    for(i in 1:length(sistersd$node1)){
      tmp <- phytools::getSisters(Sq1, sistersd$node1[i], mode="label") #the sister of the current node
      
      if(!is.null(tmp$tips)){
        tmp1 <- unique(sapply(strsplit(sistersd$node1[i],'/'),function(x) x[1])) #strips out the name before "/" for the current node
        tmp2 <-unique(sapply(strsplit(tmp$tips,'/'),function(x) x[1])) # the sister node, with the name strippped
        
        if(length(Hmm[tmp2,"best1score"]) > 1) #{print(paste(i,length(Hmm[tmp2,"best1score"])))}
        {
          tmp$tips <- tmp$tips[which.max(Hmm[grep(tmp1, Hmm$name),"best1score"])]  
          #if there are multiple sisters because there is a polytomy, identify the best score of those
          #restrip the sister, now that we identified the preferred in case of a polytomy
          tmp2 <-unique(sapply(strsplit(tmp$tips,'/'),function(x) x[1])) # the sister node, with the name strippped
        }
        
        sisters[i,2] <- tmp$tips #sister OR sister in the polytomy with the best score
        sisters[i,3] <- tail(strsplit(tmp1,"_")[[1]],n=2)[2] #genus of the current node
        sisters[i,4] <- tail(strsplit(tmp2,"_")[[1]],n=2)[2] #genus of the sister, for only one in the case of a polytomy
        sisters[i,5] <- as.numeric(max(Hmm[grep(tmp1, Hmm$name),"best1score"])) #score of the current node
        sisters[i,6] <- as.numeric(max(Hmm[grep(tmp2, Hmm$name),"best1score"])) #score of the sister
      }
    }
    colnames(sisters) <- c("node1","node2","gen1","gen2","node1score","node2score")
    #save those nodes that don't both two terminal tips
    #same number of NA from before which means that nodes that are sisters to polytomies are still considered sisterless    
    saveDeepSister <- sisters[which(is.na(sisters[,2])),]
  }
  
  #########
  sisters <- sisters[-which(is.na(sisters[,2])),] # prune away tips that do not have a labelled tip as sister 
  #these are tips that are sister to a more than one tip (i.e. a clade). they are now stored in saveDeepSister
  
  #build a list of tips that we want to KEEP
  sisters2 <- apply(sisters,1,function(dx){
    if(dx[3] != dx[4]){ #compare genus of node to sister; if they don't match:
      return(dx[1])	#return the current node. note this will occur AGAIN for when the sister node is the current node
      #return(c(dx[1],dx[2])) #if so, this may work
    }
    else{ #if the genera ARE the same between the node and its sister
      if(dx[5] > dx[6]){ #if the node score is HIGHER than the sister score
        return(dx[1]) #return the node name
      }
      #delete this; we don't need to keep the sister; it will occur when the sister is the active node
      #	else{ return(dx[2])
      #}	
    }
    
  })
  #add back in internally branching tips
  saveDeepSister2 <- c(saveDeepSister$node1,saveDeepSister$node2)
  saveDeepSister3 <- saveDeepSister2[!(is.na(saveDeepSister2))]
  sisters3 <- unique(c(sisters2,saveDeepSister3))
  
  droptips <- tips[!(tips %in% sisters3)]
  numdropped <- length(droptips)
  print(paste("dropped",numdropped,"tips"))
  Sq1 <- drop.tip(Sq1,droptips)
  
} #while numdropped > 0
finaltips <- Sq1$tip.label
write(as.vector(finaltips), file = out)

