library("gRain")
library("gRbase")
library("Rgraphviz")
library("ggm")
library("bnlearn")



dag_bank = dag(~Rev, ~Age, ~RDR|Age, ~HistP|RDR, 
               ~Fiab|HistP:Age, 
               ~Actif|Rev, ~RevF|Actif,
               ~Solv|RDR:Fiab:RevF)
plot(dag_bank)

val = c("0","1") ## valeurs possibles pour chacune des variables
#val0 = c("non Élevé","Élevé") 
#val1 = c("Mauvais","Bon")
#val2 = c("non Fiable","Fiable") 
#val3 = c("non Agé","Agé") 


cp_Rev <- cptable(~Rev,values=c(50,50),levels=val)
cp_Age <- cptable(~Age,values=c(50,50),levels=val)
cp_RDR <- cptable(~RDR|Age,values=c(60,40,40,60),levels=val)
cp_HistP <- cptable(~HistP|RDR, values = c(45,55,55,45),levels=val)
cp_Actif <- cptable(~Actif|Rev,values = c(75,25,25,75),levels=val)
cp_RevF <- cptable(~RevF|Actif,values = c(75,25,25,75),levels=val)
cp_Fiab <- cptable(~Fiab|HistP:Age,values = c(95,5,90,10,10,90,5,95),levels=val)
cp_Solv <- cptable(~Solv|RDR:Fiab:RevF,values = c(95,5,90,10,85,15,80,20,20,80,15,85,10,90,5,95),levels=val)

#cp_Rev
#cp_Age
#cp_RDR
#cp_HistP
#cp_Actif
#cp_RevF
#cp_Fiab
#cp_Solv

net_list = compileCPT(list(cp_Rev,cp_Age,cp_RDR,cp_HistP,cp_Actif,cp_RevF,cp_Fiab,cp_Solv))
grain_bank = grain(net_list)
plot(grain_bank$dag)


querygrain(grain_bank, nodes=c("HistP","Fiab"), type="conditional")

querygrain(grain_bank, nodes=c("Age","Fiab"), type="conditional")

querygrain(grain_bank, nodes=c("Age","RDR"), type="conditional")

querygrain(grain_bank, nodes=c("RDR","HistP"), type="conditional")

querygrain(grain_bank, nodes=c("Rev","Actif"), type="conditional")

querygrain(grain_bank, nodes=c("Actif","RevF"), type="conditional")

querygrain(grain_bank, nodes=c("Fiab","Solv"), type="conditional")

querygrain(grain_bank, nodes=c("RevF","Solv"), type="conditional")



