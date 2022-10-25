#title: "Partie Pratique"
#author: "Yunfan CAI"
#date: "2022/10/8"

library("gRain")
library("gRbase")
library("Rgraphviz")


#dag_gicleur = dag(~Revenus, ~Actifs|Revenus, ~Age, ~RevenusFuturs|Actifs, ~RatioDvsR|Age, ~Historique|RatioDvsR ,  ~Fiable|Historique:Age , ~Solvable|Fiable:RevenusFuturs:RatioDvsR)
#dag_gicleur
#plot(dag_gicleur)

library("gRain")
val = c("0","1") ## valeurs possibles pour chacune des variables
cp_Revenus <- cptable(~Revenus,values=c(50,50),levels=val)
cp_Actifs <- cptable(~Actifs|Revenus,values=c(50,50,50,50),levels=val)
cp_Age <- cptable(~Age,values=c(50,50),levels=val)
cp_RevenusFuturs <- cptable(~RevenusFuturs|Actifs, values = c(50,50,50,50),levels=val)
cp_RatioDvsR <- cptable(~RatioDvsR|Age,values = c(25,75,25,75),levels=val)
cp_Historique <- cptable(~Historique|RatioDvsR,values = c(90,10,90,10),levels=val)
cp_Fiable <- cptable(~Fiable|Historique+Age,values = c(50,50,50,50,20,80,20,80),levels=val)
cp_Solvable <- cptable(~Solvable|Fiable+RevenusFuturs+RatioDvsR,values = c(50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50),levels=val)

library("gRain")
net_list = compileCPT(list(cp_Revenus,cp_Actifs,cp_Age,cp_RevenusFuturs,cp_RatioDvsR,cp_Historique,cp_Fiable,cp_Solvable))
grain_gicleur = grain(net_list)
plot(grain_gicleur$dag)


querygrain(grain_gicleur, nodes=c("Historique","Fiable"), type="conditional")

querygrain(grain_gicleur, nodes=c("Age","Fiable"), type="conditional")

querygrain(grain_gicleur, nodes=c("Age","RatioDvsR"), type="conditional")

querygrain(grain_gicleur, nodes=c("RatioDvsR","Historique"), type="conditional")

querygrain(grain_gicleur, nodes=c("Revenus","Actifs"), type="conditional")

querygrain(grain_gicleur, nodes=c("Actifs","RevenusFuturs"), type="conditional")

querygrain(grain_gicleur, nodes=c("Fiable","Solvable"), type="conditional")

querygrain(grain_gicleur, nodes=c("RevenusFuturs","Solvable"), type="conditional")