library("gRain")
library("gRbase")
library("Rgraphviz")
library("ggm")
library("bnlearn")

data(marks)
names(marks)

dag_notes = empty.graph(names(marks))

arcs(dag_notes) = matrix(
  c("VECT", "MECH",
    "ALG", "MECH",
    "ALG", "VECT",
    "ANL", "ALG",
    "STAT", "ALG",
    "STAT", "ANL"),
  ncol = 2, byrow = TRUE, dimnames = list(c(), c("from", "to")))

mat_ad = matrix(c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0,
                  0, 0, 1, 0, 0, 0, 0, 0), nrow = 5,
                dimnames = list(names(marks), names(marks)))

dag2_notes = empty.graph(names(marks))
amat(dag2_notes) = mat_ad

dag2 = drop.arc(dag_notes, from = "STAT", to = "ANL")

v_st = list(arcs = vstructs(dag2, arcs = TRUE), lwd = 4, col = "black")
graphviz.plot(dag2, highlight = v_st, layout = "fdp", main = "Interdépendances des sujets")


name_Crit <- c("Age","Fiab","HistP","RDR","Solv","RevF","Rev","Actif")

dag_bank = empty.graph(name_Crit)


arcs(dag_bank) = matrix(
  c("Age", "Fiab",
    "Age", "RDR",
    "RDR", "HistP",
    "RDR", "Solv",
    "HistP", "Fiab",
    "Fiab", "Solv",
    "Rev", "Actif",
    "Actif", "RevF",
    "RevF", "Solv"),
  ncol = 2, byrow = TRUE, dimnames = list(c(), c("from", "to")))

graphviz.plot(dag_bank, layout = "fdp", main = "Interdépendances des sujets")

dag_bank1 = dag(~Rev, ~RDR|Age, ~HistP|RDR, 
                ~Fiab|HistP:Age, 
                ~Actif|Rev, ~RevF|Actif,
                ~Solv|RDR:Fiab:RevF)
dag_bank1

plot(dag_bank1)


val0 = c("Faible","Moyen","Élevé") ## valeurs possibles pour chacune des variables
val1 = c("Mauvais","Bon")
val2 = c("Non Fiable","Fiable") 
val3 = c("<25","[25:50]","[50:65]","[>65]") 

cp_Rev <- cptable(~Rev,values=c(33,33,33),levels=val0)
cp_Age <- cptable(~Rev,values=c(25,25,25,25),levels=val3)
cp_RDR <- cptable(~RDR|Age,values=c(80,15,5,50,20,30,20,30,50,5,15,80),levels=val0)
cp_HistP <- cptable(~HistP|RDR, values = c(5,95,60,40,95,5),levels=val1)
cp_Actif <- cptable(~Actif|Rev,values = c(85,10,5,33,33,33,5,10,85),levels=val0)
cp_RevF <- cptable(~RevF|Actif,values = c(85,10,5,33,33,33,5,10,85),levels=val0)

cp_Fiab <- cptable(~Fiab|HistP:Age,values = c(95,5,60,40,50,50,30,70,70,30,50,50,40,60,5,95),levels=val2)
cp_Solv <- cptable(~Solv|RDR:Fiab:RevF,values = c(0,1),levels=val2)

cp_Rev
cp_Age
cp_RDR
cp_HistP
cp_Actif
cp_RevF
cp_Fiab
cp_Solv


net_list = compileCPT(list(cp_Rev,cp_RDR,cp_HistP,cp_Fiab,cp_Actif,cp_RevF,cp_Solv))
grain_bank = grain(net_list)
plot(grain_bank$dag_bank1)


dag_bank2 = dag(~Rev, ~RDR|Age, ~HistP|RDR, 
                ~Fiab|HistP:Age, 
                ~Actif|Rev, ~RevF|Actif)
dag_bank2

plot(dag_bank2)


val = c("0","1") ## valeurs possibles pour chacune des variables


cp_Rev <- cptable(~Rev,values=c(1,0),levels=val)
cp_Age <- cptable(~Rev,values=c(1,0,1,0),levels=val)
cp_RDR <- cptable(~RDR|Age,values=c(1,0,1,0),levels=val)
cp_HistP <- cptable(~HistP|RDR, values = c(1,0,1,0),levels=val)
cp_Actif <- cptable(~Actif|Rev,values = c(1,0,1,0),levels=val)
cp_RevF <- cptable(~RevF|Actif,values = c(1,0,1,0),levels=val)

cp_Fiab <- cptable(~Fiab|HistP:Age,values = c(1,0,1,0),levels=val)


net_list2 = compileCPT(list(cp_Rev,cp_RDR,cp_HistP,cp_Fiab,cp_Actif,cp_RevF))
grain_bank2 = grain(net_list2)
plot(grain_bank$dag_bank1)

dag_gicleur = dag(~C, ~P|C, ~O|C, ~G|O:P)
dag_gicleur

plot(dag_gicleur)

val = c("0","1") ## valeurs possibles pour chacune des variables
cp_C <- cptable(~C,values=c(50,50),levels=val)
cp_P <- cptable(~P|C,values=c(90,10,5,95),levels=val)
cp_O <- cptable(~O|C, values = c(40,60,55,45),levels=val)
cp_G <- cptable(~G|P+O,values = c(1,0,0,1,0,1,0,1),levels=val)

net_list = compileCPT(list(cp_C,cp_P,cp_O,cp_G))
grain_gicleur = grain(net_list)
plot(grain_gicleur$dag)
