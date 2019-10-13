#Load packages
library(gRbase)
library(gRain)
library(ggm)
library(igraph)
library(Rgraphviz)


# a)  ---------------------------------------------------------------------


# Construct DAG -----------------------------------------------------------


g <- list(~Sex, ~Smoker|Sex, ~SuffHeartF, ~Inherit|Smoker, ~Hyperchol|Smoker:SuffHeartF, ~CAD|Inherit:Hyperchol)
cad1_dag = dagList(g)

plot(cad1_dag)
title("Coronary Artery Disease")


# Find d-separations ------------------------------------------------------


dSep(as(cad1_dag, "matrix"),"Sex","SuffHeartF",cond=NULL) #True
dSep(as(cad1_dag, "matrix"),"Sex","Inherit","Smoker") #True
dSep(as(cad1_dag, "matrix"),"Sex","Hyperchol","CAD") #False
dSep(as(cad1_dag, "matrix"),"Sex","Hyperchol","Smoker") #True
dSep(as(cad1_dag, "matrix"),"Sex","SuffHeartF","CAD") #False
dSep(as(cad1_dag, "matrix"), "Inherit","SuffHeartF","Smoker") #True
dSep(as(cad1_dag, "matrix"),"Smoker","CAD",c("Inherit","Hyperchol")) #True
dSep(as(cad1_dag, "matrix"),"Sex","SuffHeartF",c("Inherit","Hyperchol","CAD")) #False


# Infer Conditional Probability Tables ------------------------------------


data(cad1)
df <- cad1[,c('Sex','SuffHeartF','Hyperchol','Smoker','Inherit','CAD')]

sex = xtabs(~Sex, df)
smoker_sex = xtabs(~Smoker+Sex, df)
inherit_smoker = xtabs(~Inherit+Smoker, df)
hyperchol_smoker_suffheartf = xtabs(~Hyperchol+Smoker+SuffHeartF, df)
cad_inherit_hyperchol = xtabs(~CAD+Inherit+Hyperchol, df)
suffheartf = xtabs(~SuffHeartF, df)

cad1_cpt = compileCPT(list(sex, smoker_sex, inherit_smoker, cad_inherit_hyperchol, hyperchol_smoker_suffheartf, suffheartf))

# b) ----------------------------------------------------------------------


# Create and compile the network
cad1_network = compile(grain(cad1_cpt))

# Propagate the Network
cad1_network = propagate(cad1_network)


# Find probability before and after evidence ------------------------------

# Absorb evidence
absorbed_nw = setFinding(cad1_network, nodes=c("Sex","Hyperchol"), states = c("Female","Yes"))
getFinding(absorbed_nw)

#Conditional Probability
#With evidence
c_prob_absrb = querygrain(absorbed_nw, nodes = c("SuffHeartF","CAD"), type="conditional") 
print(c_prob_absrb)
#Without evidence
c_prob_n_absrb = querygrain(cad1_network, nodes = c("SuffHeartF","CAD"), type="conditional") 
print(c_prob_n_absrb)

# Joint Probability
#With evidence
j_prob_absrb = querygrain(absorbed_nw, nodes = c("SuffHeartF","CAD"), type="joint") 
print(j_prob_absrb)
#Without evidence
j_prob_n_absrb = querygrain(cad1_network, nodes = c("SuffHeartF","CAD"), type="joint") 
print(j_prob_n_absrb)

# Marginal probability
#With evidence
m_prob_absrb = querygrain(absorbed_nw, nodes = c("SuffHeartF","CAD"), type="marginal") #hf_no=0.61, cad_yes=0.60
print(m_prob_absrb)
#Without evidence
m_prob_n_absrb = querygrain(cad1_network, nodes = c("SuffHeartF","CAD"), type="marginal") #hf_no=0.7, cad_yes=0.45
print(m_prob_n_absrb)


# c) ----------------------------------------------------------------------


# Simulate 5 observations and make predictions
sim_obs = simulate(absorbed_nw, nsim = 5)
print(sim_obs)
predictions<-predict(cad1_network, response = c("Smoker","CAD"),newdata = sim_obs, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
print(predictions)


# d) ----------------------------------------------------------------------


# Simulate 500 observations
sim_obs500 = simulate.grain(absorbed_nw , nsim = 500)
save(sim_obs500, file = 'simulated_obs.RData')

# Make predictions on simulated data
predictions = predict(absorbed_nw, response = c("Smoker", "CAD"), newdata= sim_obs500, type="class")

# Calculate misclassification rate
cm_smoker = table(as.factor(sim_obs500$Smoker),as.factor(predictions$pred$Smoker))
print(cm_smoker[1]/500)
cm_cad = table(as.factor(sim_obs500$CAD),as.factor(predictions$pred$CAD))
print(cm_cad[1]/500)