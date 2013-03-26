
# Setting the predicted t=1 to observed t=1, for easier manipulation later
pred[1,"pG"]<-ob[1,"pG"] 
pred[1,"pI"]<-ob[1,"pI"]
pred[1,"pA"]<-ob[1,"pA"]

# # Model specification for Original Diffusion
for( i in 2:yearCount ) {
#   # Model is defined though predictions of specific transitions
#   # How many people will transit from g(Goers) to i(Irregulars)? 
  pred[i,"pgi"]<-Tgi*pred[i-1, "pG"]*pred[i-1, "pG"]
  pred[i,"pga"]<-Tga*pred[i-1, "pG"]*pred[i-1, "pG"] 
  pred[i,"pig"]<-Tig*pred[i-1, "pI"]*pred[i-1, "pI"]
  pred[i,"pag"]<-Tag*pred[i-1, "pA"]*pred[i-1, "pA"]
  pred[i,"pai"]<-Tai*pred[i-1, "pA"]*pred[i-1, "pA"]
  pred[i,"pia"]<-Tia*pred[i-1, "pI"]*pred[i-1, "pI"]
# #   # The rest of the code does not change with models
  pred[i,"pgg"]<- pred[i-1,"pG"]-pred[i,"pgi"]- pred[i,"pga"]
  pred[i,"pii"]<- pred[i-1,"pI"]-pred[i,"pig"]- pred[i,"pia"]
  pred[i,"paa"]<- pred[i-1,"pA"]-pred[i,"pag"]- pred[i,"pai"]
#   # COUNTING TRANTIONS: pG - gi - ga + ig + ag 
  pred[i, "pG"] <-pred[i-1, "pG"] - pred[i,"pgi"] - pred[i,"pga"] + pred[i,"pig"] + pred[i,"pag"]
  pred[i, "pA"] <-pred[i-1, "pA"] - pred[i,"pai"] - pred[i,"pag"]+ pred[i,"pia"] + pred[i,"pga"]
  pred[i, "pI"] <- 1-pred[i,  "pG"] -pred[i,  "pA"] 
}