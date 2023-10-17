MUtwitch = function(n = MUnumbers, index, samples, CT1, CTn, TF1, TFn){
n 
index

TL = seq(CT1,CTn, length.out = 120)   ########Contraction Time########
RP = seq(TF1,TFn,length.out = 120)    ########Twitch Force############
b = log(RP)/n
P = exp(b*index)
t = seq(0,samples)

RT = 3
c = log(RP,RT)

MUtwitch = list()
T = list()
for (i in 1:n) {
  T[[i]] = TL[i]*(1/P[i])^(1/c[i])
  MUtwitch[[i]] = (P[i]*t)/T[[i]]*exp(1-t/T[[i]])
}
return(MUtwitch)
}