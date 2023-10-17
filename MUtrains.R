MUtrains = function(Input,ns = sample_rate, sim = simulation_length, len = sample_plot_length){
ns   ######Number of Samples increase#####
sim   ######Length of Signal increase######
len 
suppressWarnings({
  Mnum = matrix(data = NA,length(Input), ncol = 1)
  for (i in 1:length(Input)) 
    Mnum[i,] = Input[[i]][1,1]
}
)
MU_rec = na.omit(Mnum)
Mu = list()
for (i in 1:length(MU_rec)) {
  Mu[[i]] = c(round(min(Input[[i]][,2])), round(median(Input[[i]][,2])), round(max(Input[[i]][,2]))) 
}
fra = list()
MUtrains = list()
for (i in 1:length(MU_rec)) {
  fra[[i]] = round(100/Mu[[i]])
}
#suppressWarnings({
le_ngth = matrix(NA, nrow = length(MU_rec), ncol = 1)
for (i in 1:length(MU_rec)){
  MUtrains[[i]] = c(rep(c(1,rep(0,fra[[i]][1])),sim), rep(c(1,rep(0,fra[[i]][2])),sim), rep(c(1,rep(0,fra[[i]][3])),sim))    
  le_ngth[i] = round(Input[[i]][1,1]*sim)

}
ax = data.frame(unlist(le_ngth))
ax = na.omit(ax)
for (i in 1:nrow(ax)) {
MUtrains[[i]] = c(rep(0,ax[i,1]), MUtrains[[i]])
}  
return(MUtrains)
}