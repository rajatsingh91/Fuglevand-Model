MUplot = function(Input,ns = sample_rate, sim = simulation_length, len = sample_plot_length){

  ######Number of Samples increase##### ns
  ######Length of Simulation (samples)###### sim
  #######Duration of Signal Plot ########### len
  
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
  Mutrains = list()
  for (i in 1:length(MU_rec)) {
    fra[[i]] = round(100/Mu[[i]])
  }

  #suppressWarnings({
  le_ngth = matrix(NA, nrow = length(MU_rec), ncol = 1)
  for (i in 1:length(MU_rec)){
    Mutrains[[i]] = c(rep(c(rep(0,fra[[i]][1]),1),sim), rep(c(rep(0,fra[[i]][2]),1),sim), rep(c(rep(0,fra[[i]][3]),1),sim))    
    le_ngth[i] = round(Input[[i]][1,1]*sim)
  }
  #)
  
  ax = data.frame(unlist(le_ngth))
  ax = na.omit(ax)
  
  for (i in 1:nrow(ax)) {
    Mutrains[[i]] = c(rep(0,ax[i,1]), Mutrains[[i]])
    Mutrains[[i]][Mutrains[[i]] == 0] <- NA
    Mutrains[[i]] = Mutrains[[i]]+(i)
  }
  
  a = seq(1,length(MU_rec),10)
  ISItrains = Mutrains
  for (i in 1:length(a)) {
    plot(ISItrains[[a[i]]], ylim = c(1,120), xlim = c(10,len), xlab = 'Samples', ylab = 'Motor Unit Number', main = 'Motor Unit Train Simulation')
    segments(1:length(ISItrains[[a[i]]]), ISItrains[[a[i]]]-5, 1:length(ISItrains[[a[i]]]), ISItrains[[a[i]]], col = blues9, lwd = 5)
    par(new = TRUE)
  }
  
}