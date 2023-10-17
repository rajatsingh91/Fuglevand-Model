Output = function(MFR, PFRD, PFR_1, percentage, RTE, ge,n = MUnumbers){
MFR                ###### Minimum Firing Rate ######
ge                 ###### Gain #####
PFRD              ###### Peak Firing Rate Difference ##  
PFR_1 
PFR = list()
for (i in 1:n) {
  PFR[[i]] = PFR_1-PFRD*(RTE[i]/RTE[n])  ##### Peak Firing Rate MUs ### Equation 5 #####
}

Emax = list()
E_t = list()         
FR = list()
Emaxpercent = list()
for (i in 1:n) {
  Emax[[i]] = RTE[i] + (PFR[[i]]-MFR)/ge ### Maximum Excitation ### Equation 8 ####
}


############# Excitation drive Firing Rate Relationship #############

for (i in 1:n) {
  E_t[[i]] = seq(RTE[i], Emax[[i]], length.out = 120) #### Excitation Drive ###
  FR[[i]] = (E_t[[i]]-RTE[i])+MFR                     #### Firing Rate #####
}

############## Plotting motor neuron model ##############

h = list()
pulse = list()
Output = list()


for (i in 1:n) {
  Emaxpercent[[i]] = E_t[[i]]/Emax[[n]]*100  
  h[[i]] = as.data.frame(c(data.frame(Emaxpercent[[i]]),data.frame(FR[[i]])))
  pulse[[i]] = which(h[[i]][,1] < percentage)
  #Output[[i]] = h[[i]][pulse[[i]],2]
  Output[[i]] = h[[i]][pulse[[i]],1:2]
}
return(Output)
}