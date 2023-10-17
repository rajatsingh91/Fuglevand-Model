#################### Recruitment Threshold Excitation Model###########
n = 120
RR = seq(1,30,length.out = 120)
index = seq(1,n)
alpha = log(RR)/n
RTE = exp(alpha*index)    ###### Equation 1 ########
percentage = 100


MFR = 8               ###### Minimum Firing Rate ######
ge = 1                ###### Gain #####
PFRD = 10             ###### Peak Firing Rate Difference ##  
PFR_1 = 45
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

for (i in 1:n) {
  plot(Output[[i]], ylim = c(8,45), xlim = c(0,100), xlab = "Excitation Level(%)"
       ,   ylab = "Firing Rate (PPS)")
  par(new = TRUE)
}


############# Motor Unit Twitch Model ###################

TL = seq(90,30, length.out = 120)
RP = seq(1,100,length.out = 120)
b = log(RP)/n
P = exp(b*index)
t = seq(0,200)

RT = 3
c = log(RP,RT)

f = list()
T = list()
for (i in 1:n) {
T[[i]] = TL[i]*(1/P[i])^(1/c[i])
f[[i]] = (P[i]*t)/T[[i]]*exp(1-t/T[[i]])
#plot(f[[i]], ylim=c(0,100), xlim = c(0,200), type = 'line', xlab = 
#      'Samples', ylab = 'Peak Force (a.u)', main = 'MU Twitch')
#par(new = TRUE)
  }

############# Motor Unit Action Potential Model ########

MUAP = list()
x = seq(-5,5,length.out = 50)
for (i in 1:n) {
MUAP[[i]] = (1-x^2)*exp(-x^2/2)*RP[i]
#plot(MUAP[[i]], type = 'line', ylim = c(-40,100), xlab = 'Samples', ylab = 'MUAP Ampltiude', main = 'MUAP Waveform')
#par(new = TRUE)
}

########## Motor Unit Recruiment #######################

ns = 1000   ######Number of Samples increase#####
sim = 100   ######Length of Signal increase######
len = 2000

suppressWarnings({
Mnum = matrix(data = NA,length(Output), ncol = 1)
for (i in 1:length(Output)) 
Mnum[i,] = Output[[i]][1,1]
}
)

MU_rec = na.omit(Mnum)

Mu = list()
for (i in 1:length(MU_rec)) {
Mu[[i]] = c(round(min(Output[[i]][,2])), round(median(Output[[i]][,2])), round(max(Output[[i]][,2]))) 
}

fra = list()
Mutrains = list()
for (i in 1:length(MU_rec)) {
fra[[i]] = round(100/Mu[[i]])
}

#suppressWarnings({
le_ngth = matrix(NA, nrow = length(MU_rec), ncol = 1)
for (i in 1:length(MU_rec)){
Mutrains[[i]] = c(rep(c(1,rep(0,fra[[i]][1])),sim), rep(c(1,rep(0,fra[[i]][2])),sim), rep(c(1,rep(0,fra[[i]][3])),sim))    
le_ngth[i] = round(Output[[i]][1,1]*sim)
#le_ngth[i] = na.omit(le_ngth[[i]])
}
#)

ax = data.frame(unlist(le_ngth))
ax = na.omit(ax)

EMG = list()
Forces = list()
for (i in 1:nrow(ax)) {
Mutrains[[i]] = c(rep(0,ax[i,1]), Mutrains[[i]])
#Mutrains[[i]][Mutrains[[i]] == 0] <- NA
#Mutrains[[i]] = Mutrains[[i]]+(i)
Forces[[i]] = conv(f[[i]],Mutrains[[i]]) 
EMG[[i]] = conv(MUAP[[i]],Mutrains[[i]])
}

a = seq(1,length(MU_rec),10)
#ISItrains = Mutrains
#for (i in 1:length(a)) {
#plot(ISItrains[[a[i]]], ylim = c(1,120), xlim = c(10,len), xlab = 'Samples', ylab = 'Motor Unit Number', main = 'Motor Unit Train Simulation')
#segments(1:length(ISItrains[[a[i]]]), ISItrains[[a[i]]]-5, 1:length(ISItrains[[a[i]]]), ISItrains[[a[i]]], col = blues9, lwd = 5)
#par(new = TRUE)
#}
EMGout = list()
ma = list()
for (i in 1:length(MU_rec)){
ma[[i]] = Forces[[i]][(length(Forces[[i]])-500):length(Forces[[i]])] 
plot(ma[[i]], ylim = c(0,700), type = 'line', ylab = 'Force (a.u.)', xlab = 'Samples', main = 'Total Force')
par(new = TRUE)
EMGout[[i]] = EMG[[i]][1:5000]
}

x = Reduce("+", EMGout)

Tetanic = plot(Reduce("+", ma), type = 'line', xlab = 'Samples', ylab = 'Force (a.u.)', lwd = 2, main = 'Total Force')
