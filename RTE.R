RTE = function(n = MUnumbers){
#################### Recruitment Threshold Excitation Model###########
n 
RR = seq(1,30,length.out = 120)
index = seq(1,n)
alpha = log(RR)/n
RTE = exp(alpha*index)    ###### Equation 1 ########
}