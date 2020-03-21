rm(list = ls())
load('result2.Rdata')
alpha=0.05;
p0=p_0
p2=p_2

s=matrix(data=0, nrow = length(p2[[1]]), ncol = 11)
w_pow=NULL;
w_p=matrix(data=NA, nrow = length(p2), ncol = 1)
wm_pow=matrix(data=NA, nrow = 1, ncol = length(p2[[1]][[1]]))
wp=NULL;
wmwap=NULL;

for (i in 1:length(p2)){
  w_pow[1]=sum(p0[[i]][1,51:100]<=alpha)/50

  for (j in 1:length(p2[[i]])){
    for (k in 1:length(p2[[i]][[j]])){
      wm_pow[1,k]=sum(p2[[i]][[j]][[k]][1,51:100]<=alpha)/50
    }
    
    s[j,]=s[j,]+wm_pow;
  }
  w_p[i,]=w_pow;
}

wp[1]=sum(w_p[,1])/length(p2);
wmwap=s/length(p2);
wp
wmwap
