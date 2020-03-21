rm(list = ls())
load('result1.Rdata')
alpha=0.05;

s=matrix(data=0, nrow = 2, ncol = 11)
w_pow=NULL;
w_p=matrix(data=NA, nrow = length(p2), ncol = 1)
wm_pow=matrix(data=NA, nrow = 1, ncol = length(p2[[1]]))
wp=NULL;
wmwap=NULL;

for (i in 1:length(p2)){
  w_pow[1]=sum(p0[[i]][1,51:100]<=alpha)/50
  
  for (j in 1:length(p2[[i]])){
    wm_pow[j]=sum(p2[[i]][[j]][1,51:100]<=alpha)/50
  }
  w_p[i,]=w_pow;
  
  s[1,]=s[1,]+wm_pow;
}

wp[1]=sum(w_p[,1])/length(p2);

wmwap=s/length(p2);
wp
wmwap
