rm(list = ls())
load('nm2.Rdata')
n_case=dim(x[[1]])[2]
n_control=dim(y[[1]])[2]
sca=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1);
sca2=c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0);

nperm=1000;

#WMW test
w<-function(expr_case,expr_control,n_case,n_control){
  k=nrow(expr_case);
  E=n_case*(n_case+n_control+1)/2;
  Var=n_case*n_control*(n_case+n_control+1)/12
  p0=matrix(data=NA,nrow=1,ncol=k,dimnames = NULL)
  
  for (i in 1:k){
    r=rank(c(expr_case[i,],expr_control[i,]))
    WMW=sum(r[1:n_case])
    
    tar=table(r[1:n_case])
    tarf=as.data.frame(tar)
    if (length(tarf$freq<(n_case+n_control))){
      sum_t=0;
      for (j in 1:length(tarf$freq)){
        sum_t=sum_t+tarf$freq[j]*((tarf$freq[j])^2-1)
      }
      Var=Var-n_case*n_control*sum_t/12/(n_case+n_control-1)/(n_case+n_control)
    }
    
    p0[i]=pnorm((WMW-E)/sqrt(Var),0,1)
  }
  return (p0);
}




wmper<-function(expr_case,expr_control,expr_auxi,n_case,n_control,n_auxi){
  k=nrow(expr_case);
  W=NULL;
  for (i in 1:k){
    r1=rank(c(expr_case[i,],expr_auxi[i,]));
    r2=rank(c(expr_control[i,],expr_auxi[i,]));
    W1=sum(r1[1:n_case])/(n_case+n_auxi+1)
    W2=sum(r2[1:n_control])/(n_control+n_auxi+1)
    W[i]=W1-W2*n_case/n_control;
    
  }
  return (W);
}

#WMW-A test
WMWA<-function(expr,expr_case,expr_control,expr_auxi,ind,nperm){
  k=nrow(expr);
  W_permu=matrix(data=NA,nrow=nperm,ncol=k,dimnames = NULL)
  n_case=ncol(expr_case);
  n_control=ncol(expr_control);
  n_auxi=ncol(expr_auxi);
  p1=matrix(data=NA,nrow=1,ncol=k,dimnames = NULL)
  
  wp=wmper(expr_case,expr_control,expr_auxi,n_case,n_control,n_auxi);
  
  for (j in 1:nperm) {
    case.permu.ind=sample(ind,n_case)
    control.permu.ind=ind[!(ind %in% case.permu.ind)]
    permu_case=expr[,case.permu.ind]
    permu_control=expr[,control.permu.ind]
    W_permu[j,]=wmper(permu_case,permu_control,expr_auxi,n_case,n_control,n_auxi);
  }
  
  for (i in 1:k){
    p1[i]=sum(W_permu[,i]<wp[i])/nperm
  }
  
  return (p1);
}


p_0=NULL;
p_1=NULL;
p_2=NULL;
p_3=NULL;
for (k in 1:length(x)){
  k
  expr_case=x[[k]];expr_control=y[[k]];
  p_0[[k]]=w(expr_case,expr_control,n_case,n_control);
  for (j in 1:length(n_auxi)){
    j
    pd1=cbind(x[[k]],zx[[j]][[k]]);pd2=cbind(y[[k]],zy[[j]][[k]]);
    data=cbind(pd1,pd2);
    ind=c(1:n_case,(n_case+n_auxi[j]+1):(n_case+n_auxi[j]+n_control));
    for (i in 1:length(sca)){
      auxi1=pd1[,(n_case+1):(n_case+n_auxi[j]*sca[i])];
      auxi2=pd2[,(n_control+1):(n_control+n_auxi[j]*sca2[i])];
      expr_auxi=cbind(auxi1,auxi2);
      p_3[[i]]=WMWA(data,expr_case,expr_control,expr_auxi,ind,nperm)
    }
    p_1[[j]]=p_3
  }
  p_2[[k]]=p_1
}

save(p_0,p_2,file='result2.Rdata')







