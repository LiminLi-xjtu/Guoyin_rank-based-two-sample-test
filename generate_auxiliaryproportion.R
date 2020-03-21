
rm(list = ls())
num=100;
x<- NULL;
y<- NULL;
z_zx<- NULL;
z_zy<- NULL;
zx<- NULL;
zy<- NULL;

m1=50;m2=50;#m1 is number of true hypotheses, m2 is number of false hypotheses
n1=30;n2=60;#n1 is number of case samples, n2 is number of control samples
sigma=1;mu1=0;mu2=0.7;


#generate data: dimension of x~N(0,1) is (m1+m2)*n1, dimension of y~N(0.7,1) is (m1+m2)*n2
#repeat the procedure num times
for (k in 1:num) {
  
  x_temp1=matrix(data=NA,nrow=m1+m2,ncol=n1,dimnames = NULL)
  y_temp1=matrix(data=NA,nrow=m1+m2,ncol=n2,dimnames = NULL)
  
  for (i in 1:(m1+m2)){
    x_temp1[i,]=rnorm(n1,mu1,sigma);
  }
  for (j in 1:m1){
    y_temp1[j,]=rnorm(n2,mu1,sigma);
  }
  for (j in (m1+1):(m1+m2)){
    y_temp1[j,]=rnorm(n2,mu2,sigma);
  }
  
  x[[k]]=x_temp1;y[[k]]=y_temp1;
  
}


n_auxi=300;#number of auxiliary samples
for (s in 1:length(n_auxi)){
  
  x_temp2=matrix(data=NA,nrow=m1+m2,ncol=n_auxi[s],dimnames = NULL)
  y_temp2=matrix(data=NA,nrow=m1+m2,ncol=n_auxi[s],dimnames = NULL)
  
  for (k in 1:num){
    for (i in 1:(m1+m2)){
      x_temp2[i,]=rnorm(n_auxi[s],mu1,sigma);
    }
    for (j in 1:m1){
      y_temp2[j,]=rnorm(n_auxi[s],mu1,sigma);
    }
    for (j in (m1+1):(m1+m2)){
      y_temp2[j,]=rnorm(n_auxi[s],mu2,sigma);
    }
    z_zx[[k]]=x_temp2;z_zy[[k]]=y_temp2;
  }
  zx[[s]]=z_zx;zy[[s]]=z_zy;
}


save(x,y,zx,zy,n_auxi,file='nm2.Rdata')

