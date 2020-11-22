#TRIANGLE
  
  library(sqldf) 
  library(ChainLadder) 
  s=read.csv2("trii.csv")
  
  data=sqldf("SELECT ANNSIN,ANNDEV,sum(charge)as charg FROM s GROUP BY ANNSIN,ANNDEV")
  data$ANNDEV= data$ANNDEV- data$ANNSIN
  names(data)=c("origin","devellopement","cumcharge")
  
  charg <- as.triangle(data, origin="origin", dev="devellopement", value="cumcharge") 
  charg=incr2cum(charg)
  
  
#Chain ladder
    
    
    
    lambda=rep(1,14)
    for(k in 1:14){
      lambda[k]=sum(charg[1:(15-k),k+1])/sum(charg[1:(15-k),k])
      
    }
  
  M=charg
  for(j in 1:14){
    for(i in (16-j):15){
      
      M[i,j+1]=M[i,j]*lambda[j]}
    
  }
  
  
  
  prov=0
  
  for(i in 2:15){ prov[i]=M[i,15]-M[i,(16-i)]
  }
  
  sum(prov)
  
#charge ultime
    
  ef=log(lambda-1)
  ez=1:14
  rego=lm(ef~ez) 
  u=seq(15,30)
  vf=predict(rego,newdata=data.frame(ez=u)) factor=prod(1+exp(vf)) chargeult=M[,15]*factor paiement=diag(M[,15:1]) 
  provision=chargeult-paiement
  sum(provision)
  
  //LONDON CHAIN///
    
  lambda=rep(0,14)
  beta=rep(0,14)
  
  for(j in 1:13)
    
  {
    
    lambda[j]=cov(charg[-(15:(16-j)),j+1],charg[-(15:(16-j)),j])/var(charg[-(15:(16-j)),j])
    
    beta[j]=mean(charg[-(15:(16-j)),j+1])-mean(charg[-(15:(16-j)),j])*lambda[j]
    
  }
  
  lambda[14]=charg[-(15:2),15]/charg[-(15:2),14]
  
  
  
  
  M=charg for(j in 1:14){
    for(i in (16-j):15){ M[i,j+1]=M[i,j]*lambda[j]+beta[j]
    }
    
  }
  
  
  prov=0
  
  for(i in 2:15){ prov[i]=M[i,15]-M[i,(16-i)]
  }
  
  sum(prov)
  
#LONDON PIVOT
    
    
    
    a=rep(0,21)
    c=matrix(0,nrow=100,ncol=14) 
    n=matrix(0,nrow=100,ncol=14)
    m=matrix(0,nrow=100,ncol=14)
  
  
  
  
  for(l in 1:20){
    
    for(j in 1:13){
      
      
      
      c[l,j]=cov(charg[-(15:(16-j)),j+1]+a[l],charg[-(15:(16-j)),j]+a[l])/var(charg[-(15:(16-j)),j]+a[l])
      
      
      
    }
    
    c[l,14]=(charg[1,15]+a[l])/(charg[1,14]+a[l])
    
    
    
    for(j in 1:14){
      
      n[l,j]=(1-c[l,j])*(sum(charg[-(15:(16-j)),j+1])-c[l,j]*sum(charg[-(15:(16-j)),j])) m[l,j]=(15-j)*(c[l,j]-1)*(c[l,j]-1)
    }
    
    
    
    
    
    a[l+1]=sum(n[l,])/sum(m[l,])
    
    
    
    
    
    
  }
  
  lambda=rep(1,14) for(k in 1:14){
    lambda[k]=sum(charg[1:(15-k),k+1]+a[21])/sum(charg[1:(15-k),k]+a[21])
    
  }
  
  lambda
  
  
  
  
  
  M=charg for(j in 1:14){
    for(i in (16-j):15){ M[i,j+1]=(M[i,j]+a[21])*lambda[j]-a[21]}
  }
  
  prov=0
  
  for(i in 2:15){ prov[i]=M[i,15]-M[i,(16-i)]
  }
  
  sum(prov)
  
#MakCHAINLADDER
    
  M<-MackChainLadder(charg,est.sigma = "Mack")
  
  M$f ef=log(M$f-1)
  ez=1:14
  
  rego=lm(ef[-15]~ez) 
  u=seq(15,30)
  
  vf=predict(rego,newdata=data.frame(ez=u)) factor=prod(1+exp(vf)) chargeult=M$FullTriangle[,15]*factor paiement=diag(charg[,15:1]) 
  
  provision=chargeult-paiement 
  
  sum(provision)
  
#GLM#LOGNORMALE
    
  ligne<- rep(1:15,15) 
  
  colonne<-rep(1:15,each=15) 
  
  YY=cum2incr(charg)
  
  YY
  
  Y<-as.vector(YY) 
  lig<-as.factor(ligne)
  
  col<-as.factor(colonne) cbind(Y,lig,col)
  
  reg<-lm(log(Y)~lig+col) summary(reg)
  
  z=predict(reg,newdata=data.frame(lig,col),type="response") z
  
  sigma<-summary(reg)$sigma 
  
  sigma 
  
  prediy=exp(z+sigma^2/2) 
  
  prediy 
  
  sum(prediy[is.na(Y)==TRUE])
  
  
#GLM#GAMMA
  
  
  
library(glm2) gam=glm(Y~lig+col,family=Gamma(link="log"))
  
summary(gam)
  
z=predict(gam,newdata=data.frame(lig,col),type="response") 
  
z
  
sum(z[is.na(Y)==TRUE])
  

    
