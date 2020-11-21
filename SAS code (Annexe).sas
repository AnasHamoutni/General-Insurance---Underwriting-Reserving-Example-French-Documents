////Triangle///

data mydata;

infile "/folders/myfolders/trii.csv" firstobs=2 delimiter=";" truncover;

input CODUSA Zone_souscription Zone_Risque ANNDEV ANNSIN charge ; 
run;
proc sql;

create table mydata2 as

select ANNDEV,ANNSIN,sum(charge) as sumcharge from
mydata

group by ANNSIN,ANNDEV; quit;
data mydata3; set mydata2;
dev=ANNDEV-ANNSIN;

drop ANNDEV; run;


proc transpose data=mydata3 out=provss (drop=_name_); by ANNSIN;
 
var sumcharge; id dev;
run;

/////ChainLadder////

proc iml; use provss;
read all var _all_ into mat; 
cu=mat;
do j=3 to ncol(cu); cu[,j]=mat[,j]+cu[,(j-1)];
end;
do i=3 to ncol(cu); sub1=cu[1:(ncol(cu)-i+1),i];
sub2=cu[1:(ncol(cu)-i+1),(i-1)]; f=f||sum(sub1)/sum(sub2);
end;
do j=2 to 15;

do i=(17-j) to 15; cu[i,j+1]=cu[i,j]*f[1,j-1];
end;
end;

prov={0 0 0 0 0 0 0 0 0 0 0 0 0 0};

do i=2 to 15;

prov[i-1]=cu[i,16]-cu[i,(17-i)]; 
end;
prov=prov||sum(prov); print prov cu f;
quit;

/////LONDONCHAIN///





proc iml;
 
use provss;

read all var _all_ into mat; 

cu=mat;
do j=3 to ncol(cu); cu[,j]=mat[,j]+cu[,(j-1)]; 
end;
lambda={0 0 0 0 0 0 0 0 0 0 0 0 0 0};

beta={0 0 0 0 0 0 0 0 0 0 0 0 0 0};

do j=2 to 14;



lambda[1,j-1]=cov(cu[-(15:(16-j+1)),j+1]cu[-(15:(16-j+1)),j])/var(cu[-(15:(16-j+1)),j]);

beta[1,j-1]=mean(cu[-(15:(16-j+1)),j+1])-mean(cu[-(15:(16-j+1)),j])*lambda[1,j-1];
end;
lambda[1,14]=cu[1,16]/cu[1,15]; do j=2 to 15;
do i=(17-j) to 15; cu[i,j+1]=cu[i,j]*lambda[1,j-1]+beta[1,j-1];
end;
end;

prov={0 0 0 0 0 0 0 0 0 0 0 0 0 0};

do i=2 to 15;

prov[i-1]=cu[i,16]-cu[i,(17-i)];
end;
prov=prov||sum(prov);
print lambda beta prov cu ;
quit;
