%Normal distribution for latent index
clear
addpath Functions/
rng('default');
ar=.0545; %admit rate to match
N=128422; %number of applicants
Nd=12842; %number in each decile
nadmit=round(ar*N); %number of admits
id=[1:N]';
A2=1.4161;

%setting the latext index to be normally distributed
X1=ones(N,1);
X2=randn(N,1);

%observable index absent constant term
%constant term is not relevant because it automatically adjusts
index=X2*A2;

%drawing logistic errors as the difference between two extreme value errors
error=log(-log(rand(N,1)))-log(-log(rand(N,1)));

%calculating latent index
lindex=index+error;

%admitting those who have the highest latent indexes so it matches the
%nubmer of admits

temp=[lindex id];
temp=sortrows(temp,-1);
admit=zeros(N,1);
admit(1:nadmit)=1;
temp=[admit temp];
temp=sortrows(temp,3);
Y=temp(:,1);

admitrate=mean(Y)

%getting the likelihood controlling only for a constant
[b1,like1]=fminunc('logit',-3,[],X1,Y);

%getting the likelihood controlling for both a constant and the observable
%index
[b2,like2]=fminunc('logit',[-3;1],[],[X1 X2],Y);

%calculating mcfaddens pseudo r2
pseudor2=1-(like2./like1)

%calculating the r2 of the observable index
[b3,b3int,r,rint,stats]=regress(lindex,[X1 X2]);

rsquare=stats(1)

%calculating predicted admits based on observables alone and admit probabilities

temp=[index id X1 X2];
temp=sortrows(temp,-1);
padmit=zeros(N,1);
padmit(1:nadmit)=1;
padmit2=exp(temp(:,3:4)*b2)./(1+exp(temp(:,3:4)*b2));

%mean admissions probablities by deciles of the admissions index
deciles=zeros(6,1);
deciles(6)=mean(padmit2(1:Nd));
deciles(5)=mean(padmit2(Nd+1:2*Nd));
deciles(4)=mean(padmit2(2*Nd+1:3*Nd));
deciles(3)=mean(padmit2(3*Nd+1:4*Nd));
deciles(2)=mean(padmit2(4*Nd+1:5*Nd));
deciles(1)=mean(padmit2(5*Nd+1:N));


temp=[padmit temp(:,1:2) padmit2];
temp=sortrows(temp,3);
padmit2=temp(:,4);

deciles

%accuracy
accuracy=zeros(3,1);
accuracy(1)=mean(temp(:,1)==Y);
accuracy(2)=mean(temp(Y==1,1)==Y(Y==1));
accuracy(3)=mean(temp(Y==0,1)==Y(Y==0));
accuracy

fullresults=[rsquare;deciles;accuracy;admitrate;pseudor2;A2]

save ../../Data/fullresults2esp fullresults


