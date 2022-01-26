%Normal distribution for latent index
clear
addpath Functions/
rng('default');

N=411400; %number of applicants
Nd=41140; %number in each decile
error=log(-log(rand(N,1)))-log(-log(rand(N,1)));
o1=optimoptions('fminunc','OptimalityTolerance',1e-12,'StepTolerance',1e-12);
%o1=optimoptions('fminunc','OptimalityTolerance',1e-08);
%moments to match: asian american admit rates by decile
Y=[.0004;.0032;.0077;.0203;.0701;.4168];
W=[.5;.1*ones(5,1)];

%drawing the component of the observable index 

X1=randn(N,1);
X1=sortrows(X1,1);

%Case 1: Normal distributed index

X=[ones(N,1) X1];

[b1,like1]=fminunc('logitmatch2',[-3;0],o1,X,Y,Nd,W);
[b1,like1]=fminunc('logitmatch2',b1,o1,X,Y,Nd,W);

Xpred1=X*b1;
Xtot1=Xpred1+error;
Admit1=Xtot1>0;
R1=var(Xpred1)./var(Xtot1);
Pd1=plogitmatch2(b1,X,Y,Nd,W);

%Case 2: Normal distributed index plus spline

X=[ones(N,1) X1 (X1>0).*(X1.^2) (X1<0).*(X1.^2)];

[b2,like2]=fminunc('logitmatch2',[-3;0;0;0],[],X,Y,Nd,W);
[b2,like2]=fminunc('logitmatch2',b2,[],X,Y,Nd,W);

Xpred2=X*b2;
Xtot2=Xpred2+error;
Admit2=Xtot2>0;
R2=var(Xpred2)./var(Xtot2);
Pd2=plogitmatch2(b2,X,Y,Nd,W);

%Case 3: Normal distributed index plus spline plus log normal 

X=[ones(N,1) X1 (X1>0).*(X1.^2) (X1<0).*(X1.^2) exp(X1)];

[b3,like3]=fminunc('logitmatch2',[-3;0;0;0;0],[],X,Y,Nd,W);
[b3,like3]=fminunc('logitmatch2',b3,[],X,Y,Nd,W);

Xpred3=X*b3;
Xtot3=Xpred3+error;
Admit3=Xtot3>0;
R3=var(Xpred3)./var(Xtot3);
Pd3=plogitmatch2(b3,X,Y,Nd,W);

[b1a,like1a]=fminunc('logit',[-6],[],[ones(N,1)],Admit1);
[b2a,like2a]=fminunc('logit',[-6],[],[ones(N,1)],Admit2);
[b3a,like3a]=fminunc('logit',[-6],[],[ones(N,1)],Admit3);

[b1b,like1b]=fminunc('logit',[-6;0],[],[ones(N,1) Xpred1],Admit1);
[b2b,like2b]=fminunc('logit',[-6;0],[],[ones(N,1) Xpred2],Admit2);
[b3b,like3b]=fminunc('logit',[-6;0],[],[ones(N,1) Xpred3],Admit3);

PseudoR=1-[like1b;like2b;like3b]./[like1a;like2a;like3a];
R=[R1;R2;R3];
Pd=[Pd1 Pd2 Pd3];

save ../../Data/asianadmitdecile b1 b2 b3 like1 like2 like3 b1a b2a b3a like1a like2a like3a  b1b b2b b3b like1b like2b like3b R PseudoR Y W 

