function [sse]=logitmatch2(param,X,Y,Nd,W)

U = X*param;

p = exp(U)./(1+exp(U));

avp=zeros(6,1);

avp(1)=mean(p(1:5*Nd));
avp(2)=mean(p(5*Nd+1:6*Nd));
avp(3)=mean(p(6*Nd+1:7*Nd));
avp(4)=mean(p(7*Nd+1:8*Nd));
avp(5)=mean(p(8*Nd+1:9*Nd));
avp(6)=mean(p(9*Nd+1:end));

sse=sum(W.*(100*(Y-avp)).^2);

