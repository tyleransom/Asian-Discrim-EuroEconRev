function [ like,grad ] = logit( param,X,d )

%This function can be used to estimate binary logit models 

U = X*param;

% P = exp(U)./(1+exp(U));
% like_i = (P.^d).*((1-P).^(1-d)); %For checking code in logit_like

like = sum( log(1+exp(U)) - d.*U ); %Negative log likelihood

if nargout>1
    
    %Gradient of negative log likelihood
    grad = sum( bsxfun(@times,( (exp(U)./(1+exp(U))) - d ), X) ,1)';

end

end

