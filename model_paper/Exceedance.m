function [ eXprob ] = Exceedance(X)
%Returns Exceedance probabililty in column 2. Weekly Cases in column 2.


totalcount=length(X);
tbl=tabulate(X);
tbl(tbl(:, 2) == 0, :) = []; %removes zeros
tblL = size(tbl,1);
prob=zeros(tblL,2);
n=tblL;
exNum=0;
for i=1:tblL
    exNum=tbl(n,2)+exNum;
    prob(n,1)=tbl(n);
    prob(n,2)=exNum/totalcount;
    %prob(n,2)=exNum/(sum(tbl(:,2))-tbl(1,2));
    n=n-1;
end
eXp1=sort(prob(1:tblL,1),'descend');
eXp2=sort(prob(1:tblL,2),'ascend');
eXprob=[eXp1 eXp2];


end

