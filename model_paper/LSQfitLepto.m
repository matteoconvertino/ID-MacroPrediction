c=1/23;
xWeeksFiner=0:.1:10;
xWeeksa=0:10;

xx(1) = 0; %b
xx(2) = 0; %D
xx(3) = 0; %tau

F = @(xx,xxdata)(exp(-xxdata/xx(3))/c).*(1+c*xx(2).*xx(3).*(exp(xxdata/xx(3))-1)).^(xx(1)/xx(2));

xx0=[58 104 1.1];

[xx,resnorm,~,exitflag,output] = lsqcurvefit(F,xx0,xWeeksa,AP2011atest,[],[],options)

figure 
plot(xWeeksa,AP2011atest,':*')
title('Apura2011')
xlabel('Weeks')
ylabel('Infected')

hold on
plot(xWeeksFiner,F(xx,xWeeksFiner))
hold off

