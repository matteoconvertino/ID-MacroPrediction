%Fits I(t) to data using least squares optimization

peakData=SLW0914(:); %Peak chosen to fit. Get from SLW0914.mat. 
          

c=1/23; %c = 1/I(0)
xWeeksFiner=0:.1:10; %varibable for plots
xWeeksa=0:10;

xx(1) = 0; %b
xx(2) = 0; %D
xx(3) = 0; %tau

% Incidence function
F = @(xx,xxdata)(exp(-xxdata/xx(3))/c).*(1+c*xx(2).*xx(3).*(exp(xxdata/xx(3))-1)).^(xx(1)/xx(2));

xx0=[58 104 1.1]; %Inital values for optimization

[xx,resnorm,~,exitflag,output] = lsqcurvefit(F,xx0,xWeeksa,peakData,[],[],options);

figure 
plot(xWeeksa,peakData,':*') % plot icidence data for peak
title('')
xlabel('Weeks')
ylabel('Infected')

hold on
plot(xWeeksFiner,F(xx,xWeeksFiner)) % plot resulting best fit
hold off

