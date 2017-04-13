clear all
close all

%%%%% original version of Azaele et al 2012
%%%%% modified by Reddy and Convertino 2015, 2016 versions 

%parameters (to be set up based on inferred b, D and tau)
tau=16.85;     %(day-1)     
D=0.7174;

beta=0.8065;
b=beta*D; 
c=0.5;

N=10000;    %number of realization
ndays=200;  %number of days of simulation

dt=0.1;     %time step of simulation (day)

%main
t=0:dt:ndays;  %time vector
x=-log(rand(N,1))/c;   %initial conditions
media=zeros(1,length(t));  %preallocation media vector
cum0=zeros(1,length(t));  %preallocation cum0 vector
int=zeros(N,1);
realvive=(x>0);
media(1)=mean(x);
cum0(1)=sum(realvive)/N;
for cont=2:length(t)
    x(realvive)=x(realvive)+(b-x(realvive)/tau)*dt+...
               ((D*x(realvive)).^0.5).*(-(2^0.5)*erfcinv(2*rand(sum(realvive),1)))*(2^0.5)*(dt)^0.5;%randn(sum(realvive),1)
    realvive=(x>0);
    x(~realvive)=0;
    media(cont)=mean(x);
    cum0(cont)=sum(realvive)/N;
    int=int+x;
end
load mediaIdit
load cumul_0
media_anal=(1/c)*exp(-t./tau).*(1+c*D*tau*(exp(t./tau)-1)).^(beta);
cum0_anal=(1+c*D*tau*(exp(t./tau)-1)).^(beta-1).*exp(-(c*exp(t./tau)*0)./(1+c*D*tau*(exp(t./tau)-1)));
plot(t,media,t,media_anal,0:1:409,mediaIdit,'r.')
figure
plot(t,cum0,t,cum0_anal,0:1:409,cumul,'r.')

cumcasi_lang=int/(5/dt);
cumcasi_lang(cumcasi_lang<1)=[];
load cumcasi
figure
dati=cumcasi;
ndati=length(dati);
dati=sort(dati,'descend');
h=loglog(dati,(1:ndati)./(ndati),'co');
set(h,'displayname','cases langevin')
ylabel('P[X>x]','fontsize',16)
xlabel('x','fontsize',16)
set(gca,'Ylim',[0.0001 1],'Xlim',[1 10000],'fontsize',12)
hold on

dati=cumcasi_lang;
ndati=length(dati);
dati=sort(dati,'descend');
h=loglog(dati,(1:ndati)./(ndati),'-b','linewidth',2);
set(h,'displayname','cases langevin')

figure

dati=cumcasi;
ndati=length(dati);
dati=sort(dati,'descend');
h=semilogx(dati,(1:ndati)./(ndati),'co');
set(h,'displayname','cases langevin')
ylabel('P[X>x]','fontsize',16)
xlabel('x','fontsize',16)
set(gca,'Ylim',[0 1.2],'Xlim',[1 10000],'fontsize',12)
hold on

dati=cumcasi_lang;
ndati=length(dati);
dati=sort(dati,'descend');
h=semilogx(dati,(1:ndati)./(ndati),'-b','linewidth',2);
set(h,'displayname','cases langevin')



