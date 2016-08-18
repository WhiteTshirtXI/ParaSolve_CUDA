clear all
%close all
clc

T = load('Parasolve.dat');
Tsat = 373.15;

M = 12;

Tnew = T - Tsat;
x = linspace(.001,0,M);

figure
hold on
grid on
plot(x,Tnew,'-r')
xlabel('x')
ylabel('T')

Twall = 383.15;
Tsat = 373.15;

lam_v = .025;
cp_v = 2030;
rho_v = 0.597;

L = 2.26 * (10^6);

Nx = 12;
Nt = 11;

x = linspace(0,0.001,Nx);
t = linspace(0,0.1,Nt);

t = t';

T = zeros(Nt,Nx);
X = zeros(Nt,1);

alpha_v = (lam_v)/(cp_v*rho_v);

syms X_sol;

eqn = X_sol*exp(X_sol^2)*erf(X_sol) == (cp_v*(Twall-Tsat))/(sqrt(pi)*L);

SolnX = solve(eqn,X_sol);

%X = sqrt(t.*alpha_v).*(2*SolnX);

for i=1:Nt

    X(i) = 2*SolnX*sqrt(alpha_v*t(i));
    T(i,:) = Twall+((Tsat - Twall)/(erf(SolnX)))*(erf(x./(2*sqrt(alpha_v*t(i)))));

end

Xx = X(2:end);
Tx = (T(2:end,:)-Tsat);
I = find(Tx<=0);
Tx(I) = 0;

T_an = Tx(end,:)';
T_an = flipud(T_an);

Er = T_an - Tnew;