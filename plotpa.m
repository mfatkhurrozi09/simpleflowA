clear all; close all; clc

load fielda.txt;
phi=fielda;

dx=0.1; dy=0.1;
nx=18; ny=18;
x=1:1:nx; y=1:1:ny;
[x,y]=meshgrid(x,y);


for i=2:nx-2
    for j=2:ny-2
        u(i,j)=0.;
        v(i,j)=0.;
    end
end

for i=2:nx-2
    for j=2:ny-2
        m=i-1;
        n=j-1;
        u(m,n)=-(phi(i,j+1)-phi(i,j))/(dx);
        v(m,n)=-(phi(i+1,j)-phi(i,j))/(dy);
    end
end

for i=2:nx-1
    for j=2:ny-1
        m=i-1;
        n=j-1;
        xx(m,n)=i-1; 
        yy(m,n)=j-1;
    end
end

quiver(xx,yy,u,v,0)
figure(2)
surf(x,y,phi); title('medan potensial \phi');  
view(25,60); colorbar; 

%shading interp; 
