clear all
%close all
clc

x_min = -0.5;
x_max = 0.5;

y_min = -0.5;
y_max = 0.5;

Nx = 200; % Nxb * x_procs
Ny = 200; % Nyb * y_procs

x_Grid = linspace(x_min,x_max,Nx);
x_grid = (x_Grid(1:end-1)+x_Grid(2:end))/2;
%x_grid = x_Grid;


y_Grid = linspace(y_min,y_max,Ny);
y_grid = y_Grid;
%y_grid = (y_Grid(1:end-1)+y_Grid(2:end))/2;


x_cell = (x_grid(1:end-1) + x_grid(2:end))/2;
y_cell = (y_grid(1:end-1) + y_grid(2:end))/2;

[XG,YG] = meshgrid(x_Grid,y_Grid);
[XC,YC] = meshgrid(x_cell,y_cell);

ZG = zeros(size(XG));
ZC = zeros(size(XC));

% Divide Geometry into four segments

x_c = 0.;
y_c = 0.;

r = 0.1;

x_up_1 = linspace(r,0,100);
y_up_1 = sqrt(r^2-x_up_1.^2);

x_up_2 = linspace(0,-r,100);
y_up_2 = sqrt(r^2-x_up_2.^2);

x_lo_1 = fliplr(x_up_2(1:end));
y_lo_1 = -fliplr(y_up_2(1:end));

x_lo_2 = fliplr(x_up_1(1:end));
y_lo_2 = -fliplr(y_up_1(1:end));

x_bod = [x_up_1,x_up_2,x_lo_1,x_lo_2];
y_bod = [y_up_1,y_up_2,y_lo_1,y_lo_2];

% Calculate where the body cuts the grid
k = 0;

for i=1:length(x_up_1)-1   
    for j=1:min(length(x_grid),length(y_grid))
        
        if(x_grid(j) >= min(x_up_1(i),x_up_1(i+1)) && ...
                x_grid(j) <= max(x_up_1(i),x_up_1(i+1)))
            
            k = k+1;
            
            x_bod_grid_up_1(k) = x_grid(j);
            y_bod_grid_up_1(k) = y_up_1(i+1) + (x_bod_grid_up_1(k) - x_up_1(i+1))*...
                            ((y_up_1(i+1)-y_up_1(i))/(x_up_1(i+1)-x_up_1(i)));
                       
        end
        
        if(y_grid(j) >= min(y_up_1(i),y_up_1(i+1)) && ...
                y_grid(j) <= max(y_up_1(i),y_up_1(i+1)))
            
            k = k+1;
            
            y_bod_grid_up_1(k) = y_grid(j);
            x_bod_grid_up_1(k) = x_up_1(i+1) + (y_bod_grid_up_1(k) - y_up_1(i+1))*...
                            ((x_up_1(i+1)-x_up_1(i))/(y_up_1(i+1)-y_up_1(i)));
                        
        end         
    end        
end

k = 0;

for i=1:length(x_up_2)-1
    for j=1:min(length(x_grid),length(y_grid))
        
        if(x_grid(j) >= min(x_up_2(i),x_up_2(i+1)) && ...
                x_grid(j) <= max(x_up_2(i),x_up_2(i+1)))
            
            k = k+1;
            
            x_bod_grid_up_2(k) = x_grid(j);
            y_bod_grid_up_2(k) = y_up_2(i+1) + (x_bod_grid_up_2(k) - x_up_2(i+1))*...
                            ((y_up_2(i+1)-y_up_2(i))/(x_up_2(i+1)-x_up_2(i)));
                       
        end
        
        if(y_grid(j) >= min(y_up_2(i),y_up_2(i+1)) && ...
                y_grid(j) <= max(y_up_2(i),y_up_2(i+1)))
            
            k = k+1;
            
            y_bod_grid_up_2(k) = y_grid(j);
            x_bod_grid_up_2(k) = x_up_2(i+1) + (y_bod_grid_up_2(k) - y_up_2(i+1))*...
                            ((x_up_2(i+1)-x_up_2(i))/(y_up_2(i+1)-y_up_2(i)));
                       
        end
    end        
end

k = 0;

for i=1:length(x_lo_1)-1    
    for j=1:min(length(x_grid),length(y_grid))
        
        if(x_grid(j) >= min(x_lo_1(i),x_lo_1(i+1)) && ...
                x_grid(j) <= max(x_lo_1(i),x_lo_1(i+1)))
            
            k = k+1;
            
            x_bod_grid_lo_1(k) = x_grid(j);
            y_bod_grid_lo_1(k) = y_lo_1(i+1) + (x_bod_grid_lo_1(k) - x_lo_1(i+1))*...
                            ((y_lo_1(i+1)-y_lo_1(i))/(x_lo_1(i+1)-x_lo_1(i)));
                       
        end
        
        if(y_grid(j) >= min(y_lo_1(i),y_lo_1(i+1)) && ...
                y_grid(j) <= max(y_lo_1(i),y_lo_1(i+1)))
            
            k = k+1;
            
            y_bod_grid_lo_1(k) = y_grid(j);
            x_bod_grid_lo_1(k) = x_lo_1(i+1) + (y_bod_grid_lo_1(k) - y_lo_1(i+1))*...
                            ((x_lo_1(i+1)-x_lo_1(i))/(y_lo_1(i+1)-y_lo_1(i)));
                       
        end           
    end
end

k = 0;

for i=1:length(x_lo_2)-1   
    for j=1:min(length(x_grid),length(y_grid))
        
        if(x_grid(j) >= min(x_lo_2(i),x_lo_2(i+1)) && ...
                x_grid(j) <= max(x_lo_2(i),x_lo_2(i+1)))
            
            k = k+1;
            
            x_bod_grid_lo_2(k) = x_grid(j);
            y_bod_grid_lo_2(k) = y_lo_2(i+1) + (x_bod_grid_lo_2(k) - x_lo_2(i+1))*...
                            ((y_lo_2(i+1)-y_lo_2(i))/(x_lo_2(i+1)-x_lo_2(i)));
                       
        end
        
        if(y_grid(j) >= min(y_lo_2(i),y_lo_2(i+1)) && ...
                y_grid(j) <= max(y_lo_2(i),y_lo_2(i+1)))
            
            k = k+1;
            
            y_bod_grid_lo_2(k) = y_grid(j);
            x_bod_grid_lo_2(k) = x_lo_2(i+1) + (y_bod_grid_lo_2(k) - y_lo_2(i+1))*...
                            ((x_lo_2(i+1)-x_lo_2(i))/(y_lo_2(i+1)-y_lo_2(i)));
                       
        end        
    end       
end

xy = unique([x_bod_grid_up_1',y_bod_grid_up_1'],'rows');
x_bod_grid_up_1 = xy(:,1);
y_bod_grid_up_1 = xy(:,2);

xy = unique([x_bod_grid_lo_1',y_bod_grid_lo_1'],'rows');
x_bod_grid_lo_1 = xy(:,1);
y_bod_grid_lo_1 = xy(:,2);

xy = unique([x_bod_grid_up_2',y_bod_grid_up_2'],'rows');
x_bod_grid_up_2 = xy(:,1);
y_bod_grid_up_2 = xy(:,2);

xy = unique([x_bod_grid_lo_2',y_bod_grid_lo_2'],'rows');
x_bod_grid_lo_2 = xy(:,1);
y_bod_grid_lo_2 = xy(:,2);

% Calculate control points in fluid and inside body
m=0;

for i=1:length(x_bod_grid_up_1)   
    
    for j=1:length(x_grid)
       for k=1:length(y_grid)-1
        if(x_grid(j) == x_bod_grid_up_1(i) && y_bod_grid_up_1(i) >= y_grid(k) && y_bod_grid_up_1(i) <= y_grid(k+1))
               
            m=m+1;
            x_fluid(m) = x_grid(j);
            y_fluid(m) = y_grid(k+1);
            
            x_body(m) = x_grid(j);
            y_body(m) = y_grid(k);
         
        end          
       end
    end
    
    for j=1:length(y_grid)
       for k=1:length(x_grid)-1      
        if(y_grid(j) == y_bod_grid_up_1(i) && x_bod_grid_up_1(i) >= x_grid(k) && x_bod_grid_up_1(i) <= x_grid(k+1))
                  
            m=m+1;
            y_fluid(m) = y_grid(j);
            x_fluid(m) = x_grid(k+1);
            
            y_body(m) = y_grid(j);
            x_body(m) = x_grid(k);
            
        end      
       end
    end
end

for i=1:length(x_bod_grid_up_2)   
    
    for j=1:length(x_grid)
       for k=1:length(y_grid)-1
        if(x_grid(j) == x_bod_grid_up_2(i) && y_bod_grid_up_2(i) >= y_grid(k) && y_bod_grid_up_2(i) <= y_grid(k+1))
               
            m=m+1;
            x_fluid(m) = x_grid(j);
            y_fluid(m) = y_grid(k+1);
            
            x_body(m) = x_grid(j);
            y_body(m) = y_grid(k);
         
        end          
       end
    end
    
    for j=1:length(y_grid)
       for k=1:length(x_grid)-1      
        if(y_grid(j) == y_bod_grid_up_2(i) && x_bod_grid_up_2(i) >= x_grid(k) && x_bod_grid_up_2(i) <= x_grid(k+1))
                  
            m=m+1;
            y_fluid(m) = y_grid(j);
            x_fluid(m) = x_grid(k);
            
            y_body(m) = y_grid(j);
            x_body(m) = x_grid(k+1);
            
        end      
       end
    end
end

for i=1:length(x_bod_grid_lo_1)   
    
    for j=1:length(x_grid)
       for k=1:length(y_grid)-1
        if(x_grid(j) == x_bod_grid_lo_1(i) && y_bod_grid_lo_1(i) >= y_grid(k) && y_bod_grid_lo_1(i) <= y_grid(k+1))
               
            m=m+1;
            x_fluid(m) = x_grid(j);
            y_fluid(m) = y_grid(k);
            
            x_body(m) = x_grid(j);
            y_body(m) = y_grid(k+1);
         
        end          
       end
    end
    
    for j=1:length(y_grid)
       for k=1:length(x_grid)-1      
        if(y_grid(j) == y_bod_grid_lo_1(i) && x_bod_grid_lo_1(i) >= x_grid(k) && x_bod_grid_lo_1(i) <= x_grid(k+1))
                  
            m=m+1;
            y_fluid(m) = y_grid(j);
            x_fluid(m) = x_grid(k);
            
            y_body(m) = y_grid(j);
            x_body(m) = x_grid(k+1);
            
        end      
       end
    end
end

for i=1:length(x_bod_grid_lo_2)   
    
    for j=1:length(x_grid)
       for k=1:length(y_grid)-1
        if(x_grid(j) == x_bod_grid_lo_2(i) && y_bod_grid_lo_2(i) >= y_grid(k) && y_bod_grid_lo_2(i) <= y_grid(k+1))
               
            m=m+1;
            x_fluid(m) = x_grid(j);
            y_fluid(m) = y_grid(k);
            
            x_body(m) = x_grid(j);
            y_body(m) = y_grid(k+1);
         
        end          
       end
    end
    
    for j=1:length(y_grid)
       for k=1:length(x_grid)-1      
        if(y_grid(j) == y_bod_grid_lo_2(i) && x_bod_grid_lo_2(i) >= x_grid(k) && x_bod_grid_lo_2(i) <= x_grid(k+1))
                  
            m=m+1;
            y_fluid(m) = y_grid(j);
            x_fluid(m) = x_grid(k+1);
            
            y_body(m) = y_grid(j);
            x_body(m) = x_grid(k);
            
        end      
       end
    end
end

xy_fluid = unique([x_fluid',y_fluid'],'rows');

x_fluidnew = xy_fluid(:,1);
y_fluidnew = xy_fluid(:,2);

figure
hold on
mesh(XG,YG,ZG)
%for i=1:length(XC)
%    scatter(XC(i,:),YC(i,:),'.m')
%end
%plot(x_bod_grid,y_bod_grid,'b')
%plot(x_fluid,y_fluid,'om')
plot(x_bod,y_bod,'-r')
plot(x_fluidnew,y_fluidnew,'xk')
axis('equal')
