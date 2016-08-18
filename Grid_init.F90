subroutine Grid_init()
           
     use Grid_data
     use physicaldata
     use MPI_data

#include "Solver.h"

     implicit none

     integer :: I
     real :: pi=4.0*atan(1.0)

     gr_Lx = (D_xmax)-(D_xmin)
     gr_Ly = (D_ymax)-(D_ymin)

     gr_Lx = gr_Lx/HK
     gr_Ly = gr_Ly/HK

     gr_dx = gr_Lx/Nxb
     gr_dy = gr_Ly/Nyb

     allocate(gr_dx_centers(Nxb+1,Nyb+1))
     allocate(gr_dy_centers(Nxb+1,Nyb+1))

     allocate(gr_dx_nodes(Nxb+2,Nyb+2))
     allocate(gr_dy_nodes(Nxb+2,Nyb+2))
     
     allocate(gr_x(Nxb+1,Nyb+1))
     allocate(gr_y(Nxb+1,Nyb+1))

     allocate(ph_center(CENT_VAR,Nxb+2,Nyb+2))
     allocate(ph_facex(FACE_VAR,Nxb+2,Nyb+2))
     allocate(ph_facey(FACE_VAR,Nxb+2,Nyb+2))

#ifdef SOLVER_GRID_UG

     do i=1,Nyb+1
        gr_x(:,i)=D_xmin+mod(myid,HK)*gr_Lx+gr_dx*(/(I,I=0,Nxb)/)
     enddo

     do i=1,Nxb+1
        gr_y(i,:)=D_ymin+(myid/HK)*gr_Ly+gr_dy*(/(I,I=0,Nyb)/)
    enddo
   

#endif

#ifdef SOLVER_GRID_NON_UG

     do i=1,Nyb+1

        if (mod(myid,HK) == 0) then
           gr_x(:,i)=D_xmin+mod(myid,HK)*gr_Lx+gr_Lx*(1-cos((pi/(2*Nxb))*(/(I,I=0,Nxb)/)))
       
        else if (mod(myid,HK) == HK-1) then
           gr_x(:,i)=D_xmin+mod(myid,HK)*gr_Lx+gr_Lx*(cos((pi/(2*Nxb))*(/(I,I=Nxb,0,-1)/)))
       
        else
           !x(:,i)=D_xmin+mod(myid,HK)*Lx+dx*(/(I,I=0,Nxb)/)
          
           gr_x(1:Nxb/2+1,i)=D_xmin+mod(myid,HK)*gr_Lx+gr_Lx*(cos((pi/(Nxb))*(/(I,I=Nxb/2,0,-1)/)))/2
          
           gr_x(Nxb/2+2:Nxb+1,i)=D_xmin+mod(myid,HK)*gr_Lx+gr_Lx/2+&
                              gr_Lx*(1-cos((pi/(Nxb))*(/(I,I=1,Nxb/2)/)))/2

        end if

      enddo

     do i=1,Nxb+1

        if (myid/HK == 0) then
            gr_y(i,:)=D_ymin+(myid/HK)*gr_Ly+gr_Ly*(1-cos((pi/(2*Nyb))*(/(I,I=0,Nyb)/)))
   
        else if (myid/HK == HK-1) then
            gr_y(i,:)=D_ymin+(myid/HK)*gr_Ly+gr_Ly*(cos((pi/(2*Nyb))*(/(I,I=Nyb,0,-1)/)))

        else
          !y(i,:)=D_ymin+(myid/HK)*Ly+dy*(/(I,I=0,Nyb)/)
          
          gr_y(i,1:Nyb/2+1)=D_ymin+(myid/HK)*gr_Ly+gr_Ly*(cos((pi/(Nyb))*(/(I,I=Nyb/2,0,-1)/)))/2
          
          gr_y(i,Nyb/2+2:Nyb+1)=D_ymin+(myid/HK)*gr_Ly+gr_Ly/2+&
                               gr_Ly*(1-cos((pi/(Nyb))*(/(I,I=1,Nyb/2)/)))/2
        end if

     enddo

#endif

gr_dx_nodes(2:Nxb+1,1:Nyb+1) = gr_x(2:Nxb+1,:)-gr_x(1:Nxb,:)
gr_dy_nodes(1:Nxb+1,2:Nyb+1) = gr_y(:,2:Nyb+1)-gr_y(:,1:Nyb)

gr_dx_nodes(1,:) = gr_dx_nodes(2,:)
gr_dy_nodes(:,1) = gr_dy_nodes(:,2)

gr_dx_nodes(:,1) = gr_dx_nodes(:,2)
gr_dy_nodes(1,:) = gr_dy_nodes(2,:)

gr_dx_nodes(Nxb+2,:) = gr_dx_nodes(Nxb+1,:)
gr_dy_nodes(:,Nyb+2) = gr_dy_nodes(:,Nyb+1)

gr_dx_nodes(:,Nyb+2) = gr_dx_nodes(:,Nyb+1)
gr_dy_nodes(Nxb+2,:) = gr_dy_nodes(Nxb+1,:)

gr_dx_centers = 0.5*(gr_dx_nodes(1:Nxb+1,1:Nyb+1)+gr_dx_nodes(2:Nxb+2,1:Nyb+1))
gr_dy_centers = 0.5*(gr_dy_nodes(1:Nxb+1,1:Nyb+1)+gr_dy_nodes(1:Nxb+1,2:Nyb+2)) 

end subroutine Grid_init
