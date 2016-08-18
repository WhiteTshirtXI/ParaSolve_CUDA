subroutine MPI_applyBC(u_ex)

#include "Solver.h"

       use MPI_data

       implicit none

       include "mpif.h"

       real, dimension(Nxb+2,Nyb+2), intent(inout) :: u_ex
       integer :: status(MPI_STATUS_SIZE)

       call MPI_BARRIER(solver_comm, ierr)

       if(mod(mod(myid,HK),2) == 0) then
           
             if(mod(myid,HK) == 0) then

                  call MPI_SEND(u_ex(Nxb+1,:), Nyb+2, MPI_REAL, myid+(HK**0), 1,solver_comm, ierr)
                  call MPI_RECV(u_ex(Nxb+2,:), Nyb+2, MPI_REAL, myid+(HK**0), 2,solver_comm, status, ierr) 

             else if(mod(myid,HK) == HK-1) then
            
                  call MPI_SEND(u_ex(2,:), Nyb+2, MPI_REAL, myid-(HK**0), 3,solver_comm, ierr)
                  call MPI_RECV(u_ex(1,:), Nyb+2, MPI_REAL, myid-(HK**0), 4,solver_comm,status, ierr)

             else
                  call MPI_SEND(u_ex(Nxb+1,:), Nyb+2, MPI_REAL, myid+(HK**0), 1,solver_comm, ierr)
                  call MPI_RECV(u_ex(Nxb+2,:), Nyb+2, MPI_REAL, myid+(HK**0), 2,solver_comm,status, ierr) 
                  
                  call MPI_SEND(u_ex(2,:), Nyb+2, MPI_REAL, myid-(HK**0), 3,solver_comm, ierr)
                  call MPI_RECV(u_ex(1,:), Nyb+2, MPI_REAL, myid-(HK**0), 4,solver_comm,status, ierr)
                                   

             end if

       else if (mod(mod(myid,HK),2) == 1) then

             if(mod(myid,HK) == HK-1) then
            
                  call MPI_RECV(u_ex(1,:), Nyb+2, MPI_REAL,myid-(HK**0), 1,solver_comm,status, ierr)
                  call MPI_SEND(u_ex(2,:), Nyb+2, MPI_REAL,myid-(HK**0), 2,solver_comm, ierr)

             else
                  call MPI_RECV(u_ex(1,:), Nyb+2, MPI_REAL,myid-(HK**0), 1,solver_comm,status, ierr)
                  call MPI_SEND(u_ex(2,:), Nyb+2, MPI_REAL,myid-(HK**0), 2,solver_comm,ierr) 

                  call MPI_RECV(u_ex(Nxb+2,:), Nyb+2, MPI_REAL,myid+(HK**0), 3,solver_comm,status,ierr)
                  call MPI_SEND(u_ex(Nyb+1,:), Nyb+2, MPI_REAL,myid+(HK**0), 4,solver_comm,ierr)
                  

             end if

       end if

       call MPI_BARRIER(solver_comm,ierr)
   
       !! Second dimension !!

       if(mod(myid/HK,2) == 0) then

             if(myid/HK == 0) then

                  call MPI_SEND(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, myid+(HK**1), 5,solver_comm,ierr)
                  call MPI_RECV(u_ex(:,Nyb+2), Nxb+2, MPI_REAL, myid+(HK**1), 6,solver_comm,status, ierr)

             else if(myid/HK == HK-1) then

                  call MPI_SEND(u_ex(:,2), Nxb+2, MPI_REAL, myid-(HK**1), 7,solver_comm, ierr)
                  call MPI_RECV(u_ex(:,1), Nxb+2, MPI_REAL, myid-(HK**1), 8,solver_comm,status,ierr)

             else 
                  call MPI_SEND(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, myid+(HK**1), 5,solver_comm, ierr)
                  call MPI_RECV(u_ex(:,Nyb+2), Nxb+2, MPI_REAL, myid+(HK**1), 6,solver_comm,status,ierr)

                  call MPI_SEND(u_ex(:,2), Nxb+2, MPI_REAL, myid-(HK**1), 7,solver_comm, ierr)
                  call MPI_RECV(u_ex(:,1), Nxb+2, MPI_REAL, myid-(HK**1), 8,solver_comm,status,ierr)
                  

             end if

       else if (mod(myid/HK,2) == 1) then

             if(myid/HK == HK-1) then

                  call MPI_RECV(u_ex(:,1), Nxb+2, MPI_REAL, myid-(HK**1), 5,solver_comm,status, ierr)
                  call MPI_SEND(u_ex(:,2), Nxb+2, MPI_REAL, myid-(HK**1), 6,solver_comm, ierr)

             else
                  call MPI_RECV(u_ex(:,1), Nxb+2, MPI_REAL, myid-(HK**1), 5,solver_comm,status, ierr)
                  call MPI_SEND(u_ex(:,2), Nxb+2, MPI_REAL, myid-(HK**1), 6,solver_comm,ierr)

                  call MPI_RECV(u_ex(:,Nyb+2), Nxb+2, MPI_REAL, myid+(HK**1), 7,solver_comm,status, ierr)
                  call MPI_SEND(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, myid+(HK**1), 8,solver_comm,ierr)
                  

             end if

       end if            

       call MPI_BARRIER(solver_comm,ierr)

end subroutine

