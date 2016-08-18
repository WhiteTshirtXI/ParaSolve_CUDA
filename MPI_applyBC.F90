subroutine MPI_applyBC(u_ex)

#include "Solver.h"

       use MPI_data

       implicit none

       include "mpif.h"

       real, dimension(Nxb+2,Nyb+2), intent(inout) :: u_ex
       integer :: status(MPI_STATUS_SIZE)
        
       integer :: sendcounts(HK), recvcounts(HK), displs(HK)

       sendcounts = 0
       recvcounts = 0
       displs = 0

       if(mod(x_id,2) == 0) then
           
             if(x_id == 0) then

                 call MPI_SENDRECV(u_ex(Nxb+1,:), Nyb+2, MPI_REAL, x_id + 1, 1,&
                                   u_ex(Nxb+2,:), Nyb+2, MPI_REAL, x_id + 1, 2,x_comm, status, ierr) 

                 !sendcounts(x_id+1) = Nyb+2
                 !recvcounts = sendcounts

                 !call MPI_ALLTOALLV(u_ex(Nxb+1,:),sendcounts,displs,MPI_REAL,&
                 !                   u_ex(Nxb+2,:),recvcounts,displs,MPI_REAL,x_comm,status,ierr)

             else if(x_id == HK-1) then
            
                call MPI_SENDRECV(u_ex(2,:), Nyb+2, MPI_REAL, x_id-1, 3,&
                                  u_ex(1,:), Nyb+2, MPI_REAL, x_id-1, 4,x_comm,status, ierr)

             else
                call MPI_SENDRECV(u_ex(Nxb+1,:), Nyb+2, MPI_REAL, x_id + 1, 1,&
                                  u_ex(Nxb+2,:), Nyb+2, MPI_REAL, x_id + 1, 2, x_comm, status, ierr) 
                  
                call MPI_SENDRECV(u_ex(2,:), Nyb+2, MPI_REAL, x_id-1, 3,&
                                  u_ex(1,:), Nyb+2, MPI_REAL, x_id-1, 4,x_comm,status, ierr)
                                   

             end if

       else if (mod(x_id,2) == 1) then

             if(x_id == HK-1) then
           
               call MPI_SENDRECV(u_ex(2,:), Nyb+2, MPI_REAL,x_id-1, 2,&
                                 u_ex(1,:), Nyb+2, MPI_REAL,x_id-1, 1,x_comm, status, ierr)

             else
             
               call MPI_SENDRECV(u_ex(2,:), Nyb+2, MPI_REAL,x_id-1, 2,&
                                 u_ex(1,:), Nyb+2, MPI_REAL,x_id-1, 1,x_comm,status, ierr)

               call MPI_SENDRECV(u_ex(Nxb+1,:), Nyb+2, MPI_REAL,x_id+1, 4,&
                                 u_ex(Nxb+2,:), Nyb+2, MPI_REAL,x_id+1, 3,x_comm,status,ierr)
                  

             end if

       end if
 
       !! Second dimension !!

       if(mod(y_id,2) == 0) then

             if(y_id == 0) then

                  call MPI_SENDRECV(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, y_id+1, 5,&
                                    u_ex(:,Nyb+2), Nxb+2, MPI_REAL, y_id+1, 6,y_comm,status, ierr)

             else if(y_id == HK-1) then

                  call MPI_SENDRECV(u_ex(:,2), Nxb+2, MPI_REAL, y_id-1, 7,&
                                    u_ex(:,1), Nxb+2, MPI_REAL, y_id-1, 8,y_comm,status,ierr)

             else 
                  call MPI_SENDRECV(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, y_id+1, 5,&
                                    u_ex(:,Nyb+2), Nxb+2, MPI_REAL, y_id+1, 6,y_comm,status,ierr)

                  call MPI_SENDRECV(u_ex(:,2), Nxb+2, MPI_REAL, y_id-1, 7,&
                                    u_ex(:,1), Nxb+2, MPI_REAL, y_id-1, 8,y_comm,status,ierr)
                  

             end if

       else if (mod(y_id,2) == 1) then

             if(y_id == HK-1) then

                  call MPI_SENDRECV(u_ex(:,2), Nxb+2, MPI_REAL, y_id-1, 6,&
                               u_ex(:,1), Nxb+2, MPI_REAL, y_id-1, 5,y_comm,status, ierr)

             else

                  call MPI_SENDRECV(u_ex(:,2), Nxb+2, MPI_REAL, y_id-1, 6,&
                                    u_ex(:,1), Nxb+2, MPI_REAL, y_id-1, 5,y_comm,status, ierr)

 
                  call MPI_SENDRECV(u_ex(:,Nyb+1), Nxb+2, MPI_REAL, y_id+1, 8,&
                                    u_ex(:,Nyb+2), Nxb+2, MPI_REAL, y_id+1, 7,y_comm,status, ierr)
                  

             end if

       end if            

end subroutine

