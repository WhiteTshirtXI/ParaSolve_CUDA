subroutine MPI_physicalBC_temp(t_ex)

#include "Solver.h"

       use MPI_data

       implicit none

       include "mpif.h"

       real*4, dimension(Nxb+2,Nyb+2), intent(inout) :: t_ex
       integer*4 :: status(MPI_STATUS_SIZE)
    
       if ( x_id == 0) then

           !t_ex(1,:) = 2*313.0-t_ex(2,:)
           t_ex(1,:) = t_ex(2,:)

       end if

       if ( x_id == HK-1) then

           !t_ex(Nxb+2,:) = 2*313.0-t_ex(Nxb+1,:)
           t_ex(Nxb+2,:) = t_ex(Nxb+1,:)
       end if


       if ( y_id == 0) then

           t_ex(:,1) = 2*373.15-t_ex(:,2)
           !t_ex(:,1) = 373.15

       end if

       if ( y_id == HK-1) then

            t_ex(:,Nyb+2) = 2*383.15-t_ex(:,Nyb+1)
            !t_ex(:,Nyb+2) = t_ex(:,Nyb+1)
         

       end if

       call MPI_BARRIER(solver_comm,ierr)
   
end subroutine MPI_physicalBC_temp
