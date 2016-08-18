subroutine MPI_physicalBC_pres(p_ex)

#include "Solver.h"

       use MPI_data

       implicit none

       include "mpif.h"

       real, dimension(Nxb+2,Nyb+2), intent(inout) :: p_ex
       integer :: status(MPI_STATUS_SIZE)
    
       if ( x_id == 0) then

           p_ex(1,:)=p_ex(2,:)

       end if

       if ( x_id == HK-1) then

           p_ex(Nxb+2,:)=p_ex(Nxb+1,:)

       end if


       if ( y_id == 0) then

           p_ex(:,1)=p_ex(:,2)

       end if

       if ( y_id == HK-1) then

           p_ex(:,Nyb+2)=p_ex(:,Nyb+1)

       end if

       call MPI_BARRIER(solver_comm,ierr)
   
end subroutine MPI_physicalBC_pres
