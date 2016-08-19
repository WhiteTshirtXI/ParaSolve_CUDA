subroutine MPI_CollectResiduals(res,res1)

      use MPI_data

      implicit none

      include "mpif.h"
      
      real*4, intent(inout) :: res,res1
      integer*4 :: status(MPI_STATUS_SIZE)
      
      call MPI_ALLREDUCE(res,res1,1,MPI_REAL,MPI_SUM,solver_comm,ierr)

end subroutine MPI_CollectResiduals
