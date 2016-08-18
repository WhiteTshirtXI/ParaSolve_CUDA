subroutine MPIsolver_finalize()

      !$ use omp_lib
      use MPI_data

      implicit none

      include "mpif.h"

      call MPI_COMM_FREE(x_comm,ierr)
      call MPI_COMM_FREE(y_comm,ierr)

   
      !call cpu_time(finish)
      finish = omp_get_wtime()

      exec_time = (finish - start) 

      !print *,"Execution Time = ",exec_time," seconds"
      print '("Execution time: ",f20.10," seconds")',exec_time

      call MPI_FINALIZE(ierr)

end subroutine MPIsolver_finalize
