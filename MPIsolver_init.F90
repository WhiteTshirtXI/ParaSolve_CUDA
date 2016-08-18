subroutine MPIsolver_init()

#include "Solver.h"

    !$ use omp_lib
    use MPI_data
    implicit none

    include "mpif.h"

    solver_comm = MPI_COMM_WORLD
    
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(solver_comm, myid, ierr)
    call MPI_COMM_SIZE(solver_comm, procs, ierr)

    call MPI_COMM_SPLIT(solver_comm,myid/HK,myid,x_comm,ierr)
    call MPI_COMM_SPLIT(solver_comm,mod(myid,HK),myid,y_comm,ierr)

    call MPI_COMM_RANK(x_comm,x_id,ierr)
    call MPI_COMM_SIZE(x_comm,x_procs,ierr)

    call MPI_COMM_RANK(y_comm,y_id,ierr)
    call MPI_COMM_size(y_comm,y_procs,ierr)

    !call cpu_time(start)
    start = omp_get_wtime()

end subroutine MPIsolver_init 
