module MPI_data

      implicit none


      integer*4, save :: ierr, myid, procs, solver_comm, x_id, x_procs, x_comm
      integer*4, save :: y_id, y_procs, y_comm


      double precision, save :: start, finish, exec_time

end module MPI_data
