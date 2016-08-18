module MPI_data

      implicit none


      integer, save :: ierr, myid, procs, solver_comm, x_id, x_procs, x_comm
      integer, save :: y_id, y_procs, y_comm


      double precision, save :: start, finish, exec_time

end module MPI_data
