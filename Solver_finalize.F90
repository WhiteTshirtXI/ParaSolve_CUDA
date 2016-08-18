subroutine Solver_finalize

      use Grid_interface, only: Grid_finalize
      use MPI_interface, only: MPIsolver_finalize

      implicit none

      call Grid_finalize()
      call MPIsolver_finalize()

end subroutine Solver_finalize
