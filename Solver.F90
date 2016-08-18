program Solver

   use Solver_interface, only: Solver_init, Solver_evolve, Solver_finalize

   implicit none

    call Solver_init()
    call Solver_evolve()
    call Solver_finalize()

end program Solver


