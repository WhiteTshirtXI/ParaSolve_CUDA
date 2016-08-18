module Solver_interface

   implicit none

   interface
    subroutine Solver_init()
    end subroutine Solver_init
   end interface

   interface
    subroutine Solver_evolve()
    end subroutine Solver_evolve
   end interface

   interface
    subroutine Solver_finalize()
    end subroutine Solver_finalize
   end interface

end module Solver_interface
