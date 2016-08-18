module Multiphase_interface

#include "Solver.h"

implicit none


  interface
    subroutine Multiphase_init()
    end subroutine Multiphase_init
  end interface

  interface 
    subroutine Multiphase_solver(tstep,solnX)
     implicit none
     integer, intent(in) :: tstep
     real, intent(out) :: solnX
    end subroutine Multiphase_solver
  end interface

  interface
    subroutine Multiphase_evolve(s,pf,thco,cprs,thco1,thco2,cp1,cp2)
    implicit none
    real, intent(inout), dimension(Nxb+2,Nyb+2) :: s,pf,thco,cprs
    real, intent(in) :: thco1,thco2,cp1,cp2
    end subroutine Multiphase_evolve
  end interface

end module Multiphase_interface
