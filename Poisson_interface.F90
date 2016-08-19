module Poisson_interface

#include "Solver.h"
 
       implicit none

       interface
             subroutine Poisson_solver(ps_RHS,ps,ps_res,ps_counter,ps_quant)
                implicit none
                real*4, dimension(Nxb,Nyb), intent(in) :: ps_RHS
                real*4, dimension(Nxb+2,Nyb+2), intent(inout) :: ps
                real*4, intent(out) :: ps_res
                integer*4, intent(out) :: ps_counter
                integer*4, intent(in) :: ps_quant
             end subroutine Poisson_solver
       end interface


end module Poisson_interface
