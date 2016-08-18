module Poisson_interface

#include "Solver.h"
 
       implicit none

       interface
             subroutine Poisson_solver(ps_RHS,ps,ps_res,ps_counter,ps_quant)
                implicit none
                real, dimension(Nxb,Nyb), intent(in) :: ps_RHS
                real, dimension(Nxb+2,Nyb+2), intent(inout) :: ps
                real, intent(out) :: ps_res
                integer, intent(out) :: ps_counter
                integer, intent(in) :: ps_quant
             end subroutine Poisson_solver
       end interface


end module Poisson_interface
