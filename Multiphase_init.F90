subroutine Multiphase_init()

   use Multiphase_data

#include "Solver.h"

   implicit none
 
   mph_rho1 = 0.597
   mph_rho2 = 958.4

   mph_thco1 = 0.025
   mph_thco2 = 0.679

   mph_cp1 = 2030.0*mph_rho1
   mph_cp2 = 4216.0*mph_rho2

end subroutine Multiphase_init
