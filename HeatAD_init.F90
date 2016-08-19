subroutine HeatAD_init()

#include "Solver.h"

   use HeatAD_data
   use IncompNS_data
   use physicaldata
   use Grid_data
   use Multiphase_data, only: mph_thco1,mph_cp1
   use Driver_data, only: dr_dt
   use MPI_interface, only: MPI_applyBC, MPI_physicalBC_temp

   implicit none

   real*4,pointer,dimension(:,:) :: T
   integer*4 :: j,i
   real*4 :: solnX
   real*4 :: solnY
   real*4 :: ycell

   ht_Pr = 0.7
   ht_Nu = 0.332*(ht_Pr**0.33)/(ins_inRe**0.5)

   T => ph_center(TEMP_VAR,:,:)

   T = 373.15

   nullify(T)



end subroutine HeatAD_init
