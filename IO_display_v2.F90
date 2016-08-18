subroutine IO_display_v2(simTime,solnX,T_res)

#include "Solver.h"

         use physicaldata
         use Driver_data, only: dr_dt

         implicit none

         real, intent(in) :: simtime,T_res,solnX
         real, pointer, dimension(:,:) :: df

         df => ph_center(DFUN_VAR,:,:)

         print *,"**************PARAMETER DISPLAY***************"
         print *,"Simulation Time         : ",simtime
         print *,"Interface Location      : ",solnX
         print *,"Temperature Residual    : ",T_res
         print *,"DFUN1,DFUN2,DFUN3       : ",df(5,Nxb+2),df(5,Nxb+1),df(5,Nxb)       

         nullify(df)
end subroutine IO_display_v2
