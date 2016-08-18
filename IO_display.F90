subroutine IO_display(u_res,v_res,p_res,T_res,p_counter,simtime,maxdiv,mindiv)

#include "Solver.h"

         implicit none

         real, intent(in) :: u_res,v_res,p_res,simtime,T_res,maxdiv,mindiv
         integer, intent(in) :: p_counter

         print *,"**************PARAMETER DISPLAY***************"
         print *,"Simulation Time         : ",simtime
         print *,"U Velocity Residual     : ",u_res
         print *,"V Velocity Residual     : ",v_res
         print *,"Pressure Residual       : ",p_res
         print *,"Temperature Residual    : ",T_res
         print *,"Pressure Poisson Counter: ",p_counter
         print *,"MAX DIV: ",maxdiv," MIN DIV: ",mindiv
        
end subroutine IO_display
