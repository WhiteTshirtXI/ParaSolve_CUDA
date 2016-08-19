module IncompNS_data

#include "Solver.h"

    implicit none

    real*4, save :: ins_u_res,ins_v_res,ins_p_res
    real*4, save :: ins_inRe, ins_sigma, ins_cfl
    real*4, save :: ins_maxdiv, ins_mindiv

    real*4, save, dimension(Nxb,Nyb)   :: ins_G1_old
    real*4, save, dimension(Nxb,Nyb)   :: ins_G2_old

end module IncompNS_data
