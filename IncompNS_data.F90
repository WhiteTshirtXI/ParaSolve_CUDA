module IncompNS_data

#include "Solver.h"

    implicit none

    real, save :: ins_u_res,ins_v_res,ins_p_res
    real, save :: ins_inRe, ins_sigma, ins_cfl
    real, save :: ins_maxdiv, ins_mindiv

    real, save, dimension(Nxb,Nyb)   :: ins_G1_old
    real, save, dimension(Nxb,Nyb)   :: ins_G2_old

end module IncompNS_data
