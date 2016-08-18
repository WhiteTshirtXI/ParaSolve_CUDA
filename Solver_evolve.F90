subroutine Solver_evolve       

#include "Solver.h"

    use IncompNS_interface, only: IncompNS_solver
    use HeatAD_interface, only: HeatAD_solver
    use Grid_data
    use Driver_data
    use physicaldata
    use IncompNS_data
    use HeatAD_data
    use MPI_data
    use IO_interface, only: IO_display, IO_write, IO_display_v2
    use Multiphase_interface, only:Multiphase_solver

    implicit none

    integer :: tstep,p_counter

    real, dimension(Nxb+1,Nyb+1) :: uu
    real, dimension(Nxb+1,Nyb+1) :: vv
    real, dimension(Nxb+1,Nyb+1) :: pp, tt, df, pf, th, cp

    real, pointer, dimension(:,:) :: u,v
    real, pointer, dimension(:,:,:) :: cc
    real :: solnX
  
    tstep = 0

    do while(tstep<=dr_nt) 


#ifdef MULTIPHASE
       call Multiphase_solver(tstep,solnX)
#endif
       call IncompNS_solver(tstep,p_counter)
       call HeatAD_solver(tstep)

       if (mod(tstep,5) == 0 .and. myid == 0) then

#ifdef SINGLEPHASE
          call IO_display(ins_u_res,ins_v_res,ins_p_res,ht_T_res,p_counter,tstep*dr_dt,ins_maxdiv,ins_mindiv)
#endif

#ifdef MULTIPHASE
          call IO_display_v2(tstep*dr_dt,solnX,ht_T_res)
#endif
       endif

#ifdef MULTIPHASE
       if(mod(tstep,10000) == 0) then
          cc => ph_center(:,:,:)

          df = ((cc(DFUN_VAR,1:Nxb+1,1:Nyb+1)+cc(DFUN_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(DFUN_VAR,1:Nxb+1,2:Nyb+2)+cc(DFUN_VAR,2:Nxb+2,2:Nyb+2))/2)/2
          th = ((cc(THCO_VAR,1:Nxb+1,1:Nyb+1)+cc(THCO_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(THCO_VAR,1:Nxb+1,2:Nyb+2)+cc(THCO_VAR,2:Nxb+2,2:Nyb+2))/2)/2
          tt = ((cc(TEMP_VAR,1:Nxb+1,1:Nyb+1)+cc(TEMP_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(TEMP_VAR,1:Nxb+1,2:Nyb+2)+cc(TEMP_VAR,2:Nxb+2,2:Nyb+2))/2)/2
          pf = ((cc(PFUN_VAR,1:Nxb+1,1:Nyb+1)+cc(PFUN_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(PFUN_VAR,1:Nxb+1,2:Nyb+2)+cc(PFUN_VAR,2:Nxb+2,2:Nyb+2))/2)/2

          nullify(cc)

          call IO_write(gr_x,gr_y,df,pf,th,tt,myid)

       end if
#endif

       if((ins_u_res .lt. 0.0000001) .and. (ins_u_res .ne. 0).and. (ins_v_res .lt. 0.0000001) .and. (ins_v_res .ne. 0) ) exit

       tstep = tstep +1

    end do

    u => ph_facex(VELC_VAR,:,:)
    v => ph_facey(VELC_VAR,:,:)
    cc => ph_center(:,:,:)
    
    uu = ((u(1:Nxb+1,1:Nyb+1)+u(1:Nxb+1,2:Nyb+2))/2 + (u(2:Nxb+2,1:Nyb+1)+u(2:Nxb+2,2:Nyb+2))/2)/2
    vv = ((v(1:Nxb+1,1:Nyb+1)+v(2:Nxb+2,1:Nyb+1))/2 + (v(1:Nxb+1,2:Nyb+2)+v(2:Nxb+2,2:Nyb+2))/2)/2
    pp = ((cc(PRES_VAR,1:Nxb+1,1:Nyb+1)+cc(PRES_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(PRES_VAR,1:Nxb+1,2:Nyb+2)+cc(PRES_VAR,2:Nxb+2,2:Nyb+2))/2)/2
    tt = ((cc(TEMP_VAR,1:Nxb+1,1:Nyb+1)+cc(TEMP_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(TEMP_VAR,1:Nxb+1,2:Nyb+2)+cc(TEMP_VAR,2:Nxb+2,2:Nyb+2))/2)/2

#ifdef MULTIPHASE
    df = ((cc(DFUN_VAR,1:Nxb+1,1:Nyb+1)+cc(DFUN_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(DFUN_VAR,1:Nxb+1,2:Nyb+2)+cc(DFUN_VAR,2:Nxb+2,2:Nyb+2))/2)/2
    th = ((cc(THCO_VAR,1:Nxb+1,1:Nyb+1)+cc(THCO_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(THCO_VAR,1:Nxb+1,2:Nyb+2)+cc(THCO_VAR,2:Nxb+2,2:Nyb+2))/2)/2
    cp = ((cc(CPRS_VAR,1:Nxb+1,1:Nyb+1)+cc(CPRS_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(CPRS_VAR,1:Nxb+1,2:Nyb+2)+cc(CPRS_VAR,2:Nxb+2,2:Nyb+2))/2)/2
    pf = ((cc(PFUN_VAR,1:Nxb+1,1:Nyb+1)+cc(PFUN_VAR,2:Nxb+2,1:Nyb+1))/2 + (cc(PFUN_VAR,1:Nxb+1,2:Nyb+2)+cc(PFUN_VAR,2:Nxb+2,2:Nyb+2))/2)/2
#endif

    nullify(u)
    nullify(v)
    nullify(cc)

    call IO_write(gr_x,gr_y,uu,vv,pp,tt,myid)
    !call IO_write(gr_x,gr_y,df,pf,th,tt,myid) 

end subroutine Solver_evolve
