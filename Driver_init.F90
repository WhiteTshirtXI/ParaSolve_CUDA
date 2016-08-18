subroutine Driver_init()
 
      use Driver_data
      use Grid_data
      use IncompNS_data

      implicit none
      
      real :: dt_sig, dt_cfl

      dr_t  = 200

      dt_sig = ins_sigma*(min(minval(gr_dx_nodes),minval(gr_dy_nodes))**2)/ins_inRe
      dt_cfl = ins_cfl*min(minval(gr_dx_nodes),minval(gr_dy_nodes))

      dr_dt = min(dt_sig,dt_cfl)

      dr_nt = dr_t/dr_dt

end subroutine Driver_init
