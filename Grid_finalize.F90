subroutine Grid_finalize()

   use Grid_data
   use physicaldata

   implicit none
   
   deallocate(gr_dx_centers)
   deallocate(gr_dy_centers)
   deallocate(gr_dx_nodes)
   deallocate(gr_dy_nodes)
   deallocate(gr_x)
   deallocate(gr_y)
   deallocate(ph_center)
   deallocate(ph_facex)
   deallocate(ph_facey)

end subroutine Grid_finalize 
