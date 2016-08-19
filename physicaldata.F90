module physicaldata

       implicit none

       real*4, save, allocatable :: ph_center(:,:,:), ph_facex(:,:,:), ph_facey(:,:,:)
       target :: ph_center, ph_facex, ph_facey

end module physicaldata
