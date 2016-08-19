module CUDA_interface

  contains
  attributes(global) subroutine CUDA_poisson(ps,ps_old,ps_RHS,gr_dx_centers,gr_dy_centers,gr_dx_nodes,gr_dy_nodes)
    implicit none
    real*4, intent(inout) :: ps(:,:)
    real*4, intent(in) :: ps_old(:,:),ps_RHS(:,:),gr_dx_centers(:,:),gr_dy_centers(:,:)
    real*4, intent(in) :: gr_dx_nodes(:,:),gr_dy_nodes(:,:)
  end subroutine CUDA_poisson

end module CUDA_interface
