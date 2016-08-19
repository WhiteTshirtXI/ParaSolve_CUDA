attributes(global) subroutine CUDA_poisson(ps,ps_old,ps_RHS,gr_dx_centers,gr_dy_centers,gr_dx_nodes,gr_dy_nodes)

    implicit none
    real*4, intent(inout) :: ps(:,:)
    real*4, intent(in) :: ps_old(:,:),ps_RHS(:,:),gr_dx_centers(:,:),gr_dy_centers(:,:)
    real*4, intent(in) :: gr_dx_nodes(:,:),gr_dy_nodes(:,:)

    integer*4 :: i,j,n(2)

    i = (blockIdx%x-1)*blockDim%x + threadIdx%x
    j = (blockIdx%y-1)*blockDim%y + threadIdx%y

    n(1) = size(ps,1)
    n(2) = size(ps,2)

    if (i<=n(1) .and. j<=n(2)) then

           ps(i,j)=((ps_old(i,j+1)/(gr_dy_centers(i,j)*gr_dy_nodes(i,j)))+(ps(i,j-1)/(gr_dy_nodes(i,j)*gr_dy_centers(i-1,j-1)))&
                  +(ps_old(i+1,j)/(gr_dx_centers(i,j)*gr_dx_nodes(i,j)))+(ps(i-1,j)/(gr_dx_nodes(i,j)*gr_dx_centers(i-1,j-1)))&
                  +ps_RHS(i-1,j-1))&
                  *(1/((1/(gr_dx_nodes(i,j)*gr_dx_centers(i-1,j-1)))+(1/(gr_dy_nodes(i,j)*gr_dy_centers(i-1,j-1)))+&
                   (1/(gr_dx_nodes(i,j)*gr_dx_centers(i,j)))+(1/(gr_dy_nodes(i,j)*gr_dy_centers(i,j)))))

    end if

end subroutine CUDA_poisson
