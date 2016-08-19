module Grid_data

#include "Solver.h"

       implicit none

       real*4, save :: gr_Lx
       real*4, save :: gr_Ly
       
       integer*4, save :: gr_Nx
       integer*4, save :: gr_Ny

       real*4, save :: gr_dx
       real*4, save :: gr_dy

       real*4, save, allocatable, dimension(:,:) :: gr_dx_centers,gr_dy_centers

       real*4, save, allocatable, dimension(:,:) :: gr_dx_nodes, gr_dy_nodes

       real*4, save, allocatable, dimension(:,:) :: gr_x
       real*4, save, allocatable, dimension(:,:) :: gr_y

end module Grid_data 
