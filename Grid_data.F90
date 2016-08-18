module Grid_data

#include "Solver.h"

       implicit none

       real, save :: gr_Lx
       real, save :: gr_Ly
       
       integer, save :: gr_Nx
       integer, save :: gr_Ny

       real, save :: gr_dx
       real, save :: gr_dy

       real, save, allocatable, dimension(:,:) :: gr_dx_centers,gr_dy_centers

       real, save, allocatable, dimension(:,:) :: gr_dx_nodes, gr_dy_nodes

       real, save, allocatable, dimension(:,:) :: gr_x
       real, save, allocatable, dimension(:,:) :: gr_y

end module Grid_data 
