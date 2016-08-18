subroutine Multiphase_evolve(s,pf,thco,cprs,thco1,thco2,cp1,cp2)

#include "Solver.h"

    implicit none
    real,intent(inout),dimension(Nxb+2,Nyb+2) :: s,pf,thco,cprs
    real,intent(in) :: thco1,thco2,cp1,cp2   

    integer :: i,j

    do j=1,Nyb+2
     do i=1,Nxb+2

      pf(i,j) = 0.

      if(s(i,j) .ge. 0.) then

          pf(i,j) = 1.
          thco(i,j) = thco1/thco2
          cprs(i,j) = cp1/cp2
      
      else 

          thco(i,j) = thco2/thco2
          cprs(i,j) = cp2/cp2

      end if

     end do
    end do


end subroutine Multiphase_evolve
