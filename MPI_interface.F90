module MPI_interface
#include "Solver.h"

    implicit none

    interface
          subroutine MPIsolver_init()
          implicit none
          end subroutine MPIsolver_init
    end interface

    interface 
          subroutine MPIsolver_finalize()
          implicit none
          end subroutine MPIsolver_finalize
    end interface

    interface
          subroutine MPI_applyBC(u_ex)
          implicit none
          real, dimension(Nxb+2,Nyb+2),intent(inout) :: u_ex
          end subroutine MPI_applyBC
    end interface


    interface 
       subroutine MPI_CollectResiduals(res,res1)
       implicit none
       real, intent(inout) :: res,res1
       end subroutine MPI_CollectResiduals
    end interface

    interface 
        subroutine MPI_physicalBC_vel(u_ex,v_ex)
        implicit none
        real, dimension(Nxb+2,Nyb+2), intent(inout) :: u_ex, v_ex
        end subroutine MPI_physicalBC_vel
    end interface

    interface
        subroutine MPI_physicalBC_pres(p_ex)
        implicit none
        real, dimension(Nxb+2,Nyb+2), intent(inout) :: p_ex
        end subroutine MPI_physicalBC_pres
    end interface

   interface
        subroutine MPI_physicalBC_temp(t_ex)
        implicit none
        real, dimension(Nxb+2,Nyb+2), intent(inout) :: t_ex
        end subroutine MPI_physicalBC_temp
   end interface

end module MPI_interface
