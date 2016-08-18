module HeatAD_interface

     implicit none

     interface
         subroutine HeatAD_init()
         end subroutine HeatAD_init
     end interface


     interface
         subroutine HeatAD_solver(tstep)
         implicit none
         integer, intent(in) :: tstep
         end subroutine HeatAD_solver
     end interface


end module HeatAD_interface
