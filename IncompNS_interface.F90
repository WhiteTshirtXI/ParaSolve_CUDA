module IncompNS_interface

       implicit none

       interface
           subroutine IncompNS_init()
           end subroutine IncompNS_init
       end interface

       interface
           subroutine IncompNS_solver(tstep,p_counter)
            implicit none
            integer*4, intent(in) :: tstep
            integer*4, intent(out) :: p_counter
           end subroutine IncompNS_solver
       end interface

end module IncompNS_interface
