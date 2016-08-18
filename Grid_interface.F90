module Grid_interface

        implicit none

        interface
              subroutine Grid_init()
              end subroutine Grid_init
        end interface

        interface
              subroutine Grid_finalize()
              end subroutine Grid_finalize
        end interface

end module Grid_interface
