subroutine IO_write(x,y,uu,vv,pp,tt,id)

#include "Solver.h"

       implicit none
       real, dimension(Nxb+1, Nyb+1), intent(in) :: x,y,uu,vv,pp,tt
       integer, intent(in) :: id
       character(len=10) :: f1,f2,f3,f4,f5,f7
     
       write (f1, '( "X", I2.2, ".dat" )' )id
       write (f2, '( "Y", I2.2, ".dat" )' )id
       write (f3, '( "U", I2.2, ".dat" )' )id
       write (f4, '( "V", I2.2, ".dat" )' )id
       write (f5, '( "P", I2.2, ".dat" )' )id
       write (f7, '( "T", I2.2, ".dat" )' )id

       open(unit = 1, file = f1)
       open(unit = 2, file = f2)
       open(unit = 3, file = f3)
       open(unit = 4, file = f4)
       open(unit = 5, file = f5)
       open(unit = 7, file = f7)

       write(1,*),x
       write(2,*),y
       write(3,*),uu
       write(4,*),vv
       write(5,*),pp
       write(7,*),tt

       close(1)
       close(2)
       close(3)
       close(4)
       close(5)
       close(7)

end subroutine IO_write

