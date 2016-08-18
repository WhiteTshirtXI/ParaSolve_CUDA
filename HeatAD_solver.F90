subroutine HeatAD_solver(tstep)

#include "Solver.h"

      use Grid_data
      use IncompNS_data
      use HeatAD_data
      use Driver_data
      use physicaldata
      use MPI_interface, only: MPI_applyBC, MPI_physicalBC_temp, MPI_CollectResiduals
      use Multiphase_data, only: mph_cp2,mph_thco2

      implicit none
      
      integer, intent(in) :: tstep
      real, pointer, dimension(:,:) :: T,u,v,s,pf,thco,cp
      real, dimension(Nxb+2,Nyb+2) :: T_old

      integer :: i,j

      real :: u_plus, u_mins, v_plus, v_mins, u_conv, v_conv
      real :: Tx_plus, Tx_mins, Ty_plus, Ty_mins
      real :: Tij, Tipj, Timj, Tijp, Tijm
      real :: Txx, Tyy, th, dxp, dxm, dyp, dym
      real :: alphax_plus, alphax_mins, alphay_plus, alphay_mins, alpha_interface

      real :: T_res1
      real :: Tsat

      ht_T_res = 0.0
      T_res1 = 0.0

      T => ph_center(TEMP_VAR,:,:)
      u => ph_facex(VELC_VAR,:,:)
      v => ph_facey(VELC_VAR,:,:)

#ifdef MULTIPHASE

      s => ph_center(DFUN_VAR,:,:)
      pf => ph_center(PFUN_VAR,:,:)
      thco => ph_center(THCO_VAR,:,:)
      cp => ph_center(CPRS_VAR,:,:)

      Tsat = 373.13

#endif

      T_old = T

#ifdef SINGLEPHASE

#ifdef TEMP_SOLVER_CENTRAL

   T(2:Nxb+1,2:Nyb+1) = T_old(2:Nxb+1,2:Nyb+1) &
  +((dr_dt*ins_inRe)/(ht_Pr*(gr_dx_centers(2:Nxb+1,2:Nyb+1)**2)))*(T_old(3:Nxb+2,2:Nyb+1)+T_old(1:Nxb,2:Nyb+1)-2*T_old(2:Nxb+1,2:Nyb+1))&
  +((dr_dt*ins_inRe)/(ht_Pr*(gr_dy_centers(2:Nxb+1,2:Nyb+1)**2)))*(T_old(2:Nxb+1,3:Nyb+2)+T_old(2:Nxb+1,1:Nyb)-2*T_old(2:Nxb+1,2:Nyb+1))&
  -((dr_dt*(u(2:Nxb+1,2:Nyb+1) + u(1:Nxb,2:Nyb+1))/2)/(gr_dx_centers(2:Nxb+1,2:Nyb+1)+gr_dx_centers(1:Nxb,2:Nyb+1)))&
   *(T_old(3:Nxb+2,2:Nyb+1)-T_old(1:Nxb,2:Nyb+1))&
  -((dr_dt*(v(2:Nxb+1,2:Nyb+1) + v(2:Nxb+1,1:Nyb))/2)/(gr_dy_centers(2:Nxb+1,2:Nyb+1)+gr_dx_centers(2:Nxb+1,1:Nyb)))&
  *(T_old(2:Nxb+1,3:Nyb+2)-T_old(2:Nxb+1,1:Nyb))

#endif


#ifdef TEMP_SOLVER_UPWIND

  do j=2,Nxb+1
     do i=2,Nyb+1

     u_conv = (u(i,j)+u(i-1,j))/2.
     v_conv = (v(i,j)+v(i,j-1))/2.

     u_plus = max(u_conv, 0.)
     u_mins = min(u_conv, 0.)

     v_plus = max(v_conv, 0.)
     v_mins = min(v_conv, 0.)

     Tx_plus = (T_old(i+1,j)-T_old(i,j))/gr_dx_centers(i,j)
     Tx_mins = (T_old(i,j)-T_old(i-1,j))/gr_dx_centers(i-1,j)

     Ty_plus = (T_old(i,j+1)-T_old(i,j))/gr_dy_centers(i,j)
     Ty_mins = (T_old(i,j)-T_old(i,j-1))/gr_dy_centers(i,j-1)

     T(i,j) = T_old(i,j)+((dr_dt*ins_inRe)/(ht_Pr*gr_dx_centers(i,j)*gr_dx_centers(i,j)))*(T_old(i+1,j)+T_old(i-1,j)-2*T_old(i,j))&
                        +((dr_dt*ins_inRe)/(ht_Pr*gr_dy_centers(i,j)*gr_dy_centers(i,j)))*(T_old(i,j+1)+T_old(i,j-1)-2*T_old(i,j))&
                        -((dr_dt))*(u_plus*Tx_mins + u_mins*Tx_plus)&
                        -((dr_dt))*(v_plus*Ty_mins + v_mins*Ty_plus)

     end do
  end do

#endif

     call MPI_applyBC(T)
     call MPI_physicalBC_temp(T)

     do i=1,Nxb+2
          ht_T_res = ht_T_res + sum((T(i,:)-T_old(i,:))**2)
     enddo

     call MPI_CollectResiduals(ht_T_res,T_res1)

     ht_T_res = sqrt(T_res1/((Nxb+2)*(Nyb+2)*(HK**HD)))

     nullify(T)
     nullify(u)
     nullify(v)

#endif

#ifdef MULTIPHASE

#ifdef TEMP_SOLVER_UPWIND

   do j=2,Nxb+1
     do i=2,Nyb+1

     u_conv = (u(i,j)+u(i-1,j))/2.
     v_conv = (v(i,j)+v(i,j-1))/2.

     u_plus = max(u_conv, 0.)
     u_mins = min(u_conv, 0.)

     v_plus = max(v_conv, 0.)
     v_mins = min(v_conv, 0.)

     Tx_plus = T_old(i+1,j)
     Tx_mins = T_old(i-1,j)

     Ty_plus = T_old(i,j+1)
     Ty_mins = T_old(i,j-1)


     Tipj = T_old(i+1,j)
     Timj = T_old(i-1,j)

     Tijp = T_old(i,j+1)
     Tijm = T_old(i,j-1)

     Tij = T_old(i,j)

     ! Note - Have to Pair mutually exclusive cases together

     ! Case 1 !
     if(s(i,j)*s(i+1,j).le.0.0) then

       if (abs(s(i,j))/(abs(s(i,j))+abs(s(i+1,j))) .gt. 0.01) then

       th = abs(s(i,j))/(abs(s(i,j))+abs(s(i+1,j)))
       Tx_plus = (Tsat-T_old(i,j))/th + Tij

       else

       th = abs(s(i-1,j))/(abs(s(i-1,j))+abs(s(i+1,j)))
       Tx_plus = (Tsat-T_old(i-1,j))/th + T_old(i-1,j)
       
       end if
     end if
     ! End of Case 1 !


     ! Case 2 !
     if(s(i,j)*s(i-1,j).le.0.0) then

       if (abs(s(i,j))/(abs(s(i,j))+abs(s(i-1,j))) .gt. 0.01) then

       th = abs(s(i,j))/(abs(s(i,j))+abs(s(i-1,j)))
       Tx_mins = (Tsat-T_old(i,j))/th + Tij

       else

       th = abs(s(i+1,j))/(abs(s(i+1,j))+abs(s(i-1,j)))
       Tx_mins = (Tsat-T_old(i+1,j))/th + T_old(i+1,j)
       
       end if
     end if
     ! End of Case 2 !


    ! Case 3 !
    if(s(i,j)*s(i,j+1).le.0.0) then

      if (abs(s(i,j))/(abs(s(i,j))+abs(s(i,j+1))) .gt. 0.01) then

      th = abs(s(i,j))/(abs(s(i,j))+abs(s(i,j+1)))
      Ty_plus = (Tsat-T_old(i,j))/th + Tij

      else

      th = abs(s(i,j-1))/(abs(s(i,j-1))+abs(s(i,j+1)))
      Ty_plus = (Tsat-T_old(i,j-1))/th + T_old(i,j-1)
     
      end if
    end if
    ! End of Case 3 !


    ! Case 4 !
    if(s(i,j)*s(i,j-1).le.0.0) then

      if (abs(s(i,j))/(abs(s(i,j))+abs(s(i,j-1))) .gt. 0.01) then

      th = abs(s(i,j))/(abs(s(i,j))+abs(s(i,j-1)))
      Ty_mins = (Tsat-T_old(i,j))/th + Tij

      else

      th = abs(s(i,j+1))/(abs(s(i,j+1))+abs(s(i,j-1)))
      Ty_mins = (Tsat-T_old(i,j+1))/th + T_old(i,j+1)
      
      end if
    end if
    ! End of Case 4 !

    alphax_plus = ((thco(i,j)/cp(i,j))+(thco(i+1,j)/cp(i+1,j)))*0.5!*(mph_thco2/mph_cp2)
    alphax_mins = ((thco(i,j)/cp(i,j))+(thco(i-1,j)/cp(i-1,j)))*0.5!*(mph_thco2/mph_cp2)
    alphay_plus = ((thco(i,j)/cp(i,j))+(thco(i,j+1)/cp(i,j+1)))*0.5!*(mph_thco2/mph_cp2)
    alphay_mins = ((thco(i,j)/cp(i,j))+(thco(i,j-1)/cp(i,j-1)))*0.5!*(mph_thco2/mph_cp2)

    if (s(i,j)*s(i+1,j) .le. 0.0) then

        if ( abs(s(i,j))/(abs(s(i,j))+abs(s(i+1,j))) .gt. 0.01) then

        th = abs(s(i,j))/(abs(s(i,j))+abs(s(i+1,j)))
        alpha_interface = (thco(i,j)/cp(i,j))*th + &
                          (1.-th)*(thco(i+1,j)/cp(i+1,j))
        alphax_plus = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        else

        th = abs(s(i-1,j))/(abs(s(i-1,j))+abs(s(i+1,j)))
        alpha_interface = (thco(i-1,j)/cp(i-1,j))*th + &
                          (1.-th)*(thco(i+1,j)/cp(i+1,j))
        alphax_plus = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        end if 
    end if

    if (s(i,j)*s(i-1,j) .le. 0.0) then

        if ( abs(s(i,j))/(abs(s(i,j))+abs(s(i-1,j))) .gt. 0.01) then

        th = abs(s(i,j))/(abs(s(i,j))+abs(s(i-1,j)))
        alpha_interface = (thco(i,j)/cp(i,j))*th + &
                          (1.-th)*(thco(i-1,j)/cp(i-1,j))
        alphax_mins = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        else

        th = abs(s(i+1,j))/(abs(s(i-1,j))+abs(s(i+1,j)))
        alpha_interface = (thco(i+1,j)/cp(i+1,j))*th + &
                          (1.-th)*(thco(i-1,j)/cp(i-1,j))
        alphax_mins = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        end if
    end if

    if (s(i,j)*s(i,j+1) .le. 0.0) then

        if ( abs(s(i,j))/(abs(s(i,j))+abs(s(i,j+1))) .gt. 0.01) then

        th = abs(s(i,j))/(abs(s(i,j))+abs(s(i,j+1)))
        alpha_interface = (thco(i,j)/cp(i,j))*th + &
                          (1.-th)*(thco(i,j+1)/cp(i,j+1))
        alphay_plus = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        else

        th = abs(s(i,j-1))/(abs(s(i,j-1))+abs(s(i,j+1)))
        alpha_interface = (thco(i,j-1)/cp(i,j-1))*th + &
                          (1.-th)*(thco(i,j+1)/cp(i,j+1))
        alphay_plus = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        end if
    end if

    if (s(i,j)*s(i,j-1) .le. 0.0) then

        if ( abs(s(i,j))/(abs(s(i,j))+abs(s(i,j-1))) .gt. 0.01) then

        th = abs(s(i,j))/(abs(s(i,j))+abs(s(i,j-1)))
        alpha_interface = (thco(i,j)/cp(i,j))*th + &
                          (1.-th)*(thco(i,j-1)/cp(i,j-1))
        alphay_mins = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        else

        th = abs(s(i,j+1))/(abs(s(i,j-1))+abs(s(i,j+1)))
        alpha_interface = (thco(i,j+1)/cp(i,j+1))*th + &
                          (1.-th)*(thco(i,j-1)/cp(i,j-1))
        alphay_mins = (alpha_interface + (thco(i,j)/cp(i,j)))*0.5

        end if
    end if

    alphax_plus = alphax_plus*(mph_thco2/mph_cp2)
    alphax_mins = alphax_mins*(mph_thco2/mph_cp2)                                                                                                
    alphay_plus = alphay_plus*(mph_thco2/mph_cp2)  
    alphay_mins = alphay_mins*(mph_thco2/mph_cp2) 


    Txx = (alphax_plus*(Tx_plus-Tij)/gr_dx - alphax_mins*(Tij-Tx_mins)/gr_dx)/gr_dx
    Tyy = (alphay_plus*(Ty_plus-Tij)/gr_dy - alphay_mins*(Tij-Ty_mins)/gr_dy)/gr_dy

    T(i,j) = T_old(i,j) + dr_dt*((-(u_plus*(Tij-Tx_mins)/gr_dx-u_mins*(Tx_plus-Tij)/gr_dx)&
                                  -(v_plus*(Tij-Ty_mins)/gr_dy-v_mins*(Ty_plus-Tij)/gr_dy))&
                                  +(Txx + Tyy))

     end do
   end do

#endif

   do i=1,Nxb+2
          ht_T_res = ht_T_res + sum((T(i,:)-T_old(i,:))**2)
   enddo

   call MPI_CollectResiduals(ht_T_res,T_res1)

   ht_T_res = sqrt(T_res1/((Nxb+2)*(Nyb+2)*(HK**HD)))

   call MPI_applyBC(T)
   call MPI_physicalBC_temp(T)

   nullify(T)
   nullify(u)
   nullify(v)

#ifdef MULTIPHASE
   nullify(s)
   nullify(pf)
   nullify(thco)
   nullify(cp)
#endif

#endif

end subroutine HeatAD_solver
