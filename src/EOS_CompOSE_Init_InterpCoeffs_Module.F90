module EOS_CompOSE_Init_InterpCoeffs_Module
    implicit none
contains
    subroutine EOS_CompOSE_init_setInterpCoeffs(EOS_CompOSE_init_interpCoeffs)
    implicit none
    integer, intent(out) :: EOS_CompOSE_init_interpCoeffs(4,4)

    !line 1
    EOS_CompOSE_init_interpCoeffs(:,1) = (/ &
      1,  0,  0,  0 /)

    !line 2
    EOS_CompOSE_init_interpCoeffs(:,2) = (/ &
      0,  0,  1,  0 /)

    !line 3
    EOS_CompOSE_init_interpCoeffs(:,3) = (/ &
     -3,  3, -2, -1 /)

    !line 4
    EOS_CompOSE_init_interpCoeffs(:,4) = (/ &
      2, -2,  1,  1 /)

    end subroutine EOS_CompOSE_init_setInterpCoeffs
    
    subroutine EOS_CompOSE_Init_updateCoeffs(a_old, a_new, x0_0, h_0)
        real(kind=8), intent(in) :: x0_0
        real(kind=8), intent(in) :: h_0
        real(kind=8), intent(in) :: a_old(4)
        real(kind=8), intent(out) :: a_new(4)

        a_new(1) = a_old(1) - a_old(2)*x0_0/h_0 + &
            a_old(3)*x0_0**2/h_0**2 - a_old(4)*x0_0**3/h_0**3

        a_new(2) = a_old(2)/h_0 - 2*a_old(3)*x0_0/h_0**2 + &
            3*a_old(4)*x0_0**2/h_0**3

        a_new(3) = a_old(3)/h_0**2 - 3*a_old(4)*x0_0/h_0**3

        a_new(4) = a_old(4)/h_0**3

    end subroutine EOS_CompOSE_Init_updateCoeffs
end module EOS_CompOSE_Init_InterpCoeffs_Module
