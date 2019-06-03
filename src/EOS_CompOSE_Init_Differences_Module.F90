module EOS_CompOSE_Init_Differences_Module
    implicit none
contains
    subroutine dF_dx1(F_array,dF_dx1_array,len_x1)
        integer, intent(in) :: len_x1
        real(kind=8), intent(in) :: F_array(len_x1)
        real(kind=8), intent(out) :: dF_dx1_array(len_x1)
        dF_dx1_array(1) = -3.0*F_array(1) + 4.0*F_array(2) - F_array(3)
        dF_dx1_array(len_x1) = F_array(len_x1-2) - 4.0*F_array(len_x1-1) + F_array(len_x1)
        dF_dx1_array(2:len_x1-1) = F_array(3:len_x1) - F_array(1:len_x1-2)
    end subroutine dF_dx1

    subroutine EOS_CompOSE_Init_DiffArray(variable, rhoCount, diff_array)
    !Input variables
    integer, intent(in) :: rhoCount
    real(kind=8), intent(in) :: variable(rhoCount)

    !Output variables
    real(kind=8), intent(out) :: diff_array(rhoCount,2)

    diff_array(:,1) = variable

    call dF_dx1(diff_array(:,1),diff_array(:,2),rhoCount)   !d/dx
end subroutine EOS_CompOSE_Init_DiffArray
end module EOS_CompOSE_Init_Differences_Module
