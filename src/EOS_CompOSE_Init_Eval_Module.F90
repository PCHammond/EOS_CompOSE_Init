module EOS_CompOSE_Init_Eval_Module
    implicit none
    contains
    subroutine EOS_CompOSE_Init_Eval(x1,polycoeffs,d_x1,x1_array,x1_count,result)
        integer, intent(in) :: x1_count
        real(kind=8), intent(in) :: x1, d_x1
        real(kind=8), intent(in) :: polycoeffs(4,x1_count-1), x1_array(x1_count-1)
        real(kind=8), intent(out) :: result
        real(kind=8) :: x1_temp
        integer :: i, x1_offset

        x1_offset = int((x1-x1_array(1))/d_x1)+1

        if (x1_offset.lt.1) then
            x1_offset = 1
            x1_temp = 0.0d0
            !print *, "x1 under bounds"
        else if (x1_offset.ge.x1_count-1) then
            x1_offset = x1_count-1
            x1_temp = 1.0d0
            !print *, "x1 above bounds"
        else
            x1_temp = (x1 - x1_array(x1_offset))/d_x1
        end if

        result = 0.0d0
        do i=0,3
            result = result + (polycoeffs(i+1,x1_offset) * (x1_temp**i))
        end do
    end subroutine EOS_CompOSE_Init_Eval
end module EOS_CompOSE_Init_Eval_Module
