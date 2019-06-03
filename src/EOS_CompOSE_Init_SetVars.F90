#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"
#include "cctk_Functions.h"

subroutine EOS_CompOSE_Init_SetYe(CCTK_ARGUMENTS)
    use EOS_CompOSE_Init_Module
    use EOS_CompOSE_Init_Eval_Module
    implicit none

    DECLARE_CCTK_ARGUMENTS
    DECLARE_CCTK_PARAMETERS

    CCTK_INT :: i,j,k

    do k=1,cctk_lsh(3)
      do j=1,cctk_lsh(2)
        do i=1,cctk_lsh(1)
          call EOS_CompOSE_Init_Eval(log10(rho(i,j,k)), EOS_CompOSE_Init_yeInterp, &
            EOS_CompOSE_Init_d_rho, EOS_CompOSE_Init_rhoArray, &
            EOS_CompOSE_Init_rhoCount, Y_e(i,j,k))
          call EOS_CompOSE_Init_Eval(log10(rho_p(i,j,k)), EOS_CompOSE_Init_yeInterp, &
            EOS_CompOSE_Init_d_rho, EOS_CompOSE_Init_rhoArray, &
            EOS_CompOSE_Init_rhoCount, Y_e_p(i,j,k))
          call EOS_CompOSE_Init_Eval(log10(rho_p_p(i,j,k)), EOS_CompOSE_Init_yeInterp, &
            EOS_CompOSE_Init_d_rho, EOS_CompOSE_Init_rhoArray, &
            EOS_CompOSE_Init_rhoCount, Y_e_p_p(i,j,k))
        end do
      end do
    end do
end subroutine EOS_CompOSE_Init_SetYe

subroutine EOS_CompOSE_Init_SetTemp(CCTK_ARGUMENTS)
    use EOS_CompOSE_Init_Module
    use EOS_CompOSE_Init_Eval_Module
    implicit none

    DECLARE_CCTK_ARGUMENTS
    DECLARE_CCTK_PARAMETERS

    CCTK_INT :: i,j,k,idx3,idx4
    do k=1,cctk_lsh(3)
      do j=1,cctk_lsh(2)
        do i=1,cctk_lsh(1)
          temperature(i,j,k) = eos_compose_init_temp
          temperature_p(i,j,k) = eos_compose_init_temp
          temperature_p_p(i,j,k) = eos_compose_init_temp
        end do
      end do
    end do
end subroutine EOS_CompOSE_Init_SetTemp

subroutine EOS_CompOSE_YeTemp_from_rho(npoints,rho,temp,ye)
    use EOS_CompOSE_Init_Module
    use EOS_CompOSE_Init_Eval_Module
    implicit none
    DECLARE_CCTK_PARAMETERS

    CCTK_INT, intent(in)     :: npoints
    CCTK_REAL, intent(in)    :: rho(npoints)
    CCTK_REAL, intent(out)   :: temp(npoints), ye(npoints)

    ! local vars
    integer          :: i

    do i=1,npoints
      temp(i) = eos_compose_init_temp
      call EOS_CompOSE_Init_Eval(log10(rho(i)), EOS_CompOSE_Init_yeInterp, &
        EOS_CompOSE_Init_d_rho, EOS_CompOSE_Init_rhoArray, &
        EOS_CompOSE_Init_rhoCount, ye(i))
    end do

end subroutine EOS_CompOSE_YeTemp_from_rho
