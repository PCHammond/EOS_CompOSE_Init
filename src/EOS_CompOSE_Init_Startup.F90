#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"
#include "cctk_Functions.h"

subroutine EOS_CompOSE_Init_Startup(CCTK_ARGUMENTS)

    use EOS_CompOSE_Init_Module
    use EOS_CompOSE_Init_InterpCoeffs_Module
    implicit none

    DECLARE_CCTK_PARAMETERS
    DECLARE_CCTK_ARGUMENTS

    call EOS_CompOSE_init_setInterpCoeffs(EOS_CompOSE_init_interpCoeffs)
end subroutine EOS_CompOSE_Init_Startup
