module EOS_CompOSE_Init_Module
    implicit none

    !Physical constants
    real(kind=8), parameter :: const_Msol   = 1.98847d30       !kg
    real(kind=8), parameter :: const_c      = 2.99792458d8     !m * s-1
    real(kind=8), parameter :: const_G      = 6.67408d-11      !m3 * kg-1 * s-2
    real(kind=8), parameter :: const_Mn_kg  = 1.6726d-27       !kg
    real(kind=8), parameter :: const_fm     = 1.0d-15          !m
    real(kind=8), parameter :: const_qe     = 1.6021796208d-19 !C
    real(kind=8), parameter :: const_Mn_Mev = 9.3828d2         !MeV/c**2

    !Unit conversion factors calculated by hand
    real(kind=8), parameter :: unit_rho_tabToCode     = 2.7081965813424477d-03

    integer, dimension(4,4) :: EOS_CompOSE_init_interpCoeffs

    real(kind=8), allocatable :: EOS_CompOSE_Init_table(:,:)
    real(kind=8), allocatable :: EOS_CompOSE_Init_rhoArray(:)

    real(kind=8), allocatable :: EOS_CompOSE_Init_yeInterp(:,:)

    integer :: EOS_CompOSE_Init_rhoCount
    integer :: EOS_CompOSE_Init_varCount

    real(kind=8) :: EOS_CompOSE_Init_d_rho
end module EOS_CompOSE_Init_Module
