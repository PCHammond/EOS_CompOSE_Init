#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"
#include "cctk_Functions.h"

subroutine EOS_CompOSE_Init_LoadTable(CCTK_ARGUMENTS)
    use EOS_CompOSE_Init_Module
    implicit none

    DECLARE_CCTK_ARGUMENTS
    DECLARE_CCTK_PARAMETERS
    DECLARE_CCTK_FUNCTIONS

    character(len=512) :: warnline
    character(len=256) :: eosInitTableFilename
    integer :: slength
    logical :: table_init_exists

    integer :: rowCount, colCount, row
    integer :: rhoIdx
    real(kind=8), allocatable :: tableEOSInitRaw(:,:)

    EOS_CompOSE_Init_rhoCount = eos_compose_init_tableshape(1)
    EOS_CompOSE_Init_varCount = eos_compose_init_tableshape(2)

    call CCTK_FortranString(slength, eos_compose_init_table_name, &
        eosInitTableFilename)

    inquire(file=trim(adjustl(eosInitTableFilename)), exist=table_init_exists)

    if(.not.table_init_exists) then
        write(warnline,"(A10,A,A15)") "EOS file ", &
            trim(adjustl(eosInitTableFilename)), " does not exist!"
        call CCTK_ERROR(warnline)
        stop
    endif

    rowCount = EOS_CompOSE_Init_rhoCount
    colCount = EOS_CompOSE_Init_varCount

    open(668, file=eosInitTableFilename, status="old", action="read")
    allocate(tableEOSInitRaw(colCount, rowCount))
    do row=1, rowCount
        read(668,*) tableEOSInitRaw(:,row)
    end do

    allocate(EOS_CompOSE_Init_table(EOS_CompOSE_Init_rhoCount, &
        EOS_CompOSE_Init_varCount))
    do row=1, rowCount
        rhoIdx = row

        !Density
        EOS_CompOSE_Init_table(rhoIdx,1) = &
            log10(tableEOSInitRaw(2,row)*unit_rho_tabToCode)

        !Ye
        EOS_CompOSE_Init_table(rhoIdx,2) = tableEOSInitRaw(3,row)
    end do

    allocate(EOS_CompOSE_Init_rhoArray(EOS_CompOSE_Init_rhoCount))
    EOS_CompOSE_Init_rhoArray = EOS_CompOSE_Init_table(:,1)
    EOS_CompOSE_Init_d_rho = EOS_CompOSE_Init_rhoArray(2) - &
        EOS_CompOSE_Init_rhoArray(1)

    !Ye interpolator
    allocate(EOS_CompOSE_Init_yeInterp(4, EOS_CompOSE_Init_rhoCount-1))
    call EOS_CompOSE_Init_BuildInterpolator(EOS_CompOSE_Init_table(:,2), &
        EOS_CompOSE_Init_rhoArray, EOS_CompOSE_Init_rhoCount, &
        EOS_CompOSE_Init_d_rho, EOS_CompOSE_Init_yeInterp)

    deallocate(tableEOSInitRaw)
end subroutine EOS_CompOSE_Init_LoadTable

subroutine EOS_CompOSE_Init_BuildInterpolator(variable, rhoArray, rhoCount,&
                                              d_rho, poly_coeffs_final)
    use EOS_CompOSE_Init_Module
    use EOS_CompOSE_Init_Differences_Module
    use EOS_CompOSE_Init_InterpCoeffs_Module
    implicit none

    !Input variables
    integer, intent(in) :: rhoCount
    real(kind=8), intent(in) :: variable(rhoCount)
    real(kind=8), intent(in) :: rhoArray(rhoCount)
    real(kind=8), intent(in) :: d_rho

    !Temporary variables
    real(kind=8), allocatable :: diff_array(:,:)
    real(kind=8), allocatable :: interp_matrix(:,:)
    real(kind=8), allocatable :: poly_coeffs_temp(:,:)
    integer, dimension(1,2) :: offset_matrix = reshape( (/ &
        0, 1 /), &
        shape(offset_matrix))
    integer, dimension(1) :: offset
    integer :: rhoIdx
    integer :: i, j

    !Output variables
    real(kind=8), intent(out) :: poly_coeffs_final(4,rhoCount-1)

    !Calculate finite differences
    allocate(diff_array(rhoCount,2))
    call EOS_CompOSE_Init_DiffArray(variable, rhoCount, diff_array)

    !Get cube coefficients
    allocate(interp_matrix(rhoCount-1,4))
    do i=1,2
        offset = offset_matrix(:,i)
        do j=0,1
            interp_matrix(:,i+2*j) = &
                diff_array(offset(1)+1 : rhoCount + offset(1)-1,j+1)
        end do
    end do

    !Calculate polynomial coefficients
    allocate(poly_coeffs_temp(rhoCount-1,4))
    do j=1,4
        poly_coeffs_temp(:,j) = 0.0
        do i=1,4
            poly_coeffs_temp(:,j) = poly_coeffs_temp(:,j) + &
                EOS_CompOSE_init_interpCoeffs(i,j)*interp_matrix(:,i)
        end do
    end do

    !Change of variable
    do rhoIdx = 1, rhoCount-1
        !call EOS_CompOSE_Init_updateCoeffs(poly_coeffs_temp(rhoIdx, :), &
        !    poly_coeffs_final(:, rhoIdx), rhoArray(rhoIdx), d_rho)
        poly_coeffs_final(:, rhoIdx) = &
            poly_coeffs_temp(rhoIdx, :)
    end do
end subroutine EOS_CompOSE_Init_BuildInterpolator
