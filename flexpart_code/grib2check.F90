subroutine grib2check(igrib, fpname, conversion_factor)
#if defined CTBTO
  use GRIB_API
  use par_mod
  use com_mod

  implicit none
  integer, intent(in) :: igrib     ! GRIB id (from GRIB API)
  character(LEN=15), intent(in) :: fpname    ! FLEXPART Vtable name of variable
  real, intent(out) :: conversion_factor


    conversion_factor=1.
    if (trim(fpname) .eq. 'CONVPREC') then
        conversion_factor=1000.
    endif

    if (trim(fpname) .eq. 'SD') then
        conversion_factor=1000.
    endif
  

#endif
return
end subroutine grib2check
