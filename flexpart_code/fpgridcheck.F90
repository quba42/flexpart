
subroutine fpgridcheck

  use par_mod
  use com_mod
  use conv_mod
  use fpmetbinary_mod
  !use cmapf_mod, only: stlmbr,stcm2p

  implicit none

  INTEGER :: ifn      ! .fp file index number
  integer :: i
  logical :: exists
  character(len=512) fpfname    ! .fp filename


  ! Determine which .fp file to load for getting gribcheck variables
  ! Note that this picks out the first or last file in the AVAIALBLE file,
  ! not necessarily the first or last file of the simulation (in some cases
  ! the simulation may not use all the files in the AVAILABLE file
  !if(ideltas.gt.0) then
  !  ifn=1
  !else
  !  ifn=numbwf
  !endif
  ifn=0
  do i=1,numbwf
    if ( ifn.eq.0 ) then
      if ( ldirect.eq.1 ) then
        inquire(file=TRIM(path(3)) // TRIM(wfname(i)), exist=exists)
      else
        inquire(file=TRIM(path(3)) // TRIM(wfname(i)), exist=exists)
      endif
      if ( exists ) then
        ifn = i
      endif
    endif
  end do
  
  ! Create the file name and load her up
  if ( ldirect.eq.1 ) then
    fpfname = TRIM(path(3)) // TRIM(wfname(ifn))
  else
    fpfname = TRIM(path(3)) // TRIM(wfname(ifn))
  endif
  print *, 'fpgridcheck(): LOADING.... ',  TRIM(fpfname)

  ! The second argument of this call indicates which index (1 or 2) 
  ! the met fields will be loaded into.  For this call, we don't really 
  ! care.  We just want the stuff relevant to gridcheck.  So, we arbitrarily
  ! load the other stuff in index 1.  It will be overwritten in subsequent 
  ! loads.
  CALL fpmetbinary_load(TRIM(fpfname), 1)


end subroutine fpgridcheck
