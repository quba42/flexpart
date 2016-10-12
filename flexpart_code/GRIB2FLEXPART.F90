!**********************************************************************
! Copyright 1998,1999,2000,2001,2002,2005,2007,2008,2009,2010         *
! Andreas Stohl, Petra Seibert, A. Frank, Gerhard Wotawa,             *
! Caroline Forster, Sabine Eckhardt, John Burkhart, Harald Sodemann   *
!                                                                     *
! This file is part of FLEXPART.                                      *
!                                                                     *
! FLEXPART is free software: you can redistribute it and/or modify    *
! it under the terms of the GNU General Public License as published by*
! the Free Software Foundation, either version 3 of the License, or   *
! (at your option) any later version.                                 *
!                                                                     *
! FLEXPART is distributed in the hope that it will be useful,         *
! but WITHOUT ANY WARRANTY; without even the implied warranty of      *
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
! GNU General Public License for more details.                        *
!                                                                     *
! You should have received a copy of the GNU General Public License   *
! along with FLEXPART.  If not, see <http://www.gnu.org/licenses/>.   *
!**********************************************************************

program grib2flexpart

  !*****************************************************************************
  !                                                                            *
  !     This is the GRIB 2 FP conversion routine                               *
  !                                                                            *
  !     Author: M. Harustak                                                    *
  !                                                                            *
  !     05 October 2015                                                        *
  !                                                                            *
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  !                                                                            *
  ! Constants:                                                                 *
  !                                                                            *
  !*****************************************************************************

  use point_mod
  use par_mod
  use com_mod
  use conv_mod

  implicit none

  integer :: metdata_format = unknown_metdata
  integer :: itime, nstop1
  integer :: i
  character(len=256) :: arg
  character(len=512) :: dumpPath
  character(len=512) :: inputFileName
  character(len=512) :: nestedFileName
  character(len=32) :: lsubgridTXT
  integer :: useAvailable = 0
  integer :: overwritecheck

  ! Print the GPL License statement
  !*******************************************************
#if defined CTBTO
  print*,'Welcome to GRIB2FLEXPART Version 9.3.1f CTBTO'
#else
  print*,'Welcome to GRIB2FLEXPART Version 9.3.1f'
#endif

  print*,'FLEXPART is free software released under the GNU Genera'// &
       'l Public License.'

  ! no argument => error
  if (( iargc().lt.2 ).or.( iargc().eq.3)) then
    print *,' '
    print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
    print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> <source file> [<source file>...]'
    print *,' '
    stop 'Error: Missing arguments'
  elseif ( iargc().eq.2 ) then
    call getarg(1,arg)
    if ( arg.eq."useAvailable" ) then
      print *,'Running in use-available mode'
      useAvailable = 1
    else
      print *,' '
      print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
      print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> &
 <source file> [<source file>...]'
      print *,' '
      stop 'Error: Incorrect arguments'
    endif
  else
    ! 2 and more arguments => ok, parse arguments
    call getarg(1,arg)
    if ( arg.eq."forward") then
      ldirect = 1
    else if ( arg.eq."backward") then
      ldirect = -1
    else
      print *,' '
      print *,'Usage in use-available mode: grib2flexpart useAvailable <output directory>'
      print *,'Usage in command line mode: grib2flexpart forward|backward subgrid <output directory> &
<source file> [<source file>...]'
      print *,' '
      stop 'Error: Incorrect arguments'
    endif
    print *,'Running in command line mode'
    useAvailable = 0
  endif

  if ( useAvailable.eq.0 ) then
    call getarg(2,lsubgridTxt)
    read (lsubgridTxt, '(i10)') lsubgrid
    lsubgrid=1
    call getarg(3,dumpPath)
    do i=4,iargc()
      call getarg(i,inputFileName)
      numbwf = i - 4 + 1
      if ( ldirect.eq.1 ) then
        wfname(i-4+1) = inputFileName
        if ( overwritecheck( dumpPath, wfname(i-4+1), 0) == -1 ) then
          ! if the output and input directory is the same, exit with error
          print *, "Input and output paths must be different"
          print *, "Output: "//trim(dumpPath)
          print *, "input: "//trim(wfname(i-4+1))
          stop 'Error: Incorrect arguments'
        endif
      else
        wfname(iargc()+1-i) = inputFileName
        if ( overwritecheck( dumpPath, wfname(iargc()+1-i), 0) == -1 ) then
          ! if the output and input directory is the same, exit with error
          print *, "Input and output paths must be different"
          print *, "Output: "//trim(dumpPath)
          print *, "Input: "//trim(wfname(iargc()+1-i))
          stop 'Error: Incorrect arguments'
        endif
      endif
    end do

  else
    call getarg(2,dumpPath)

    call readpaths

    call readcommand

    call readavailable
    do i=1,numbwf 
      if ( overwritecheck( dumpPath, path(3)(1:length(3)) // trim(wfname(i)),0) == -1) then
          ! if the output and input directory is the same, exit with error
          print *, "Input and output paths must be different"
          print *, "Output: "//trim(dumpPath)
          print *, "Input: "//path(3)(1:length(3)) // trim(wfname(i))
          stop 'Error: Incorrect arguments'
        endif
    enddo
  endif


! Reset the times of the wind fields that are kept in memory to no time
 !**********************************************************************

  do i=1,2
    memind(i)=i
    memtime(i)=999999999
  end do

  ! Detect metdata format
  call detectformat(metdata_format)
  if (metdata_format.eq.ecmwf_metdata) then
    print*,'ECMWF metdata detected'
  elseif (metdata_format.eq.gfs_metdata) then
    print*,'NCEP metdata detected'
  else
    stop 'Unknown metdata format'
  endif

  ! Read the model grid specifications,
  ! both for the mother domain and eventual nests
  !**********************************************

  if (metdata_format.eq.ecmwf_metdata) call gridcheck_ecmwf
  if (metdata_format.eq.gfs_metdata) call gridcheck_gfs
  call gridcheck_nests

  do i=1,numbwf
    if ( ( useAvailable.eq.0 ).or.(numbwf.eq.1) ) then
      print *,' '
      print *,wfname(i)
      call convertfields( i, metdata_format, dumpPath)
    else
       if (((ldirect*wftime(i).le.0).and. &
             (ldirect*wftime(i+1).gt.0)).or. & 
          (ldirect*wftime(i).gt.0)) then
          print *,' '
          print *,wfname(i)
          call convertfields( i, metdata_format, dumpPath)
       endif
    endif
  end do

  write(*,*) 'CONGRATULATIONS: YOU HAVE SUCCESSFULLY COMPLETED A GRIB2FLE&
       &XPART PREPROCESSING RUN!'

end program grib2flexpart

! This function checks whether input and output directories differ
! It does so by creating tmp file in output directory and by checking for its presence in input one
! It's does this way to avoid the need for relative 2 absolute path expansion and to avoid handling of links
integer function overwritecheck( dump_path, input_path, input_is_path )
  character(len=*) :: dump_path, input_path
  integer :: input_is_path, open_status
  character(len=512) :: tmp_file_name, tmp_file_path, check_file_path
  character(len=64) :: pid, current_time
  logical :: exists

  overwritecheck = 1

  write (pid,*) getpid()
  write (current_time, *) time()
  ! generate tmp file name using PID and timestamp
  !! tmp_file_name = "overwritecheck_"//trim(adjustl(pid))//"_"//trim(adjustl(current_time))//".tmp"
  !! tmp_file_path = trim(dump_path)//"/"//trim(tmp_file_name)

  ! create tmp file in output directory
  !! open(10001, file=trim(tmp_file_path), status="new", action="write", iostat=open_status)
  ! check for tmp file
  !! if ( open_status /= 0 ) then
  !!   print *, "Output directory does not exist or is not writeable"
  !!   print *, "File "//trim(tmp_file_path)//", iostat=",open_status
  !!   print *, "PID: ", pid, getpid()
  !!   print *, "current_time: ", current_time

  !!   stop 'Error: Incorrect arguments'
  !! endif
    
  ! generate tmp file name in input directory
  if ( input_is_path == 1) then
    !! check_file_path = trim(input_path)//"/"//trim(tmp_file_name)
    stop 'Error: Incorrect arguments; input file is path'
  else
    if ( scan(input_path, '/') == 0 ) then
       check_file_path=trim(dump_path)//"/"//trim(input_path)
    else
       check_file_path=trim(dump_path)//trim(input_path(scan(input_path, '/', .TRUE.):))
    endif
  endif
  !check for file presence
  print *, "Check if output file "//trim(check_file_path)//" exists ..."
  inquire(file=TRIM(check_file_path), exist=exists)
  ! delete tmp file
  !! close(10001, status='DELETE')
  if ( exists ) then
    overwritecheck = -1
    print *, "Warning: Output file "//trim(check_file_path)//" exists"
    stop 'Please remove this file if the output directory is correct'
  endif
  
end function
