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

program flexpart

  !*****************************************************************************
  !                                                                            *
  !     This is the Lagrangian Particle Dispersion Model FLEXPART.             *
  !     The main program manages the reading of model run specifications, etc. *
  !     All actual computing is done within subroutine timemanager.            *
  !                                                                            *
  !     Author: A. Stohl                                                       *
  !                                                                            *
  !     18 May 1996                                                            *
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

  integer :: i,j,ix,jy,inest
  integer :: idummy = -320

  integer :: metdata_format = unknown_metdata

#ifdef TESTUSEGETFPFIELDS
  ! Data structures used to pirate the data structures filled from
  ! pathnames and AVAILABLE and point to test .fp binary files.

  integer :: idx_wf    ! Stores the index number of the met/.fp file

  ! Full path to the .fp files location
  !character(len=*), parameter :: FPDIR = '/home/morton/basic_ec_nc_testing/'
  character(len=*), parameter :: FPDIR = '/home/morton/'
  !character(len=*), parameter :: FPDIR = '/home/morton/FPForward/'
  !character(len=*), parameter :: FPDIR = '/home/morton/FPTinyFwd/'
  !character(len=*), parameter :: FPDIR = '/home/morton/ecmwf_tiny_fp/'

  character(len=512) :: fpname  ! Stores name of a single .fp file
#endif

  ! Generate a large number of random numbers
  !******************************************

  do i=1,maxrand-1,2
    call gasdev1(idummy,rannumb(i),rannumb(i+1))
  end do
  call gasdev1(idummy,rannumb(maxrand),rannumb(maxrand-1))

  ! Print the GPL License statement
  !*******************************************************
#if defined CTBTO
  print*,'Welcome to FLEXPART Version 9.3.1f CTBTO'
#else
  print*,'Welcome to FLEXPART Version 9.3.1f'
#endif

  print*,'FLEXPART is free software released under the GNU Genera'// &
       'l Public License.'

  ! Read the pathnames where input/output files are stored
  !*******************************************************

  call readpaths

  ! Read the user specifications for the current model run
  !*******************************************************

  call readcommand


  ! Read the age classes to be used
  !********************************

  call readageclasses


  ! Read, which wind fields are available within the modelling period
  !******************************************************************

  call readavailable





#ifdef TESTUSEGETFPFIELDS
  ! THIS IS ONLY USED FOR TESTING  
  ! Hard code the path to .fp binary files into the data structures as if they
  ! were read in from pathnames and the AVAILABLE file up above in 
  ! readpaths() and readavailable().  In short, this replaces the path and
  ! names of the GRIB met files with those of the .fp binary files.
  ! THIS IS ONLY USED FOR TESTING  

  ! I think it might be problematic that the AVAILABLE file just might have
  ! more files than there are .fp files, because the generation of the .fp
  ! files may have taken place with a subset of the full AVAILABLE file list.

  ! These variables, used below, come from com_mod 
  !
  !             --- numbwf, wfname, path, length

  ! This test code needs to come after gridcheck() and gridcheck_nests(), since
  ! at the time those are called, the data structures still need to hold path
  ! names to GRIB files

  print *, 'Before overwriting...'
  print *, 'path(3): ', path(3)
  print *, 'length(3): ', length(3)

  ! Get and store the base name of the metfiles read in from AVAILABLE
  DO idx_wf = 1, numbwf
      wfname(idx_wf) = TRIM(wfname(idx_wf)) 
      PRINT *, wfname(idx_wf)
  END DO
  
  ! The values in path and length arrays were meant to store met file path
  ! and length of path.  Replace with the .fp file path and length of path
  path(3) = FPDIR
  length(3) = LEN(TRIM(path(3)))

  print *, 'After overwriting...'
  print *, 'path(3): ', path(3)
  print *, 'length(3): ', length(3)

  ! Add the .fp extension to each of the met file base names
  DO idx_wf = 1, numbwf
      fpname = TRIM(wfname(idx_wf)) // ".fp"
      wfname(idx_wf) = TRIM(fpname)
      PRINT *, wfname(idx_wf)
  END DO

  ! At this point, the path and filename data structures have been overwritten
  ! to point to the .fp files, rather than the metfiles

  !!!!!!!!!!!!!!! CALL TO fpgridcheck()
  CALL fpgridcheck
#else

  if ( preprocessed_metdata.eq.1 ) then
    call fpgridcheck
  else
    ! Detect metdata format
    !******************************************************************
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
  endif
#endif

  ! Read the output grid specifications
  !************************************
  call readoutgrid
  if (nested_output.eq.1) call readoutgrid_nest


  ! Read the receptor points for which extra concentrations are to be calculated
  !*****************************************************************************
  call readreceptors


  ! Read the physico-chemical species property table
  !*************************************************
  !SEC: now only needed SPECIES are read in readreleases.f
  !call readspecies


  ! Read the landuse inventory
  !***************************
  call readlanduse


  ! Assign fractional cover of landuse classes to each ECMWF grid point
  !********************************************************************
  call assignland



  ! Read the coordinates of the release locations
  !**********************************************

  call readreleases

  ! Read and compute surface resistances to dry deposition of gases
  !****************************************************************

  call readdepo


  ! Convert the release point coordinates from geografical to grid coordinates
  !***************************************************************************

  call coordtrafo


  ! Initialize all particles to non-existent
  !*****************************************

  do j=1,maxpart
    itra1(j)=-999999999
  end do

  ! For continuation of previous run, read in particle positions
  !*************************************************************

  if (ipin.eq.1) then
    call readpartpositions
  else
    numpart=0
    numparticlecount=0
  endif


  ! Calculate volume, surface area, etc., of all output grid cells
  ! Allocate fluxes and OHfield if necessary
  !***************************************************************

  call outgrid_init
  if (nested_output.eq.1) call outgrid_init_nest


  ! Read the OH field
  !******************

  if (OHREA.eqv..TRUE.) &
       call readOHfield

  ! Write basic information on the simulation to a file "header"
  ! and open files that are to be kept open throughout the simulation
  !******************************************************************

  call writeheader
  if (nested_output.eq.1) call writeheader_nest
  open(unitdates,file=path(2)(1:length(2))//'dates')
  call openreceptors
  if ((iout.eq.4).or.(iout.eq.5)) call openouttraj


  ! Releases can only start and end at discrete times (multiples of lsynctime)
  !***************************************************************************

  do i=1,numpoint
    ireleasestart(i)=nint(real(ireleasestart(i))/ &
         real(lsynctime))*lsynctime
    ireleaseend(i)=nint(real(ireleaseend(i))/ &
         real(lsynctime))*lsynctime
  end do


  ! Initialize cloud-base mass fluxes for the convection scheme
  !************************************************************

  do jy=0,nymin1
    do ix=0,nxmin1
      cbaseflux(ix,jy)=0.
    end do
  end do
  do inest=1,numbnests
    do jy=0,nyn(inest)-1
      do ix=0,nxn(inest)-1
        cbasefluxn(ix,jy,inest)=0.
      end do
    end do
  end do


  ! Calculate particle trajectories
  !********************************


  call timemanager(metdata_format)


  write(*,*) 'CONGRATULATIONS: YOU HAVE SUCCESSFULLY COMPLETED A FLE&
       &XPART MODEL RUN!'

end program flexpart
