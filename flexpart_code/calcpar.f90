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

subroutine calcpar_ecmwf(n,uuh,vvh,pvh)
  !                   i  i   i   o
  !*****************************************************************************
  !                                                                            *
  !     Computation of several boundary layer parameters needed for the        *
  !     dispersion calculation and calculation of dry deposition velocities.   *
  !     All parameters are calculated over the entire grid.                    *
  !                                                                            *
  !     Author: A. Stohl                                                       *
  !                                                                            *
  !     21 May 1995                                                            *
  !                                                                            *
  ! ------------------------------------------------------------------         *
  !     Petra Seibert, Feb 2000:                                               *
  !     convection scheme:                                                     *
  !     new variables in call to richardson                                    *
  !                                                                            *
  !*****************************************************************************
  !  Changes, Bernd C. Krueger, Feb. 2001:
  !   Variables tth and qvh (on eta coordinates) in common block
  !*****************************************************************************
  !  Changes Arnold, D. and Morton, D. (2015):                                 *
  !   -- description of local and common variables                             *
  !*****************************************************************************
  ! Functions:                                                                 *
  ! scalev             computation of ustar                                    *
  ! obukhov            computatio of Obukhov length                            *
  !                                                                            *
  !*****************************************************************************

  use par_mod
  use com_mod

  implicit none

  !***********************************************************************
  ! Subroutine Parameters:                                               *
  !    input                                                             *
  ! n                   temporal index for meteorological fields (1 to 3)*  
  ! uuh,vvh, wwh        wind components in ECMWF model levels            *  
  integer :: n
  real :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)  
  real :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)  
  real :: pvh(0:nxmax-1,0:nymax-1,nuvzmax)

  !***********************************************************************
  ! Local variables                                                      *
  !                                                                      *
  ! ttlev 
  ! qvlev
  ! * obukhov_ecmwf      subroutine/function to calculate Obukhov length *
  ! * scalev             subroutine/function to calculate ustar          *
  ! ol                   obukhov length
  ! hmixplus             maximum lifting from availiable kinetic enrgy   *
  ! ulev, vlev           wind speed at model levels                      *
  ! ew                   subroutine/function to calculate saturation     *
  !                         water vaport for a given air temperature     * 
  ! rh                   relative humidity at surface                    *
  ! vd                   deposition velocity from all species            *
  ! subsceff             excess hmin due to subgrid effects              *
  ! ylat                 temporary latitude                              *
  ! altmin               minimum height of the tropopause                *
  ! tvoldm pold, zold    temporary variables to keep previous values     *
  ! pint                 pressure on model levels                        *
  ! tv                   virtual temperature on model levels             *
  ! zlev                 height of model levels                          *
  real :: ttlev(nuvzmax),qvlev(nuvzmax),obukhov_ecmwf,scalev,ol,hmixplus
  real :: ulev(nuvzmax),vlev(nuvzmax),ew,rh,vd(maxspec),subsceff,ylat
  real :: altmin,tvold,pold,zold,pint,tv,zlev(nuvzmax)

  ! Other variables:
  ! ix,jy,kz,i,lz,kzmin     loop control indices in each direction       *
  integer :: ix,jy,i,kz,lz,kzmin

  !***********************************************************************

  !***********************************************************************
  ! Local constants                                                      *
  real,parameter :: const=r_air/ga
  !***********************************************************************


  !***********************************************************************
  ! Global variables                                                     *
  !     from par_mod and com_mod                                         *
  ! ustar [m/s]          friction velocity                               *
  ! oli [m]              inverse Obukhov length (1/L)                    *
  ! hmix  [m]            mixing height                                   *
  ! wstar  [m/s]          convective velocity scale                      *
  ! ustar  [m/s]          friction velocity                              *
  ! z0      roughness length for the landuse classes                     *
  ! tropopause   [m]       altitude of thermal tropopause                *
  ! nx, ny  actual dimensions of  wind fields in x and y direction       *
  ! dx, dy  grid distances in x,y direction                              *
  ! akm, bkm  coefficients which regulate vertical discretization of ecmwf*
  ! akz, bkz  model discretization coeffizients at the centre of layers   *
  ! ps        surface pressure                                            * 
  ! tt2       2-m temperature                                             *
  ! tt2d      2-m dew temperature                                         *
  ! sshf      surface sensible heat flux                                  *
  ! surfstr   surface stress                                              *
  ! convprec  convective precip                                           *
  ! lsprec    large scale precip                                          *
  ! sd        snow depth                                                  *
  ! ssr      surface solar radiation                                      *
  ! xlon0, ylat0  geographical longitude/latitude of lower left grid point*
  ! 
  !***********************************************************************

!-----------------------------------------------------------------------------


  !write(*,*) 'in calcpar writting snowheight'
  !***********************************
  ! for test: write out snow depths

  ! open(4,file='slandusetest',form='formatted')
  ! do 5 ix=0,nxmin1
  !5       write (4,*) (sd(ix,jy,1,n),jy=0,nymin1)
  !  close(4)


  ! Loop over entire grid
  !**********************

  do jy=0,nymin1

  ! Set minimum height for tropopause
  !**********************************

    ylat=ylat0+real(jy)*dy
    if ((ylat.ge.-20.).and.(ylat.le.20.)) then
      altmin = 5000.
    else
      if ((ylat.gt.20.).and.(ylat.lt.40.)) then
        altmin=2500.+(40.-ylat)*125.
      else if ((ylat.gt.-40.).and.(ylat.lt.-20.)) then
        altmin=2500.+(40.+ylat)*125.
      else
        altmin=2500.
      endif
    endif

    do ix=0,nxmin1

  ! 1) Calculation of friction velocity
  !************************************

      ustar(ix,jy,1,n)=scalev(ps(ix,jy,1,n),tt2(ix,jy,1,n), &
           td2(ix,jy,1,n),surfstr(ix,jy,1,n))
      if (ustar(ix,jy,1,n).le.1.e-8) ustar(ix,jy,1,n)=1.e-8

  ! 2) Calculation of inverse Obukhov length scale
  !***********************************************

      ol=obukhov_ecmwf(ps(ix,jy,1,n),tt2(ix,jy,1,n),td2(ix,jy,1,n), &
           tth(ix,jy,2,n),ustar(ix,jy,1,n),sshf(ix,jy,1,n),akm,bkm)
      if (ol.ne.0.) then
        oli(ix,jy,1,n)=1./ol
      else
        oli(ix,jy,1,n)=99999.
      endif


  ! 3) Calculation of convective velocity scale and mixing height
  !**************************************************************

      do i=1,nuvz
        ulev(i)=uuh(ix,jy,i)
        vlev(i)=vvh(ix,jy,i)
        ttlev(i)=tth(ix,jy,i,n)
        qvlev(i)=qvh(ix,jy,i,n)
      end do

      call richardson_ecmwf(ps(ix,jy,1,n),ustar(ix,jy,1,n),ttlev,qvlev, &
           ulev,vlev,nuvz,akz,bkz,sshf(ix,jy,1,n),tt2(ix,jy,1,n), &
           td2(ix,jy,1,n),hmix(ix,jy,1,n),wstar(ix,jy,1,n),hmixplus)

      if(lsubgrid.eq.1) then
        subsceff=min(excessoro(ix,jy),hmixplus)
      else
        subsceff=0.0
      endif
  !
  ! CALCULATE HMIX EXCESS ACCORDING TO SUBGRIDSCALE VARIABILITY AND STABILITY
  !
      hmix(ix,jy,1,n)=hmix(ix,jy,1,n)+subsceff
      hmix(ix,jy,1,n)=max(hmixmin,hmix(ix,jy,1,n)) ! set minimum PBL height
      hmix(ix,jy,1,n)=min(hmixmax,hmix(ix,jy,1,n)) ! set maximum PBL height

  ! 4) Calculation of dry deposition velocities
  !********************************************

      if (DRYDEP) then
  ! Sabine Eckhardt, Dec 06: use new index for z0 for water depending on
  ! windspeed
        z0(7)=0.016*ustar(ix,jy,1,n)*ustar(ix,jy,1,n)/ga

  ! Calculate relative humidity at surface
  !***************************************
        rh=ew(td2(ix,jy,1,n))/ew(tt2(ix,jy,1,n))

        call getvdep(n,ix,jy,ustar(ix,jy,1,n), &
             tt2(ix,jy,1,n),ps(ix,jy,1,n),1./oli(ix,jy,1,n), &
             ssr(ix,jy,1,n),rh,lsprec(ix,jy,1,n)+convprec(ix,jy,1,n), &
             sd(ix,jy,1,n),vd)

        do i=1,nspec
          vdep(ix,jy,i,n)=vd(i)
        end do

      endif

  !******************************************************
  ! Calculate height of thermal tropopause (Hoinka, 1997)
  !******************************************************

  ! 1) Calculate altitudes of ECMWF model levels
  !*********************************************

      tvold=tt2(ix,jy,1,n)*(1.+0.378*ew(td2(ix,jy,1,n))/ &
           ps(ix,jy,1,n))
      pold=ps(ix,jy,1,n)
      zold=0.
      do kz=2,nuvz
        pint=akz(kz)+bkz(kz)*ps(ix,jy,1,n)  ! pressure on model layers
        tv=tth(ix,jy,kz,n)*(1.+0.608*qvh(ix,jy,kz,n))

        if (abs(tv-tvold).gt.0.2) then
         zlev(kz)=zold+const*log(pold/pint)*(tv-tvold)/log(tv/tvold)
        else
          zlev(kz)=zold+const*log(pold/pint)*tv
        endif
        tvold=tv
        pold=pint
        zold=zlev(kz)
      end do

  ! 2) Define a minimum level kzmin, from which upward the tropopause is
  !    searched for. This is to avoid inversions in the lower troposphere
  !    to be identified as the tropopause
  !************************************************************************

      do kz=1,nuvz
        if (zlev(kz).ge.altmin) then
          kzmin=kz
          goto 45
        endif
      end do
45    continue

  ! 3) Search for first stable layer above minimum height that fulfills the
  !    thermal tropopause criterion
  !************************************************************************

      do kz=kzmin,nuvz
        do lz=kz+1,nuvz
          if ((zlev(lz)-zlev(kz)).gt.2000.) then
            if (((tth(ix,jy,kz,n)-tth(ix,jy,lz,n))/ &
                 (zlev(lz)-zlev(kz))).lt.0.002) then
              tropopause(ix,jy,1,n)=zlev(kz)
              goto 51
            endif
            goto 50
          endif
        end do
50      continue
      end do
51    continue


    end do
  end do

  ! Calculation of potential vorticity on 3-d grid
  !***********************************************

  call calcpv(n,uuh,vvh,pvh)


end subroutine calcpar_ecmwf
