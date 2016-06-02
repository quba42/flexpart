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

subroutine calcpar_nests(n,uuhn,vvhn,pvhn,metdata_format)
  !                         i  i    i    o
  !*****************************************************************************
  !                                                                            *
  !     Computation of several boundary layer parameters needed for the        *
  !     dispersion calculation and calculation of dry deposition velocities.   *
  !     All parameters are calculated over the entire grid.                    *
  !     This routine is similar to calcpar, but is used for the nested grids.  *
  !                                                                            *
  !     Author: A. Stohl                                                       *
  !                                                                            *
  !     8 February 1999                                                        *
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
  ! uuhn,vvhn, wwhn     wind components in ECMWF model levels            *  
  ! metdata_format      to identify the met data
  integer :: n, metdata_format
  real :: uuhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: vvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
  real :: pvhn(0:nxmaxn-1,0:nymaxn-1,nuvzmax,maxnests)
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
  real :: ttlev(nuvzmax),qvlev(nuvzmax),obukhov_ecmwf,obukhov_gfs,scalev,ol,hmixplus
  real :: ulev(nuvzmax),vlev(nuvzmax),ew,rh,vd(maxspec),subsceff,ylat
  real :: altmin,tvold,pold,zold,pint,tv,zlev(nuvzmax)

  ! Other variables:
  ! ix,jy,kz,i,lz,kzmin     loop control indices in each direction       *
  integer :: ix,jy,i,l,kz,lz,kzmin
  !***********************************************************************

  !***********************************************************************
  ! Local constants                                                      *
  real,parameter :: const=r_air/ga
  !***********************************************************************

  !***********************************************************************
  ! Global variables                                                     *
  !     from par_mod and com_mod                                         *
  ! olin [m]              inverse Obukhov length (1/L)                    *
  ! hmixn  [m]            mixing height                                   *
  ! wstarn  [m/s]          convective velocity scale                      *
  ! ustarn  [m/s]          friction velocity                              *
  ! z0n      roughness length for the landuse classes                     *
  ! tropopausen   [m]       altitude of thermal tropopause                *
  ! nxn, nyn  actual dimensions of nested wind fields in x and y direction*
  ! dxn, dyn  grid distances in x,y direction for the nested grids        *
  ! akm, bkm  coefficients which regulate vertical discretization of ecmwf*
  ! akz, bkz  model discretization coeffizients at the centre of layers   *
  ! psn       surface pressure for nests                                  * 
  ! tt2n      2-m temperature for nests                                   *
  ! tt2dn     2-m dew temperature for nests                               *
  ! sshfn     surface sensible heat flux for nests                        *
  ! surfstrn  surface stress fro nests                                    *
  ! numbnests number of nested grids                                      *
  ! convprecn convective precip on the nested grid                        *
  ! lsprecn   large scale precip on the nested grid                       *
  ! sdn       snow depth on the nested grid                               *
  ! ssrn      surface solar radiation for nests                           *
  ! xlon0n, ylat0n  geographical longitude/latitude of lower left grid    *
  !                  point of nested wind fields                          *
  ! 
  !************************************************************************

!-----------------------------------------------------------------------------


  ! Loop over all nests
  !********************

  do l=1,numbnests

  ! Loop over entire grid
  !**********************

  do jy=0,nyn(l)-1

  ! Set minimum height for tropopause
  !**********************************

    ylat=ylat0n(l)+real(jy)*dyn(l)
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

    do ix=0,nxn(l)-1

  ! 1) Calculation of friction velocity
  !************************************

      ustarn(ix,jy,1,n,l)=scalev(psn(ix,jy,1,n,l),tt2n(ix,jy,1,n,l), &
           td2n(ix,jy,1,n,l),surfstrn(ix,jy,1,n,l))

  ! 2) Calculation of inverse Obukhov length scale
  !***********************************************

      if (metdata_format.eq.ecmwf_metdata) then 
        ol=obukhov_ecmwf(psn(ix,jy,1,n,l),tt2n(ix,jy,1,n,l), &
           td2n(ix,jy,1,n,l),tthn(ix,jy,2,n,l),ustarn(ix,jy,1,n,l), &
           sshfn(ix,jy,1,n,l),akm,bkm)
      endif
      if (metdata_format.eq.gfs_metdata) then
        ol=obukhov_gfs(psn(ix,jy,1,n,l),tt2n(ix,jy,1,n,l), &
           td2n(ix,jy,1,n,l),tthn(ix,jy,2,n,l),ustarn(ix,jy,1,n,l), &
           sshfn(ix,jy,1,n,l),akm,bkm)
      endif
      if (ol.ne.0.) then
        olin(ix,jy,1,n,l)=1./ol
      else
        olin(ix,jy,1,n,l)=99999.
      endif


  ! 3) Calculation of convective velocity scale and mixing height
  !**************************************************************

      do i=1,nuvz
        ulev(i)=uuhn(ix,jy,i,l)
        vlev(i)=vvhn(ix,jy,i,l)
        ttlev(i)=tthn(ix,jy,i,n,l)
        qvlev(i)=qvhn(ix,jy,i,n,l)
      end do

      if ( metdata_format.eq.ecmwf_metdata) then
        call richardson_ecmwf(psn(ix,jy,1,n,l),ustarn(ix,jy,1,n,l),ttlev, &
           qvlev,ulev,vlev,nuvz,akz,bkz,sshfn(ix,jy,1,n,l), &
           tt2n(ix,jy,1,n,l),td2n(ix,jy,1,n,l),hmixn(ix,jy,1,n,l), &
           wstarn(ix,jy,1,n,l),hmixplus)
      endif
      if ( metdata_format.eq.gfs_metdata) then
        call richardson_gfs(psn(ix,jy,1,n,l),ustarn(ix,jy,1,n,l),ttlev, &
           qvlev,ulev,vlev,nuvz,akz,bkz,sshfn(ix,jy,1,n,l), &
           tt2n(ix,jy,1,n,l),td2n(ix,jy,1,n,l),hmixn(ix,jy,1,n,l), &
           wstarn(ix,jy,1,n,l),hmixplus)
      endif


      if(lsubgrid.eq.1) then
        subsceff=min(excessoron(ix,jy,l),hmixplus)
      else
        subsceff=0
      endif
  !
  ! CALCULATE HMIX EXCESS ACCORDING TO SUBGRIDSCALE VARIABILITY AND STABILITY
  !
      hmixn(ix,jy,1,n,l)=hmixn(ix,jy,1,n,l)+subsceff
      hmixn(ix,jy,1,n,l)=max(hmixmin,hmixn(ix,jy,1,n,l)) ! minim PBL height
      hmixn(ix,jy,1,n,l)=min(hmixmax,hmixn(ix,jy,1,n,l)) ! maxim PBL height


  ! 4) Calculation of dry deposition velocities
  !********************************************

      if (DRYDEP) then
        z0(4)=0.016*ustarn(ix,jy,1,n,l)*ustarn(ix,jy,1,n,l)/ga
        z0(9)=0.016*ustarn(ix,jy,1,n,l)*ustarn(ix,jy,1,n,l)/ga

  ! Calculate relative humidity at surface
  !***************************************
        rh=ew(td2n(ix,jy,1,n,l))/ew(tt2n(ix,jy,1,n,l))

        call getvdep_nests(n,ix,jy,ustarn(ix,jy,1,n,l), &
             tt2n(ix,jy,1,n,l),psn(ix,jy,1,n,l),1./olin(ix,jy,1,n,l), &
             ssrn(ix,jy,1,n,l),rh,lsprecn(ix,jy,1,n,l)+ &
             convprecn(ix,jy,1,n,l),sdn(ix,jy,1,n,l),vd,l)

        do i=1,nspec
          vdepn(ix,jy,i,n,l)=vd(i)
        end do

      endif

  !******************************************************
  ! Calculate height of thermal tropopause (Hoinka, 1997)
  !******************************************************

  ! 1) Calculate altitudes of ECMWF model levels
  !*********************************************

      tvold=tt2n(ix,jy,1,n,l)*(1.+0.378*ew(td2n(ix,jy,1,n,l))/ &
           psn(ix,jy,1,n,l))
      pold=psn(ix,jy,1,n,l)
      zold=0.
      do kz=2,nuvz
        pint=akz(kz)+bkz(kz)*psn(ix,jy,1,n,l)  ! pressure on model layers
        tv=tthn(ix,jy,kz,n,l)*(1.+0.608*qvhn(ix,jy,kz,n,l))

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
            if (((tthn(ix,jy,kz,n,l)-tthn(ix,jy,lz,n,l))/ &
                 (zlev(lz)-zlev(kz))).lt.0.002) then
              tropopausen(ix,jy,1,n,l)=zlev(kz)
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


    call calcpv_nests(l,n,uuhn,vvhn,pvhn)

  end do


end subroutine calcpar_nests
