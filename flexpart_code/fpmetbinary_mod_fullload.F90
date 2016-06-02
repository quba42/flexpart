MODULE fpmetbinary_mod

  !*****************************************************************************
  !                                                                            *
  !     Contains data and routines for dumping and loading processed met       *
  !     fields.                                                                *
  !     Authors Don Morton (Don.Morton@borealscicomp.com)                      *
  !             Delia Arnold (deliona.arnold@gmail.com)                        *
  !                                                                            *
  !     15 Sep 2015                                                            *
  !                                                                            *
  !     Currently, the only data being dumped and loaded has data structures   *
  !     defined in com_mod.f90.  In the future, perhaps it will be necessary   *
  !     to use data structures from other parts of the FLEXPART code system.   *
  !                                                                            *
  !     Note that these routines need more robustness.  For example, what      *
  !     what happens if the filename can't be read or written.  Or, what       *
  !     happens if a read or write fails in any way.  Right now, it's crash    *
  !     city.                                                                  *
  !                                                                            *
  !*****************************************************************************

    USE com_mod

    IMPLICIT NONE

    ! Users may want to change these IO Unit values if they conflict with other parts
    ! of code
    INTEGER, PARAMETER :: IOUNIT_DUMP = 33, IOUNIT_LOAD = 34
    PRIVATE IOUNIT_DUMP, IOUNIT_LOAD, fpio


CONTAINS

  !*****************************************************************************
  !                                                                            *
  !    Subroutines fpdump() and fpload() provide the public interface to       *
  !    this module functionality.  I created the PRIVATE fpio() because I      *
  !    wanted all interactions with variables to be in one place.  The read    *
  !    and write operations need to be done in exactly the same sequence, so   *
  !    I felt like keeping them in the same routine would at least allow for   *
  !    coders to more easily compare the two sequences than if they were       *
  !    separate.                                                               *
  !                                                                            *
  !*****************************************************************************


    SUBROUTINE fpdump(filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename

        OPEN(IOUNIT_DUMP, file=filename, action='WRITE', status='REPLACE', form="unformatted", access="stream")
        CALL fpio(IOUNIT_DUMP, 'DUMP')
        CLOSE(IOUNIT_DUMP)
    END SUBROUTINE fpdump

    SUBROUTINE fpload(filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename

        OPEN(IOUNIT_LOAD, file=filename, action='READ', status='OLD', form="unformatted", access="stream")
        CALL fpio(IOUNIT_LOAD, 'LOAD')
        CLOSE(IOUNIT_LOAD)
    END SUBROUTINE fpload





    SUBROUTINE fpio(iounit, op)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: iounit
        CHARACTER(LEN=4), INTENT(IN) :: op

        if (op == 'DUMP') THEN
#ifndef QUICKDUMP
            WRITE(iounit) path, length
            WRITE(iounit) ibdate, ibtime, iedate, ietime, bdate, edate
            WRITE(iounit) ldirect, ideltas, loutstep, loutaver, loutsample, method, lsynctime, outstep
            WRITE(iounit) ctl, fine, ifine, iout, ipin, iflux, mdomainfill
            WRITE(iounit) mquasilag,nested_output,ind_source,ind_receptor
            WRITE(iounit) ind_rel,ind_samp,ioutputforeachrelease,linit_cond, turbswitch
            WRITE(iounit) mintime,itsplit, lsubgrid,lconvection,lagespectra
            WRITE(iounit) nageclass, lage, gdomainfill
            WRITE(iounit) compoint, numpoint, specnum
            WRITE(iounit) decay, weta, wetb, reldiff, henry, f0
            WRITE(iounit) density, dquer, dsigma, vsetaver, cunningham, weightmolar
            WRITE(iounit) vset, schmi, fract, ri, rac, rcl ,rgs, rlu, rm, dryvel, kao, ohreact
            WRITE(iounit) spec_ass, area_hour, point_hour, area_dow, point_dow
            WRITE(iounit) nspec, maxpointspec_act, species
            WRITE(iounit) nx_we, ny_sn, numcolumn, numcolumn_we, numcolumn_sn
            WRITE(iounit) zcolumn_we, zcolumn_sn, xmassperparticle, acc_mass_we, acc_mass_sn
            WRITE(iounit) numbwf, wftime, lwindinterv, wfname, wfspec
            WRITE(iounit) memtime, memind
            WRITE(iounit) nx,ny,nxmin1,nymin1,nxfield,nuvz,nwz,nz,nmixz,nlev_ec
            WRITE(iounit) dx,dy,xlon0,ylat0,dxconst,dyconst,height
            WRITE(iounit) akm, bkm, akz, bkz, aknew, bknew
            WRITE(iounit) oro, excessoro, lsm, xlanduse
            WRITE(iounit) uu, vv, uupol, vvpol, ww, tt, qv, pv, rho, drhodz, tth, qvh, pplev, clouds, cloudsh
            WRITE(iounit) ps, sd, msl, tcc, u10, v10, tt2, td2, lsprec, convprec
            WRITE(iounit) sshf, ssr, surfstr, ustar, wstar, hmix, tropopause, oli, diffk
            WRITE(iounit) vdep, numbnests, wfnamen, wfspecn, nxn, nyn, dxn, dyn, xlon0n, ylat0n
            WRITE(iounit) oron, excessoron, lsmn, xlandusen
            WRITE(iounit) uun, vvn, wwn, ttn, qvn, pvn, cloudsn, cloudsnh, rhon, drhodzn, tthn, qvhn
            WRITE(iounit) psn, sdn, msln, tccn, u10n, v10n, tt2n, td2n
            WRITE(iounit) lsprecn, convprecn, sshfn, ssrn, surfstrn, ustarn, wstarn
            WRITE(iounit) hmixn, tropopausen, olin, diffkn, vdepn
            WRITE(iounit) xresoln, yresoln, xln, yln, xrn, yrn
            WRITE(iounit) xglobal, sglobal, nglobal, switchnorthg, switchsouthg
            WRITE(iounit) southpolemap, northpolemap
            WRITE(iounit) landinvent, z0
            WRITE(iounit) numxgrid, numygrid, numzgrid, dxout, dyout, outlon0, outlat0, xoutshiftn, youtshiftn
            WRITE(iounit) DEP, DRYDEP, DRYDEPSPEC, WETDEP, OHREA, ASSSPEC
            WRITE(iounit) xreceptor, yreceptor, receptorarea, creceptor, receptorname, numreceptor
            WRITE(iounit) numpart, itra1, npoint, nclass, idt, itramem, itrasplit, numparticlecount
            WRITE(iounit) xtra1, ytra1, ztra1, xmass1, rannumb
#endif

            ! This IO was actually done above, but it's put in here to help serve to test this
            ! routine.  After all the writes and reads, it comes to this one, and a program that wants
            ! to test all this can use this to insure that the values of uu, vv, ww read from the binary file
            ! are the same as those that were written in
            WRITE(iounit) uu, vv, ww


        ELSE   ! We assume op is 'LOAD'
#ifndef QUICKDUMP
            READ(iounit) path, length
            READ(iounit) ibdate, ibtime, iedate, ietime, bdate, edate
            READ(iounit) ldirect, ideltas, loutstep, loutaver, loutsample, method, lsynctime, outstep
            READ(iounit) ctl, fine, ifine, iout, ipin, iflux, mdomainfill
            READ(iounit) mquasilag,nested_output,ind_source,ind_receptor
            READ(iounit) ind_rel,ind_samp,ioutputforeachrelease,linit_cond, turbswitch
            READ(iounit) mintime,itsplit, lsubgrid,lconvection,lagespectra
            READ(iounit) nageclass, lage, gdomainfill
            READ(iounit) compoint, numpoint, specnum
            READ(iounit) decay, weta, wetb, reldiff, henry, f0
            READ(iounit) density, dquer, dsigma, vsetaver, cunningham, weightmolar
            READ(iounit) vset, schmi, fract, ri, rac, rcl ,rgs, rlu, rm, dryvel, kao, ohreact
            READ(iounit) spec_ass, area_hour, point_hour, area_dow, point_dow
            READ(iounit) nspec, maxpointspec_act, species
            READ(iounit) nx_we, ny_sn, numcolumn, numcolumn_we, numcolumn_sn
            READ(iounit) zcolumn_we, zcolumn_sn, xmassperparticle, acc_mass_we, acc_mass_sn
            READ(iounit) numbwf, wftime, lwindinterv, wfname, wfspec
            READ(iounit) memtime, memind
            READ(iounit) nx,ny,nxmin1,nymin1,nxfield,nuvz,nwz,nz,nmixz,nlev_ec
            READ(iounit) dx,dy,xlon0,ylat0,dxconst,dyconst,height
            READ(iounit) akm, bkm, akz, bkz, aknew, bknew
            READ(iounit) oro, excessoro, lsm, xlanduse
            READ(iounit) uu, vv, uupol, vvpol, ww, tt, qv, pv, rho, drhodz, tth, qvh, pplev, clouds, cloudsh
            READ(iounit) ps, sd, msl, tcc, u10, v10, tt2, td2, lsprec, convprec
            READ(iounit) sshf, ssr, surfstr, ustar, wstar, hmix, tropopause, oli, diffk
            READ(iounit) vdep, numbnests, wfnamen, wfspecn, nxn, nyn, dxn, dyn, xlon0n, ylat0n
            READ(iounit) oron, excessoron, lsmn, xlandusen
            READ(iounit) uun, vvn, wwn, ttn, qvn, pvn, cloudsn, cloudsnh, rhon, drhodzn, tthn, qvhn
            READ(iounit) psn, sdn, msln, tccn, u10n, v10n, tt2n, td2n
            READ(iounit) lsprecn, convprecn, sshfn, ssrn, surfstrn, ustarn, wstarn
            READ(iounit) hmixn, tropopausen, olin, diffkn, vdepn
            READ(iounit) xresoln, yresoln, xln, yln, xrn, yrn
            READ(iounit) xglobal, sglobal, nglobal, switchnorthg, switchsouthg
            READ(iounit) southpolemap, northpolemap
            READ(iounit) landinvent, z0
            READ(iounit) numxgrid, numygrid, numzgrid, dxout, dyout, outlon0, outlat0, xoutshiftn, youtshiftn
            READ(iounit) DEP, DRYDEP, DRYDEPSPEC, WETDEP, OHREA, ASSSPEC
            READ(iounit) xreceptor, yreceptor, receptorarea, creceptor, receptorname, numreceptor
            READ(iounit) numpart, itra1, npoint, nclass, idt, itramem, itrasplit, numparticlecount
            READ(iounit) xtra1, ytra1, ztra1, xmass1, rannumb
#endif

            ! This IO was actually done above, but it's put in here to help serve to test this
            ! routine.  After all the writes and reads, it comes to this one, and a program that wants
            ! to test all this can use this to insure that the values of uu, vv, ww read from the binary file
            ! are the same as those that were written in
            READ(iounit) uu, vv, ww
        ENDIF
    END SUBROUTINE fpio


END MODULE fpmetbinary_mod
