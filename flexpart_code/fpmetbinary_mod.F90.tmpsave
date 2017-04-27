MODULE fpmetbinary_mod

  !*****************************************************************************
  !                                                                            *
  !     Contains data and routines for dumping and loading processed met       *
  !     fields.                                                                *
  !     Authors Don Morton (Don.Morton@borealscicomp.com)                      *
  !             Delia Arnold (deliona.arnold@gmail.com)                        *
  !                                                                            *
  !     07 Oct 2016                                                            *
  !                                                                            *
  !     Most of the data structures from com_mod.f90 that are dumped and       *
  !     loaded have a final dimension of size two, so that they may hold data  *
  !     from two met files.  When we dump the contents into a .fp file, we     *
  !     need to specify which of the two to dump.  Likewise, when we load      *
  !     from a .fp file, we need to specify which of the two possible indices  *
  !     to load into.                                                          *
  !                                                                            *
  !     Note that these routines need more robustness.  For example, what      *
  !     what happens if the filename can't be read or written.  Or, what       *
  !     happens if a read or write fails in any way.  Right now, it's crash    *
  !     city.                                                                  *
  !                                                                            *
  !     Recent enhancements (07 Oct 2016) DJM:                                 *
  !                                                                            *
  !     - file format changed so that compiled dimensions are output, and      *
  !       during input these same dimensions are compared with the dimensions  *
  !       compiled into the flexpart that is reading it.  A discrepancy        *
  !       causes abort, so that time isn't wasted reading an incompatible      *
  !       file.                                                                *
  !                                                                            *
  !     - file format changed so that first item is an 8-character string      *
  !       depicting the version of the preprocessed file format.               *
  !       An inconsistency between a detected and expected string results      *
  !       in program abort.                                                    *
  !                                                                            *
  !       *** IMPORTANT *** - when the format of the preprocessed output is    *
  !       modified in any way, be sure to change the version string below,     *
  !       PREPROC_FORMAT_VERSION_STR, so that attempts to read the output      *
  !       with a different format version will cause an abort.                 *
  !                                                                            *
  !*****************************************************************************

    USE com_mod
    USE conv_mod
    USE par_mod, ONLY : nxmax, nymax, nzmax, nuvzmax, nwzmax

    IMPLICIT NONE

    ! Users may want to change these IO Unit values if they conflict with other parts
    ! of code
    INTEGER, PARAMETER :: IOUNIT_DUMP = 33, IOUNIT_LOAD = 34, &
                          IOUNIT_TEXTOUT = 35

    ! When a change is made to the format of the preprocessed file, such that
    ! this routine will not be able to read a previous version, this version
    ! string should be modified
    CHARACTER(LEN=12), PARAMETER :: PREPROC_FORMAT_VERSION_STR = 'FP_p-9.3.1'//char(0)

    PRIVATE IOUNIT_DUMP, IOUNIT_LOAD, IOUNIT_TEXTOUT, fpio,    &
&           PREPROC_FORMAT_VERSION_STR


CONTAINS

  !*****************************************************************************
  !                                                                            *
  !    Subroutines fpmetbinary_dump() and fpmetbinary_load() provide the       *
  !    public interface to                                                     *
  !    this module functionality.  I created the PRIVATE fpio() because I      *
  !    wanted all interactions with variables to be in one place.  The read    *
  !    and write operations need to be done in exactly the same sequence, so   *
  !    I felt like keeping them in the same routine would at least allow for   *
  !    coders to more easily compare the two sequences than if they were       *
  !    separate.                                                               *
  !                                                                            *
  !    As mentioned above, the dumps and loads will, for most variables,       *
  !    need to refer to one of two index values for the last dimension of      *
  !    the array.                                                              *
  !                                                                            *
  !*****************************************************************************


    SUBROUTINE fpmetbinary_dump(filename, cm_index)
        CHARACTER(LEN=*), INTENT(IN) :: filename  ! Full path for file
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        INTEGER millisecs_start, millisecs_stop, count_rate, count_max

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
        OPEN(IOUNIT_DUMP, file=filename, action='WRITE', status='REPLACE', form="unformatted", access="stream")
        CALL fpio(IOUNIT_DUMP, 'DUMP', cm_index)
        CLOSE(IOUNIT_DUMP)
        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)

        !PRINT *, 'Dump walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_dump

    SUBROUTINE fpmetbinary_load(filename, cm_index)
        CHARACTER(LEN=*), INTENT(IN) :: filename  ! Full path for file
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        INTEGER millisecs_start, millisecs_stop, count_rate, count_max

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)
        OPEN(IOUNIT_LOAD, file=filename, action='READ', status='OLD', form="unformatted", access="stream")
        CALL fpio(IOUNIT_LOAD, 'LOAD', cm_index)
        CLOSE(IOUNIT_LOAD)
        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
        !PRINT *, 'Load walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_load

    SUBROUTINE fpmetbinary_zero(cm_index)
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 


        ! Zeroes out, in local datastructures, the values dumped/loaded
        ! This was written primarily as a testing mechanism.
        ! The lines here correspond to READ and WRITE in the dump/load routines

        ! Scalar values
        nx=0.0; ny=0.0; nxmin1=0.0; nymin1=0.0; nxfield=0.0
        nuvz=0.0; nwz=0.0; nz=0.0; nmixz=0.0; nlev_ec=0.0
        dx=0.0; dy=0.0; xlon0=0.0; ylat0=0.0; dxconst=0.0; dyconst=0.0

        ! Fixed fields, static in time
        oro=0.0; excessoro=0.0; lsm=0.0; xlanduse=0.0; height=0.0

        ! 3d fields
        uu(:,:,:,cm_index) = 0.0
        vv(:,:,:,cm_index) = 0.0
        uupol(:,:,:,cm_index) = 0.0
        vvpol(:,:,:,cm_index) = 0.0
        ww(:,:,:,cm_index) = 0.0
        tt(:,:,:,cm_index) = 0.0
        qv(:,:,:,cm_index) = 0.0
        pv(:,:,:,cm_index) = 0.0
        rho(:,:,:,cm_index) = 0.0
        drhodz(:,:,:,cm_index) = 0.0
        tth(:,:,:,cm_index) = 0.0
        qvh(:,:,:,cm_index) = 0.0
        pplev(:,:,:,cm_index) = 0.0
        clouds(:,:,:,cm_index) = 0.0
        cloudsh(:,:,cm_index) = 0.0

        ! 2d fields
        ps(:,:,:,cm_index) = 0.0
        sd(:,:,:,cm_index) = 0.0
        msl(:,:,:,cm_index) = 0.0
        tcc(:,:,:,cm_index) = 0.0
        u10(:,:,:,cm_index) = 0.0
        v10(:,:,:,cm_index) = 0.0
        tt2(:,:,:,cm_index) = 0.0
        td2(:,:,:,cm_index) = 0.0
        lsprec(:,:,:,cm_index) = 0.0
        convprec(:,:,:,cm_index) = 0.0
        sshf(:,:,:,cm_index) = 0.0
        ssr(:,:,:,cm_index) = 0.0
        surfstr(:,:,:,cm_index) = 0.0
        ustar(:,:,:,cm_index) = 0.0
        wstar(:,:,:,cm_index) = 0.0
        hmix(:,:,:,cm_index) = 0.0
        tropopause(:,:,:,cm_index) = 0.0
        oli(:,:,:,cm_index) = 0.0
        diffk(:,:,:,cm_index) = 0.0
        vdep(:,:,:,cm_index) = 0.0

        ! 1d fields
        z0(:) = 0.0
        akm(:) = 0.0
        bkm(:) = 0.0
        akz(:) = 0.0
        bkz(:) = 0.0
        aknew(:) = 0.0
        bknew(:) = 0.0

        ! Nested, scalar values (for each nest)
        nxn(:) = 0.0
        nyn(:) = 0.0
        dxn(:) = 0.0
        dyn(:) = 0.0
        xlon0n(:) = 0.0
        ylat0n(:) = 0.0

        ! Nested fields, static in time
        oron=0.0; excessoron=0.0; lsmn=0.0; xlandusen=0.0

        ! 3d nested fields
        uun(:,:,:,cm_index,:) = 0.0
        wwn(:,:,:,cm_index,:) = 0.0
        ttn(:,:,:,cm_index,:) = 0.0
        qvn(:,:,:,cm_index,:) = 0.0
        pvn(:,:,:,cm_index,:) = 0.0
        cloudsn(:,:,:,cm_index,:) = 0.0
        cloudsnh(:,:,cm_index,:) = 0.0
        rhon(:,:,:,cm_index,:) = 0.0
        drhodzn(:,:,:,cm_index,:) = 0.0
        tthn(:,:,:,cm_index,:) = 0.0
        qvhn(:,:,:,cm_index,:) = 0.0

        ! 2d nested fields
        psn(:,:,:,cm_index,:) = 0.0
        sdn(:,:,:,cm_index,:) = 0.0
        msln(:,:,:,cm_index,:) = 0.0
        tccn(:,:,:,cm_index,:) = 0.0
        u10n(:,:,:,cm_index,:) = 0.0
        v10n(:,:,:,cm_index,:) = 0.0
        tt2n(:,:,:,cm_index,:) = 0.0
        td2n(:,:,:,cm_index,:) = 0.0
        lsprecn(:,:,:,cm_index,:) = 0.0
        convprecn(:,:,:,cm_index,:) = 0.0
        sshfn(:,:,:,cm_index,:) = 0.0
        ssrn(:,:,:,cm_index,:) = 0.0
        surfstrn(:,:,:,cm_index,:) = 0.0
        ustarn(:,:,:,cm_index,:) = 0.0
        wstarn(:,:,:,cm_index,:) = 0.0
        hmixn(:,:,:,cm_index,:) = 0.0
        tropopausen(:,:,:,cm_index,:) = 0.0
        olin(:,:,:,cm_index,:) = 0.0
        diffkn(:,:,:,cm_index,:) = 0.0
        vdepn(:,:,:,cm_index,:) = 0.0

        ! Auxiliary variables for nests
        xresoln(:) = 0.0
        yresoln(:) = 0.0
        xln(:) = 0.0
        yln(:) = 0.0
        xrn(:) = 0.0
        yrn(:) = 0.0

        ! Variables for polar stereographic projection
        xglobal=.FALSE.; sglobal=.FALSE.; nglobal=.FALSE.
        switchnorthg=0.0; switchsouthg=0.0
        southpolemap(:) = 0.0
        northpolemap(:) = 0.0

        ! Variables declared in conv_mod (convection)
        pconv(:) = 0.0
        phconv(:) = 0.0
        dpr(:) = 0.0
        pconv_hpa(:) = 0.0
        phconv_hpa(:) = 0.0
        ft(:) = 0.0
        fq(:) = 0.0
        fmass(:,:) = 0.0
        sub(:) = 0.0
        fmassfrac(:,:) = 0.0
        cbaseflux(:,:) = 0.0
        cbasefluxn(:,:,:) = 0.0
        tconv(:) = 0.0
        qconv(:) = 0.0
        qsconv(:) = 0.0
        psconv=0.0; tt2conv=0.0; td2conv=0.0
        nconvlev=0.0; nconvtop=0.0

    END SUBROUTINE fpmetbinary_zero

    SUBROUTINE fpio(iounit, op, cm_index)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: iounit
        CHARACTER(LEN=4), INTENT(IN) :: op        ! Operation - DUMP or LOAD
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        ! These are temporary variables, used in the LOAD option, for 
        ! comparing against the current values in FLEXPART of nxmax, nymax, ...
        INTEGER :: temp_nxmax, temp_nymax, temp_nzmax, &
&                  temp_nuvzmax, temp_nwzmax

        CHARACTER(LEN=12) :: temp_preproc_format_version_str

        CHARACTER(LEN=128) :: errmesg

        if (op == 'DUMP') THEN


            ! Write the preprocessing format version string
            WRITE (iounit) PREPROC_FORMAT_VERSION_STR

            ! Write the compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays
            WRITE (iounit) nxmax, nymax, nzmax, nuvzmax, nwzmax

            ! Scalar values
            WRITE(iounit) nx, ny, nxmin1, nymin1, nxfield
            WRITE(iounit) nuvz, nwz, nz, nmixz, nlev_ec
            WRITE(iounit) dx, dy, xlon0, ylat0, dxconst, dyconst

            ! Fixed fields, static in time
            WRITE(iounit) oro, excessoro, lsm, xlanduse, height

            ! 3d fields
            WRITE(iounit) uu(:,:,:,cm_index)
            WRITE(iounit) vv(:,:,:,cm_index)
            WRITE(iounit) uupol(:,:,:,cm_index)
            WRITE(iounit) vvpol(:,:,:,cm_index)
            WRITE(iounit) ww(:,:,:,cm_index)
            WRITE(iounit) tt(:,:,:,cm_index)
            WRITE(iounit) qv(:,:,:,cm_index)
            WRITE(iounit) pv(:,:,:,cm_index)
            WRITE(iounit) rho(:,:,:,cm_index)
            WRITE(iounit) drhodz(:,:,:,cm_index)
            WRITE(iounit) tth(:,:,:,cm_index)
            WRITE(iounit) qvh(:,:,:,cm_index)
            WRITE(iounit) pplev(:,:,:,cm_index)
            WRITE(iounit) clouds(:,:,:,cm_index)
            WRITE(iounit) cloudsh(:,:,cm_index)

            ! 2d fields
            WRITE(iounit) ps(:,:,:,cm_index)
            WRITE(iounit) sd(:,:,:,cm_index)
            WRITE(iounit) msl(:,:,:,cm_index)
            WRITE(iounit) tcc(:,:,:,cm_index)
            WRITE(iounit) u10(:,:,:,cm_index)
            WRITE(iounit) v10(:,:,:,cm_index)
            WRITE(iounit) tt2(:,:,:,cm_index)
            WRITE(iounit) td2(:,:,:,cm_index)
            WRITE(iounit) lsprec(:,:,:,cm_index)
            WRITE(iounit) convprec(:,:,:,cm_index)
            WRITE(iounit) sshf(:,:,:,cm_index)
            WRITE(iounit) ssr(:,:,:,cm_index)
            WRITE(iounit) surfstr(:,:,:,cm_index)
            WRITE(iounit) ustar(:,:,:,cm_index)
            WRITE(iounit) wstar(:,:,:,cm_index)
            WRITE(iounit) hmix(:,:,:,cm_index)
            WRITE(iounit) tropopause(:,:,:,cm_index)
            WRITE(iounit) oli(:,:,:,cm_index)
            WRITE(iounit) diffk(:,:,:,cm_index)
            WRITE(iounit) vdep(:,:,:,cm_index)

            ! 1d fields
            WRITE(iounit) z0(:)
            WRITE(iounit) akm(:)
            WRITE(iounit) bkm(:)
            WRITE(iounit) akz(:)
            WRITE(iounit) bkz(:)
            WRITE(iounit) aknew(:)
            WRITE(iounit) bknew(:)

            ! Nested, scalar values (for each nest)
            WRITE(iounit) nxn(:)
            WRITE(iounit) nyn(:)
            WRITE(iounit) dxn(:)
            WRITE(iounit) dyn(:)
            WRITE(iounit) xlon0n(:)
            WRITE(iounit) ylat0n(:)

            ! Nested fields, static over time
            WRITE(iounit) oron, excessoron, lsmn, xlandusen 

            ! 3d nested fields
            WRITE(iounit) uun(:,:,:,cm_index,:)
            WRITE(iounit) vvn(:,:,:,cm_index,:)
            WRITE(iounit) wwn(:,:,:,cm_index,:)
            WRITE(iounit) ttn(:,:,:,cm_index,:)
            WRITE(iounit) qvn(:,:,:,cm_index,:)
            WRITE(iounit) pvn(:,:,:,cm_index,:)
            WRITE(iounit) cloudsn(:,:,:,cm_index,:)
            WRITE(iounit) cloudsnh(:,:,cm_index,:)
            WRITE(iounit) rhon(:,:,:,cm_index,:)
            WRITE(iounit) drhodzn(:,:,:,cm_index,:)
            WRITE(iounit) tthn(:,:,:,cm_index,:)
            WRITE(iounit) qvhn(:,:,:,cm_index,:)

            ! 2d nested fields
            WRITE(iounit) psn(:,:,:,cm_index,:)
            WRITE(iounit) sdn(:,:,:,cm_index,:)
            WRITE(iounit) msln(:,:,:,cm_index,:)
            WRITE(iounit) tccn(:,:,:,cm_index,:)
            WRITE(iounit) u10n(:,:,:,cm_index,:)
            WRITE(iounit) v10n(:,:,:,cm_index,:)
            WRITE(iounit) tt2n(:,:,:,cm_index,:)
            WRITE(iounit) td2n(:,:,:,cm_index,:)
            WRITE(iounit) lsprecn(:,:,:,cm_index,:)
            WRITE(iounit) convprecn(:,:,:,cm_index,:)
            WRITE(iounit) sshfn(:,:,:,cm_index,:)
            WRITE(iounit) ssrn(:,:,:,cm_index,:)
            WRITE(iounit) surfstrn(:,:,:,cm_index,:)
            WRITE(iounit) ustarn(:,:,:,cm_index,:)
            WRITE(iounit) wstarn(:,:,:,cm_index,:)
            WRITE(iounit) hmixn(:,:,:,cm_index,:)
            WRITE(iounit) tropopausen(:,:,:,cm_index,:)
            WRITE(iounit) olin(:,:,:,cm_index,:)
            WRITE(iounit) diffkn(:,:,:,cm_index,:)
            WRITE(iounit) vdepn(:,:,:,cm_index,:)

            ! Auxiliary variables for nests
            WRITE(iounit) xresoln(:)
            WRITE(iounit) yresoln(:)
            WRITE(iounit) xln(:)
            WRITE(iounit) yln(:)
            WRITE(iounit) xrn(:)
            WRITE(iounit) yrn(:)

            ! Variables for polar stereographic projection
            WRITE(iounit) xglobal, sglobal, nglobal
            WRITE(iounit) switchnorthg, switchsouthg
            WRITE(iounit) southpolemap(:)
            WRITE(iounit) northpolemap(:)

            ! Variables declared in conv_mod (convection)
            WRITE(iounit) pconv(:)
            WRITE(iounit) phconv(:)
            WRITE(iounit) dpr(:)
            WRITE(iounit) pconv_hpa(:)
            WRITE(iounit) phconv_hpa(:)
            WRITE(iounit) ft(:)
            WRITE(iounit) fq(:)
            WRITE(iounit) fmass(:,:)
            WRITE(iounit) sub(:)
            WRITE(iounit) fmassfrac(:,:)
            WRITE(iounit) cbaseflux(:,:)
            WRITE(iounit) cbasefluxn(:,:,:)
            WRITE(iounit) tconv(:)
            WRITE(iounit) qconv(:)
            WRITE(iounit) qsconv(:)
            WRITE(iounit) psconv, tt2conv, td2conv
            WRITE(iounit) nconvlev, nconvtop

        ELSE IF (op == 'LOAD') THEN 

            ! Read the preprocessed format version string and insure it
            ! matches this version
            READ (iounit) temp_preproc_format_version_str
            PRINT *, 'Reading preprocessed file format version: ', &
&                    temp_preproc_format_version_str

            IF (TRIM(temp_preproc_format_version_str) == &
&                                        TRIM(PREPROC_FORMAT_VERSION_STR)) THEN
                CONTINUE
            ELSE
                ! PRINT *, ''  GK: causes relocation truncated to fit: R_X86_64_32
                PRINT *, 'Inconsistent preprocessing format version'
                PRINT *, 'Expected Version: ', PREPROC_FORMAT_VERSION_STR
                PRINT *, 'Detected Version: ', temp_preproc_format_version_str
                ! PRINT *, ''
                STOP
            END IF

            ! Read the compiled max dimensions that were dumped from par_mod 
            ! when creating the fp file, so that we can compare against
            ! current FLEXPART dimensions - they need to be the same, or else
            ! we abort.
            READ (iounit) temp_nxmax, temp_nymax, temp_nzmax, &
&                         temp_nuvzmax, temp_nwzmax


            IF ( (temp_nxmax == nxmax) .AND. (temp_nymax == nymax) .AND. &
&                   (temp_nzmax == nzmax) .AND. &
&                   (temp_nuvzmax == nuvzmax) .AND. &
&                   (temp_nwzmax == nwzmax) ) THEN
                CONTINUE
            ELSE
                PRINT *, 'Incompatible dimensions between fp file and current FLEXPART!'
                ! PRINT *, ''
                PRINT *, '                  FP file     Compiled FP'
                PRINT *, 'nxmax:     ', temp_nxmax, '    ', nxmax 
                PRINT *, 'nymax:     ', temp_nymax, '    ', nymax 
                PRINT *, 'nzmax:     ', temp_nzmax, '    ', nzmax 
                PRINT *, 'nuvzmax:     ', temp_nuvzmax, '    ', nuvzmax 
                PRINT *, 'nwzmax:     ', temp_nwzmax, '    ', nwzmax 
                ! PRINT *, ''
                STOP
            END IF


            ! Scalar values
            READ(iounit) nx, ny, nxmin1, nymin1, nxfield
            READ(iounit) nuvz, nwz, nz, nmixz, nlev_ec
            READ(iounit) dx, dy, xlon0, ylat0, dxconst, dyconst

            ! Fixed fields, static in time
            READ(iounit) oro, excessoro, lsm, xlanduse, height

            ! 3d fields
            READ(iounit) uu(:,:,:,cm_index)
            READ(iounit) vv(:,:,:,cm_index)
            READ(iounit) uupol(:,:,:,cm_index)
            READ(iounit) vvpol(:,:,:,cm_index)
            READ(iounit) ww(:,:,:,cm_index)
            READ(iounit) tt(:,:,:,cm_index)
            READ(iounit) qv(:,:,:,cm_index)
            READ(iounit) pv(:,:,:,cm_index)
            READ(iounit) rho(:,:,:,cm_index)
            READ(iounit) drhodz(:,:,:,cm_index)
            READ(iounit) tth(:,:,:,cm_index)
            READ(iounit) qvh(:,:,:,cm_index)
            READ(iounit) pplev(:,:,:,cm_index)
            READ(iounit) clouds(:,:,:,cm_index)
            READ(iounit) cloudsh(:,:,cm_index)

            ! 2d fields
            READ(iounit) ps(:,:,:,cm_index)
            READ(iounit) sd(:,:,:,cm_index)
            READ(iounit) msl(:,:,:,cm_index)
            READ(iounit) tcc(:,:,:,cm_index)
            READ(iounit) u10(:,:,:,cm_index)
            READ(iounit) v10(:,:,:,cm_index)
            READ(iounit) tt2(:,:,:,cm_index)
            READ(iounit) td2(:,:,:,cm_index)
            READ(iounit) lsprec(:,:,:,cm_index)
            READ(iounit) convprec(:,:,:,cm_index)
            READ(iounit) sshf(:,:,:,cm_index)
            READ(iounit) ssr(:,:,:,cm_index)
            READ(iounit) surfstr(:,:,:,cm_index)
            READ(iounit) ustar(:,:,:,cm_index)
            READ(iounit) wstar(:,:,:,cm_index)
            READ(iounit) hmix(:,:,:,cm_index)
            READ(iounit) tropopause(:,:,:,cm_index)
            READ(iounit) oli(:,:,:,cm_index)
            READ(iounit) diffk(:,:,:,cm_index)
            READ(iounit) vdep(:,:,:,cm_index)

            ! 1d fields
            READ(iounit) z0(:)
            READ(iounit) akm(:)
            READ(iounit) bkm(:)
            READ(iounit) akz(:)
            READ(iounit) bkz(:)
            READ(iounit) aknew(:)
            READ(iounit) bknew(:)


            ! Nested, scalar values (for each nest)
            READ(iounit) nxn(:)
            READ(iounit) nyn(:)
            READ(iounit) dxn(:)
            READ(iounit) dyn(:)
            READ(iounit) xlon0n(:)
            READ(iounit) ylat0n(:)


            ! Nested fields, static over time
            READ(iounit) oron, excessoron, lsmn, xlandusen 

            ! 3d nested fields
            READ(iounit) uun(:,:,:,cm_index,:)
            READ(iounit) vvn(:,:,:,cm_index,:)
            READ(iounit) wwn(:,:,:,cm_index,:)
            READ(iounit) ttn(:,:,:,cm_index,:)
            READ(iounit) qvn(:,:,:,cm_index,:)
            READ(iounit) pvn(:,:,:,cm_index,:)
            READ(iounit) cloudsn(:,:,:,cm_index,:)
            READ(iounit) cloudsnh(:,:,cm_index,:)
            READ(iounit) rhon(:,:,:,cm_index,:)
            READ(iounit) drhodzn(:,:,:,cm_index,:)
            READ(iounit) tthn(:,:,:,cm_index,:)
            READ(iounit) qvhn(:,:,:,cm_index,:)

            ! 2d nested fields
            READ(iounit) psn(:,:,:,cm_index,:)
            READ(iounit) sdn(:,:,:,cm_index,:)
            READ(iounit) msln(:,:,:,cm_index,:)
            READ(iounit) tccn(:,:,:,cm_index,:)
            READ(iounit) u10n(:,:,:,cm_index,:)
            READ(iounit) v10n(:,:,:,cm_index,:)
            READ(iounit) tt2n(:,:,:,cm_index,:)
            READ(iounit) td2n(:,:,:,cm_index,:)
            READ(iounit) lsprecn(:,:,:,cm_index,:)
            READ(iounit) convprecn(:,:,:,cm_index,:)
            READ(iounit) sshfn(:,:,:,cm_index,:)
            READ(iounit) ssrn(:,:,:,cm_index,:)
            READ(iounit) surfstrn(:,:,:,cm_index,:)
            READ(iounit) ustarn(:,:,:,cm_index,:)
            READ(iounit) wstarn(:,:,:,cm_index,:)
            READ(iounit) hmixn(:,:,:,cm_index,:)
            READ(iounit) tropopausen(:,:,:,cm_index,:)
            READ(iounit) olin(:,:,:,cm_index,:)
            READ(iounit) diffkn(:,:,:,cm_index,:)
            READ(iounit) vdepn(:,:,:,cm_index,:)

            ! Auxiliary variables for nests
            READ(iounit) xresoln(:)
            READ(iounit) yresoln(:)
            READ(iounit) xln(:)
            READ(iounit) yln(:)
            READ(iounit) xrn(:)
            READ(iounit) yrn(:)

            ! Variables for polar stereographic projection
            READ(iounit) xglobal, sglobal, nglobal
            READ(iounit) switchnorthg, switchsouthg
            READ(iounit) southpolemap(:)
            READ(iounit) northpolemap(:)

            ! Variables declared in conv_mod (convection)
            READ(iounit) pconv(:)
            READ(iounit) phconv(:)
            READ(iounit) dpr(:)
            READ(iounit) pconv_hpa(:)
            READ(iounit) phconv_hpa(:)
            READ(iounit) ft(:)
            READ(iounit) fq(:)
            READ(iounit) fmass(:,:)
            READ(iounit) sub(:)
            READ(iounit) fmassfrac(:,:)
            READ(iounit) cbaseflux(:,:)
            READ(iounit) cbasefluxn(:,:,:)
            READ(iounit) tconv(:)
            READ(iounit) qconv(:)
            READ(iounit) qsconv(:)
            READ(iounit) psconv, tt2conv, td2conv
            READ(iounit) nconvlev, nconvtop

        ELSE
            STOP 'fpio(): Illegal operation' 
            
        ENDIF
    END SUBROUTINE fpio

    SUBROUTINE fpmetbinary_filetext(filename, cm_index)

        ! This is a utility subroutine meant to be used for testing purposes.
        ! It facilitates the text output of variables read in from the 
        ! specified .fp file.  This routine will easily cause the program
        ! to crash due memory allocation issues, particularly when you are
        ! trying to text print 3d arrays in a single formatted statetment.
        
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        !OPEN(IOUNIT_TEXTOUT, file=filename, action='WRITE', status='REPLACE', &
        !    form="formatted", access="stream")
        OPEN(IOUNIT_TEXTOUT, file=filename, action='WRITE', &
             form="formatted", access="APPEND")

        WRITE(IOUNIT_TEXTOUT, *) 'oro: ', oro
        WRITE(IOUNIT_TEXTOUT, *) 'excessoro: ', excessoro
        WRITE(IOUNIT_TEXTOUT, *) 'lsm: ', lsm
        WRITE(IOUNIT_TEXTOUT, *) 'xlanduse: ', xlanduse
        WRITE(IOUNIT_TEXTOUT, *) 'height: ', height

        WRITE(IOUNIT_TEXTOUT, *) 'uu: ', uu(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'vv: ', vv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'uupol: ', uupol(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'vvpol: ', vvpol(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'ww: ', ww(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'tt: ', tt(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'qv: ', qv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'pv: ', pv(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'rho: ', rho(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'drhodz: ', drhodz(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'tth: ', tth(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'qvh: ', qvh(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'pplev: ', pplev(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'clouds: ', clouds(:,:,:,cm_index)
        WRITE(IOUNIT_TEXTOUT, *) 'cloudsh: ', cloudsh(:,:,cm_index)




        CLOSE(IOUNIT_TEXTOUT)
    END SUBROUTINE fpmetbinary_filetext


END MODULE fpmetbinary_mod
