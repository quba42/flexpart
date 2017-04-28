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
    USE par_mod, ONLY : nxmax, nymax, nzmax, nuvzmax, nwzmax, numclass, maxspec, &
&                       maxnests, nxmaxn, nymaxn

    USE netcdf

    IMPLICIT NONE

    ! Users may want to change these IO Unit values if they conflict with other parts
    ! of code
    ! April 2017 (DJM) - These are only needed if you use the fpio_rawbin
    ! routines for raw binary output.  The default now is NC4, but I've kept
    ! the old code for the time being. 
    INTEGER, PARAMETER :: IOUNIT_DUMP = 33, IOUNIT_LOAD = 34, &
                          IOUNIT_TEXTOUT = 35
                          
                   
    INTEGER, PARAMETER :: PREPROC_FMT_STR_DIM = 11

    ! When a change is made to the format of the preprocessed file, such that
    ! this routine will not be able to read a previous version, this version
    ! string should be modified


    ! April 2017 (DJM)        
    ! WARNING - for now, for NC4 compatability, make sure that the 
    ! PREPROC_FMT_STR_DIM 
    ! defined above is exactly the length of the string PLUS the null 
    ! character added
    ! I've had a hell of a time making it all compatible with NC4 (DJM)
    CHARACTER(LEN=PREPROC_FMT_STR_DIM), PARAMETER :: &
&                         PREPROC_FORMAT_VERSION_STR = 'FP_p-9.3.2'//char(0)

    PRIVATE IOUNIT_DUMP, IOUNIT_LOAD, IOUNIT_TEXTOUT, fpio,    &
&           PREPROC_FORMAT_VERSION_STR


CONTAINS

  !*****************************************************************************
  !                                                                            *
  !    April 2017 (DJM) - the comment below suggesting that variables need     *
  !         to be read in exactly the same order that they are written applies *
  !         only to raw binary format, not NC4.                                *
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

        INTEGER :: ncretval, ncid          ! NetCDF func return value, file id

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)

        ! Create and open NC4 file for writing
        PRINT *, 'Opening NC4 file...'
        ncretval = nf90_create(filename, &
&                              OR(NF90_CLOBBER, NF90_HDF5), &
&                              ncid)



        CALL fpio(ncid, 'DUMP', cm_index)

        PRINT *, 'Closing NC4 file...'
        ncretval = nf90_close(ncid)

        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)

        !PRINT *, 'Dump walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_dump

    SUBROUTINE fpmetbinary_load(filename, cm_index)
        CHARACTER(LEN=*), INTENT(IN) :: filename  ! Full path for file
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 

        INTEGER :: ncretval, ncid          ! NetCDF func return value, file id

        INTEGER millisecs_start, millisecs_stop, count_rate, count_max

        CALL SYSTEM_CLOCK(millisecs_start, count_rate, count_max)

PRINT *, "filename: ", filename
PRINT *, "Opening nc file for reading: ", filename
        ncretval = nf90_open(filename, NF90_NOWRITE, ncid)
        call handle_nf90_err(ncretval)
PRINT *, 'OPENED NC4 FILE FOR READING...'


        CALL fpio(ncid, 'LOAD', cm_index)

        CALL SYSTEM_CLOCK(millisecs_stop, count_rate, count_max)
        !PRINT *, 'Load walltime secs: ', (millisecs_stop-millisecs_start)/1000.0
    END SUBROUTINE fpmetbinary_load




    SUBROUTINE fpio(ncid, op, cm_index)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: ncid               ! NetCDF file id
        CHARACTER(LEN=4), INTENT(IN) :: op        ! Operation - DUMP or LOAD
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 


        ! Helper functions (in this module)
        !INTEGER :: logical2integer
        !LOGICAL :: integer2logical

        INTEGER :: temp_integer   ! temporary value
        INTEGER :: ncret          ! Return value from NetCDF calls
        INTEGER :: ncvarid          ! NetCDF variable ID

        INTEGER :: nxmax_dimid, nymax_dimid, nzmax_dimid, nuvzmax_dimid, nwzmax_dimid, &
&                  maxspec_dimid, numclass_dimid, maxnests_dimid, nxmaxn_dimid, nymaxn_dimid, &
&                  zero_to_nzmax_dimid, zero_to_maxnests_dimid, polemap_dimid, &
&                  nconvlevmax_dimid, na_dimid, preproc_fmt_str_dimid


        INTEGER, DIMENSION(1) :: dim1dids    ! Dimension IDs for 1D arrays
        INTEGER, DIMENSION(2) :: dim2dids    ! Dimension IDs for 2D arrays
        INTEGER, DIMENSION(3) :: dim3dids    ! Dimension IDs for 3D arrays
        INTEGER, DIMENSION(4) :: dim4dids    ! Dimension IDs for 4D arrays
        INTEGER, DIMENSION(5) :: dim5dids    ! Dimension IDs for 5D arrays




        ! These are used when loading in dimensions from NC file
        CHARACTER(LEN=NF90_MAX_NAME) :: nxmax_dimname, nymax_dimname, nzmax_dimname, &
&                                       nuvzmax_dimname, nwzmax_dimname,&
&                                       maxspec_dimname, numclass_dimname,&
&                                       maxnests_dimname, nxmaxn_dimname, nymaxn_dimname, &
&                                       zero_to_nzmax_dimname, zero_to_maxnests_dimname, &
&                                       polemap_dimname, nconvlevmax_dimname, na_dimname, &
&                                       preproc_fmt_str_dimname

        ! These are temporary variables, used in the LOAD option, for 
        ! comparing against the current values in FLEXPART of nxmax, nymax, ...
        INTEGER :: temp_nxmax, temp_nymax, temp_nzmax, &
&                  temp_nuvzmax, temp_nwzmax, &
&                  temp_maxspec, temp_numclass,&
&                  temp_maxnests, temp_nxmaxn, temp_nymaxn, temp_preproc_fmt_str_dim

        CHARACTER(LEN=PREPROC_FMT_STR_DIM) :: temp_preproc_format_version_str

        CHARACTER(LEN=128) :: errmesg

        INTEGER, PARAMETER :: DEF_LEVEL = 3

        if (op == 'DUMP') THEN


            ! Write the preprocessing format version string
            !  NEED TO FILL THIS IN FOR NC4




            ! Write the compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays



            ! Dimension for the preprocessing format string
            ncret = nf90_def_dim(ncid, 'preproc_fmt_str_dim', PREPROC_FMT_STR_DIM, &
&                                preproc_fmt_str_dimid)
            call handle_nf90_err(ncret)

            ncret = nf90_def_dim(ncid, 'nxmax', nxmax, nxmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nymax', nymax, nymax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nzmax', nzmax, nzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nuvzmax', nuvzmax, nuvzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nwzmax', nwzmax, nwzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'maxspec', maxspec, maxspec_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'numclass', numclass, numclass_dimid)
            call handle_nf90_err(ncret)

            ! There are a handful of variables indexed from 0 to n, rather than 0 to n-1,
            ! so these dimensions handle that.  What a pain.
            ncret = nf90_def_dim(ncid, 'zero_to_nzmax', nzmax+1, zero_to_nzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'zero_to_maxnests', maxnests+1, zero_to_maxnests_dimid)
            call handle_nf90_err(ncret)

            ! This is for a couple of small arrays that store polar stereographic stuff
            ncret = nf90_def_dim(ncid, 'polemap_dim', 9, polemap_dimid)
            call handle_nf90_err(ncret)

            ! These two values come from conv_mod
            ncret = nf90_def_dim(ncid, 'nconvlevmax_dim', nconvlevmax, nconvlevmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'na_dim', na, na_dimid)
            call handle_nf90_err(ncret)

            ! Scalar values


            dim1dids = (/preproc_fmt_str_dimid/)
            ncret = nf90_def_var(ncid, 'preproc_fmt_str', NF90_CHAR, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, PREPROC_FORMAT_VERSION_STR(1:preproc_fmt_str_dim)) 
            call handle_nf90_err(ncret)


            ncret = nf90_def_var(ncid, 'nx', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nx)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ny', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, ny)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nxmin1', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nxmin1)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nymin1', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nymin1)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nxfield', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nxfield)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nuvz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nuvz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nwz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nwz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nmixz', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nmixz)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nlev_ec', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nlev_ec)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dx', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dx)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dy', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dy)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'xlon0', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, xlon0)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ylat0', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, ylat0)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dxconst', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dxconst)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dyconst', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, dyconst)
            call handle_nf90_err(ncret)



            ! Fixed fields, static in time
            dim2dids = (/nxmax_dimid, nymax_dimid/)

            ncret = nf90_def_var(ncid, 'oro', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oro(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'excessoro', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                excessoro(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsm', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsm(0:nxmax-1, 0:nymax-1))
            call handle_nf90_err(ncret)

            dim3dids = (/nxmax_dimid, nymax_dimid, numclass_dimid/)
            ! numclass comes from par_mod - number of land use classes
            ncret = nf90_def_var(ncid, 'xlanduse', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlanduse(0:nxmax-1, 0:nymax-1, 1:numclass))
            call handle_nf90_err(ncret)

            dim1dids = (/nzmax_dimid/)
            ncret = nf90_def_var(ncid, 'height', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                height(1:nzmax))
            call handle_nf90_err(ncret)




            ! 3d fields
            dim3dids = (/nxmax_dimid, nymax_dimid, nzmax_dimid/)

            ncret = nf90_def_var(ncid, 'uu', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uu(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'uupol', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uupol(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vvpol', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vvpol(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ww', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ww(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pv', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pv(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'rho', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                rho(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'drhodz', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                drhodz(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'clouds', NF90_BYTE, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index))
            call handle_nf90_err(ncret)



            ! Note the change in z dimension for the following
            dim3dids = (/nxmax_dimid, nymax_dimid, nuvzmax_dimid/)

            ncret = nf90_def_var(ncid, 'tth', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tth(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvh', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvh(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pplev', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pplev(0:nxmax-1, 0:nymax-1, 1:nuvzmax, cm_index))
            call handle_nf90_err(ncret)


            dim2dids = (/nxmax_dimid, nymax_dimid/)
            ncret = nf90_def_var(ncid, 'cloudsh', NF90_INT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsh(0:nxmax-1, 0:nymax-1, cm_index))
            call handle_nf90_err(ncret)



!            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))

!            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



            ! 2d fields
            dim2dids = (/nxmax_dimid, nymax_dimid/)

            ncret = nf90_def_var(ncid, 'ps', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ps(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sd', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sd(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'msl', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                msl(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tcc', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tcc(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'u10', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                u10(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'v10', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                v10(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt2', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tt2(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'td2', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                td2(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsprec', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsprec(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'convprec', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                convprec(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sshf', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sshf(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ssr', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ssr(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'surfstr', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                surfstr(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ustar', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ustar(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'wstar', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                wstar(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'hmix', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                hmix(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tropopause', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tropopause(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'oli', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oli(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'diffk', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                diffk(0:nxmax-1, 0:nymax-1, 1, cm_index))
            call handle_nf90_err(ncret)



!           !PRINT *, 'SUM(ps(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(ps(0:nxmax-1,0:nymax-1,1, cm_index))

!            PRINT *, 'SUM(wstar(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(wstar(0:nxmax-1,0:nymax-1,1, cm_index))


            dim3dids = (/nxmax_dimid, nymax_dimid, maxspec_dimid/)

            ncret = nf90_def_var(ncid, 'vdep', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vdep(0:nxmax-1, 0:nymax-1, 1:maxspec, cm_index))
            call handle_nf90_err(ncret)



            ! 1d fields
            dim1dids = (/numclass_dimid/)

            ncret = nf90_def_var(ncid, 'z0', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                z0(1:numclass))


            dim1dids = (/nwzmax_dimid/)

            ncret = nf90_def_var(ncid, 'akm', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                akm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bkm', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bkm(1:nwzmax))
            call handle_nf90_err(ncret)


            dim1dids = (/nuvzmax_dimid/)

            ncret = nf90_def_var(ncid, 'akz', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                akz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bkz', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bkz(1:nuvzmax))
            call handle_nf90_err(ncret)


            dim1dids = (/nzmax_dimid/)

            ncret = nf90_def_var(ncid, 'aknew', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                aknew(1:nzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'bknew', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                bknew(1:nzmax))
            call handle_nf90_err(ncret)


!            PRINT *, 'SUM(bknew(1:nzmax)): ', &
!&                                        SUM(bknew(1:nzmax))



            ! Getting ready to add in nested code

            ! These are compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays
            ncret = nf90_def_dim(ncid, 'maxnests', maxnests, maxnests_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nxmaxn', nxmaxn, nxmaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_dim(ncid, 'nymaxn', nymaxn, nymaxn_dimid)
            call handle_nf90_err(ncret)


            ! Nested, scalar values (for each nest)
            dim1dids = (/maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'nxn', NF90_INT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                nxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nyn', NF90_INT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                nyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dxn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                dxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'dyn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                dyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'xlon0n', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlon0n(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ylat0n', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ylat0n(1:maxnests))
            call handle_nf90_err(ncret)




            ! Nested fields, static over time
            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'oron', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                oron(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'excessoron', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                excessoron(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsmn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsmn(0:nxmaxn-1, 0:nymaxn-1, 1:maxnests))
            call handle_nf90_err(ncret)

            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, numclass_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'xlandusen', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xlandusen(0:nxmaxn-1, 0:nymaxn-1, 1:numclass, 1:maxnests))
            call handle_nf90_err(ncret)

!            PRINT *, 'SUM(oron): ', SUM(oron)



            ! 3d nested fields
            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, nzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'uun', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                uun(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'vvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'wwn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                wwn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ttn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ttn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'pvn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pvn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'rhon', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                rhon(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'drhodzn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                drhodzn(0:nxmaxn-1, 0:nymaxn-1, 1:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)


            ! Note the new dimensions
            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, nuvzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'tthn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tthn(0:nxmaxn-1, 0:nymaxn-1, 1:nuvzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'qvhn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qvhn(0:nxmaxn-1, 0:nymaxn-1, 1:nuvzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ! Note the new dimensions
            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, zero_to_nzmax_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'cloudsn', NF90_INT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsn(0:nxmaxn-1, 0:nymaxn-1, 0:nzmax, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ! Note the new dimensions
            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'cloudsnh', NF90_INT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cloudsnh(0:nxmaxn-1, 0:nymaxn-1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)


!            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
!            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))



            ! 2d nested fields
            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'psn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                psn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sdn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sdn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'msln', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                msln(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tccn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tccn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'u10n', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                u10n(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'v10n', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                v10n(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt2n', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tt2n(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'td2n', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                td2n(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'lsprecn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                lsprecn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'convprecn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                convprecn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sshfn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sshfn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ssrn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ssrn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'surfstrn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                surfstrn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'ustarn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ustarn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'wstarn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                wstarn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'hmixn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                hmixn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tropopausen', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tropopausen(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'olin', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                olin(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'diffkn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                diffkn(0:nxmaxn-1, 0:nymaxn-1, 1, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)

            dim4dids = (/nxmaxn_dimid, nymaxn_dimid, maxspec_dimid, maxnests_dimid/)



            ncret = nf90_def_var(ncid, 'vdepn', NF90_FLOAT, &
&                                       dim4dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                vdepn(0:nxmaxn-1, 0:nymaxn-1, 1:maxspec, cm_index, 1:maxnests))
            call handle_nf90_err(ncret)


!            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(surfstrn): ', SUM(surfstrn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(vdepn): ', SUM(vdepn(:,:,:,cm_index,:))



            ! Auxiliary variables for nests
            dim1dids = (/zero_to_maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'xresoln', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xresoln(0:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'yresoln', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                yresoln(0:maxnests))
            call handle_nf90_err(ncret)

            dim1dids = (/maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'xln', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xln(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'yln', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                yln(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'xrn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                xrn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'yrn', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                yrn(1:maxnests))
            call handle_nf90_err(ncret)

!            PRINT *, 'SUM(yresoln): ', SUM(yresoln)
!            PRINT *, 'SUM(xrn): ', SUM(xrn)



            ! Variables for polar stereographic projection
            dim1dids = (/polemap_dimid/)

            ncret = nf90_def_var(ncid, 'southpolemap', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                southpolemap(:))

            ncret = nf90_def_var(ncid, 'northpolemap', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                northpolemap(:))



            ! xglobal, sglobal, nglobal are LOGICAL vars, and need to be converted
            ! to INTEGER for NetCDF storage
            ncret = nf90_def_var(ncid, 'xglobal', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, logical2integer(xglobal))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'sglobal', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, logical2integer(sglobal))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nglobal', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, logical2integer(nglobal))
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'switchnorthg', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, switchnorthg)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'switchsouthg', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, switchsouthg)
            call handle_nf90_err(ncret)



!            PRINT *, 'SUM(northpolemap): ', SUM(northpolemap)
!            PRINT *, 'xglobal: ', xglobal
!            PRINT *, 'sglobal: ', sglobal
!            PRINT *, 'nglobal: ', nglobal
!            PRINT *, 'switchsouthg: ', switchsouthg

            ! Variables declared in conv_mod (convection)
            dim1dids = (/nconvlevmax_dimid/)

            ncret = nf90_def_var(ncid, 'pconv', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pconv(:))

            ncret = nf90_def_var(ncid, 'dpr', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                dpr(:))

            ncret = nf90_def_var(ncid, 'pconv_hpa', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                pconv_hpa(:))

            ncret = nf90_def_var(ncid, 'ft', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                ft(:))

            ncret = nf90_def_var(ncid, 'fq', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                fq(:))

            ncret = nf90_def_var(ncid, 'sub', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                sub(:))

            dim1dids = (/na_dimid/)

            ncret = nf90_def_var(ncid, 'phconv', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                phconv(:))

            ncret = nf90_def_var(ncid, 'phconv_hpa', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                phconv_hpa(:))

            ncret = nf90_def_var(ncid, 'tconv', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                tconv(:))

            ncret = nf90_def_var(ncid, 'qconv', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qconv(:))

            ncret = nf90_def_var(ncid, 'qsconv', NF90_FLOAT, &
&                                       dim1dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                qsconv(:))

            ! New dimensions
            dim2dids = (/nconvlevmax_dimid, nconvlevmax_dimid/)

            ncret = nf90_def_var(ncid, 'fmass', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                fmass(:,:))

            ncret = nf90_def_var(ncid, 'fmassfrac', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                fmassfrac(:,:))


            ! New dimensions
            dim2dids = (/nxmax_dimid, nymax_dimid/)

            ncret = nf90_def_var(ncid, 'cbaseflux', NF90_FLOAT, &
&                                       dim2dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cbaseflux(0:nxmax-1,0:nymax-1))

            ! New dimensions
            dim3dids = (/nxmaxn_dimid, nymaxn_dimid, maxnests_dimid/)

            ncret = nf90_def_var(ncid, 'cbasefluxn', NF90_FLOAT, &
&                                       dim3dids, ncvarid)
            ncret = nf90_def_var_deflate(ncid, ncvarid,   &
&                                        shuffle=0,     &
&                                        deflate=1,     &
&                                        deflate_level=DEF_LEVEL)
            ncret = nf90_put_var(ncid, ncvarid, &
&                                cbasefluxn(0:nxmaxn-1,0:nymaxn-1,1:maxnests))


            ! Scalars
            ncret = nf90_def_var(ncid, 'psconv', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, psconv)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'tt2conv', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, tt2conv)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'td2conv', NF90_FLOAT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, td2conv)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nconvlev', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nconvlev)
            call handle_nf90_err(ncret)

            ncret = nf90_def_var(ncid, 'nconvtop', NF90_INT, ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_put_var(ncid, ncvarid, nconvtop)
            call handle_nf90_err(ncret)

!            PRINT *, 'SUM(pconv): ', SUM(pconv)
!            PRINT *, 'SUM(qconv): ', SUM(qconv)
!            PRINT *, 'SUM(fmassfrac): ', SUM(fmassfrac)
!            PRINT *, 'SUM(cbasefluxn): ', SUM(cbasefluxn)
!            PRINT *, 'tt2conv: ', tt2conv
!            PRINT *, 'nconvlev: ', nconvlev



        ELSE IF (op == 'LOAD') THEN 

            ! Read the preprocessed format version string and insure it
            ! matches this version
            ncret = nf90_inq_dimid(ncid, 'preproc_fmt_str_dim', preproc_fmt_str_dimid)
            call handle_nf90_err(ncret)            
            ncret = nf90_inquire_dimension(ncid, preproc_fmt_str_dimid, preproc_fmt_str_dimname, &
&                                                temp_preproc_fmt_str_dim)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_preproc_fmt_str_dim: ', temp_preproc_fmt_str_dim

            ncret = nf90_inq_varid(ncid, 'preproc_fmt_str', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, temp_preproc_format_version_str(1:temp_preproc_fmt_str_dim))
            call handle_nf90_err(ncret)


!            PRINT *, 'Reading preprocessed file format version: ', &
!&                    temp_preproc_format_version_str
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

            ! Get dimensions
            ncret = nf90_inq_dimid(ncid, 'nxmax', nxmax_dimid)
            call handle_nf90_err(ncret)            
            ncret = nf90_inquire_dimension(ncid, nxmax_dimid, nxmax_dimname, &
&                                                temp_nxmax)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nxmax: ', temp_nxmax


            ncret = nf90_inq_dimid(ncid, 'nymax', nymax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nymax_dimid, nymax_dimname, &
&                                                temp_nymax)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nymax: ', temp_nymax

            ncret = nf90_inq_dimid(ncid, 'nzmax', nzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nzmax_dimid, nzmax_dimname, &
&                                                temp_nzmax)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nzmax: ', temp_nzmax

            ncret = nf90_inq_dimid(ncid, 'nuvzmax', nuvzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nuvzmax_dimid, nuvzmax_dimname, &
&                                                temp_nuvzmax)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nuvzmax: ', temp_nuvzmax

            ncret = nf90_inq_dimid(ncid, 'nwzmax', nwzmax_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nwzmax_dimid, nwzmax_dimname, &
&                                                temp_nwzmax)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nwzmax: ', temp_nwzmax

            ncret = nf90_inq_dimid(ncid, 'numclass', numclass_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, numclass_dimid, numclass_dimname, &
&                                                temp_numclass)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_numclass: ', temp_numclass

            ncret = nf90_inq_dimid(ncid, 'maxspec', maxspec_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, maxspec_dimid, maxspec_dimname, &
&                                                temp_maxspec)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_maxspec: ', temp_maxspec



            IF ( (temp_nxmax == nxmax) .AND. (temp_nymax == nymax) .AND. &
&                   (temp_nzmax == nzmax) .AND. &
&                   (temp_nuvzmax == nuvzmax) .AND. &
&                   (temp_nwzmax == nwzmax) .AND. &
&                   (temp_numclass == numclass) .AND. &
&                   (temp_maxspec == maxspec) ) THEN
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
                PRINT *, 'numclass:     ', temp_numclass, '    ', numclass
                PRINT *, 'maxspec:     ', temp_maxspec, '    ', maxspec
                ! PRINT *, ''
                STOP
            END IF




            ! Scalar values


            ! Get the varid , then read into scalar variable
            ncret = nf90_inq_varid(ncid, 'nx', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nx)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ny', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ny)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nxmin1', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxmin1)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nymin1', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nymin1)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nxfield', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxfield)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nuvz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nuvz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nwz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nwz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nmixz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nmixz)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nlev_ec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nlev_ec)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dx', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dx)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dy', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dy)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlon0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlon0)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ylat0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ylat0)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dxconst', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dxconst)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dyconst', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dyconst)
            call handle_nf90_err(ncret)






            ! Fixed fields, static in time
            ncret = nf90_inq_varid(ncid, 'oro', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oro(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'excessoro', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, excessoro(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsm(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlanduse', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlanduse(0:nxmax-1,0:nymax-1, 1:numclass))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'height', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, height(1:nzmax))
            call handle_nf90_err(ncret)




            ! 3d fields

            ! Get the varid and read the variable into the array
            ncret = nf90_inq_varid(ncid, 'uu', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uu(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'uupol', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uupol(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vvpol', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vvpol(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ww', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ww(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pv(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'rho', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, rho(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'drhodz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, drhodz(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'clouds', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, clouds(0:nxmax-1,0:nymax-1,1:nzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tth', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tth(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvh(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pplev', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pplev(0:nxmax-1,0:nymax-1,1:nuvzmax,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsh(0:nxmax-1,0:nymax-1,cm_index))
            call handle_nf90_err(ncret)




!            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))


!            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



            ! 2d fields

            ! Get the varid and read the variable into the array
            ncret = nf90_inq_varid(ncid, 'ps', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ps(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sd', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sd(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'msl', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, msl(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tcc', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tcc(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'u10', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, u10(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'v10', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, v10(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt2', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt2(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'td2', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, td2(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsprec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsprec(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'convprec', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, convprec(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sshf', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sshf(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ssr', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ssr(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'surfstr', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, surfstr(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ustar', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ustar(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'wstar', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, wstar(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'hmix', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, hmix(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tropopause', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tropopause(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'oli', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oli(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'diffk', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, diffk(0:nxmax-1,0:nymax-1,1,cm_index))
            call handle_nf90_err(ncret)




!            PRINT *, 'SUM(ps(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(ps(0:nxmax-1,0:nymax-1,1, cm_index))

!            PRINT *, 'SUM(wstar(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(wstar(0:nxmax-1,0:nymax-1,1, cm_index))


            ncret = nf90_inq_varid(ncid, 'vdep', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vdep(0:nxmax-1,0:nymax-1,1:maxspec, cm_index))
            call handle_nf90_err(ncret)





            ! 1d fields

            ncret = nf90_inq_varid(ncid, 'z0', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, z0(1:numclass))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'akm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, akm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bkm', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bkm(1:nwzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'akz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, akz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bkz', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bkz(1:nuvzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'aknew', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, aknew(1:nzmax))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'bknew', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, bknew(1:nzmax))
            call handle_nf90_err(ncret)



!            PRINT *, 'SUM(bknew(1:nzmax)): ', &
!&                                        SUM(bknew(1:nzmax))




            ! Now the nested input grid variables
            ! Get the compiled values that were written into the FP file, and
            ! make sure they are equal to the current compiled values, to make
            ! sure we are working with consistent arrays
            ncret = nf90_inq_dimid(ncid, 'maxnests', maxnests_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, maxnests_dimid, maxnests_dimname, &
&                                                temp_maxnests)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_maxnests: ', temp_maxnests

            ncret = nf90_inq_dimid(ncid, 'nxmaxn', nxmaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nxmaxn_dimid, nxmaxn_dimname, &
&                                                temp_nxmaxn)
            call handle_nf90_err(ncret)
!            PRINT *, 'temp_nxmaxn: ', temp_nxmaxn

            ncret = nf90_inq_dimid(ncid, 'nymaxn', nymaxn_dimid)
            call handle_nf90_err(ncret)
            ncret = nf90_inquire_dimension(ncid, nymaxn_dimid, nymaxn_dimname, &
&                                                temp_nymaxn)
            call handle_nf90_err(ncret)
            ! PRINT *, 'temp_nymaxn: ', temp_nymaxn

            ! Note that maxspec_dimid and numclass_dimid were checked above




            IF ( (temp_nxmaxn == nxmaxn) .AND. (temp_nymaxn == nymaxn) .AND. &
&                   (temp_maxnests == maxnests) ) THEN
                CONTINUE
            ELSE
                PRINT *, 'Incompatible dimensions between fp file and current FLEXPART!'
                ! PRINT *, ''
                PRINT *, '                  FP file     Compiled FP'
                PRINT *, 'nxmaxn:     ', temp_nxmaxn, '    ', nxmaxn
                PRINT *, 'nymaxn:     ', temp_nymaxn, '    ', nymaxn
                PRINT *, 'maxnests:     ', temp_maxnests, '    ', maxnests
                STOP
            END IF



            ! Nested, scalar values (for each nest)
            ncret = nf90_inq_varid(ncid, 'nxn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nyn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dxn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dxn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dyn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dyn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlon0n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlon0n(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ylat0n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ylat0n(1:maxnests))
            call handle_nf90_err(ncret)



            ! Nested fields, static over time
            ncret = nf90_inq_varid(ncid, 'oron', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, oron(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'excessoron', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, excessoron(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsmn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsmn(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xlandusen', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xlandusen(0:nxmaxn-1,0:nymaxn-1,1:numclass,1:maxnests))
            call handle_nf90_err(ncret)


!            PRINT *, 'SUM(oron): ', SUM(oron)




            ! 3d nested fields

            ncret = nf90_inq_varid(ncid, 'uun', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, uun(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'wwn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, wwn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ttn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ttn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pvn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pvn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'rhon', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, rhon(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'drhodzn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, drhodzn(0:nxmaxn-1,0:nymaxn-1,1:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tthn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tthn(0:nxmaxn-1,0:nymaxn-1,1:nuvzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qvhn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qvhn(0:nxmaxn-1,0:nymaxn-1,1:nuvzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsn(0:nxmaxn-1,0:nymaxn-1,0:nzmax,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cloudsnh', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cloudsnh(0:nxmaxn-1,0:nymaxn-1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)




!            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
!            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))




            ! 2d nested fields
            ncret = nf90_inq_varid(ncid, 'psn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, psn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sdn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sdn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'msln', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, msln(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tccn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tccn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'u10n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, u10n(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'v10n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, v10n(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt2n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt2n(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'td2n', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, td2n(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'lsprecn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, lsprecn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'convprecn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, convprecn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sshfn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sshfn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ssrn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ssrn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'surfstrn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, surfstrn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ustarn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ustarn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'wstarn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, wstarn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'hmixn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, hmixn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tropopausen', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tropopausen(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'olin', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, olin(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'diffkn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, diffkn(0:nxmaxn-1,0:nymaxn-1,1,cm_index,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'vdepn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, vdepn(0:nxmaxn-1,0:nymaxn-1,1:maxspec,cm_index,1:maxnests))
            call handle_nf90_err(ncret)




!            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(surfstrn): ', SUM(surfstrn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(vdepn): ', SUM(vdepn(:,:,:,cm_index,:))



            ! Auxiliary variables for nests
            ncret = nf90_inq_varid(ncid, 'xresoln', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xresoln(0:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'yresoln', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, yresoln(0:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xln', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xln(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'yln', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, yln(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'xrn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, xrn(1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'yrn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, yrn(1:maxnests))
            call handle_nf90_err(ncret)



!            PRINT *, 'SUM(yresoln): ', SUM(yresoln)
!            PRINT *, 'SUM(xrn): ', SUM(xrn)



            ! Variables for polar stereographic projection
            ncret = nf90_inq_varid(ncid, 'southpolemap', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, southpolemap(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'northpolemap', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, northpolemap(:))
            call handle_nf90_err(ncret)

            ! xglobal, sglobal, nglobal are LOGICAL vars, and need to be converted
            ! to INTEGER for NetCDF storage
            ncret = nf90_inq_varid(ncid, 'xglobal', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, temp_integer)
            call handle_nf90_err(ncret)
            xglobal = integer2logical(temp_integer)

            ncret = nf90_inq_varid(ncid, 'sglobal', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, temp_integer)
            call handle_nf90_err(ncret)
            sglobal = integer2logical(temp_integer)

            ncret = nf90_inq_varid(ncid, 'nglobal', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, temp_integer)
            call handle_nf90_err(ncret)
            nglobal = integer2logical(temp_integer)

            ncret = nf90_inq_varid(ncid, 'switchnorthg', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, switchnorthg)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'switchsouthg', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, switchsouthg)
            call handle_nf90_err(ncret)


!            PRINT *, 'SUM(northpolemap): ', SUM(northpolemap)
!            PRINT *, 'xglobal: ', xglobal
!            PRINT *, 'sglobal: ', sglobal
!            PRINT *, 'nglobal: ', nglobal
!            PRINT *, 'switchsouthg: ', switchsouthg




            ! Variables declared in conv_mod (convection)
            ncret = nf90_inq_varid(ncid, 'pconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pconv(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'dpr', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, dpr(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'pconv_hpa', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, pconv_hpa(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'ft', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, ft(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'fq', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, fq(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'sub', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, sub(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'phconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, phconv(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'phconv_hpa', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, phconv_hpa(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tconv(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qconv(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'qsconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, qsconv(:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'fmass', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, fmass(:,:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'fmassfrac', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, fmassfrac(:,:))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cbaseflux', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cbaseflux(0:nxmax-1,0:nymax-1))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'cbasefluxn', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, cbasefluxn(0:nxmaxn-1,0:nymaxn-1,1:maxnests))
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'psconv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, psconv)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'tt2conv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, tt2conv)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'td2conv', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, td2conv)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nconvlev', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nconvlev)
            call handle_nf90_err(ncret)

            ncret = nf90_inq_varid(ncid, 'nconvtop', ncvarid)
            call handle_nf90_err(ncret)
            ncret = nf90_get_var(ncid, ncvarid, nconvtop)
            call handle_nf90_err(ncret)




!            PRINT *, 'SUM(pconv): ', SUM(pconv)
!            PRINT *, 'SUM(qconv): ', SUM(qconv)
!            PRINT *, 'SUM(fmassfrac): ', SUM(fmassfrac)
!            PRINT *, 'SUM(cbasefluxn): ', SUM(cbasefluxn)
!            PRINT *, 'tt2conv: ', tt2conv
!            PRINT *, 'nconvlev: ', nconvlev



        ELSE
            STOP 'fpio(): Illegal operation' 
            
        ENDIF
    END SUBROUTINE fpio





    SUBROUTINE fpio_rawbin(iounit, op, cm_index)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: iounit
        CHARACTER(LEN=4), INTENT(IN) :: op        ! Operation - DUMP or LOAD
        INTEGER, INTENT(IN) :: cm_index           ! Index of last dimension in
                                                  ! most com_mod variables.
                                                  ! Should be 1 or 2 


        INTEGER :: temp_integer   ! temporary value


        ! These are temporary variables, used in the LOAD option, for 
        ! comparing against the current values in FLEXPART of nxmax, nymax, ...
        INTEGER :: temp_nxmax, temp_nymax, temp_nzmax, &
&                  temp_nuvzmax, temp_nwzmax, &
&                  temp_maxspec, temp_numclass,&
&                  temp_maxnests, temp_nxmaxn, temp_nymaxn, temp_preproc_fmt_str_dim

        CHARACTER(LEN=PREPROC_FMT_STR_DIM) :: temp_preproc_format_version_str

        CHARACTER(LEN=128) :: errmesg

        INTEGER, PARAMETER :: DEF_LEVEL = 3

        if (op == 'DUMP') THEN


            ! Write the preprocessing format version string
            !  NEED TO FILL THIS IN FOR NC4



            WRITE (iounit) PREPROC_FORMAT_VERSION_STR


            ! Write the compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays


            WRITE (iounit) nxmax, nymax, nzmax, nuvzmax, nwzmax




            ! Scalar values

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



!            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))

!            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



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





!            PRINT *, 'SUM(bknew(1:nzmax)): ', &
!&                                        SUM(bknew(1:nzmax))



            ! Getting ready to add in nested code

            ! These are compiled max dimensions from par_mod - these are
            ! not meant to be reassigned during a LOAD, but used as "header"
            ! information to provide the structure of arrays
            WRITE(iounit) nxn(:)
            WRITE(iounit) nyn(:)
            WRITE(iounit) dxn(:)
            WRITE(iounit) dyn(:)
            WRITE(iounit) xlon0n(:)
            WRITE(iounit) ylat0n(:)

            ! Nested, scalar values (for each nest)


            ! Nested fields, static over time
            WRITE(iounit) oron, excessoron, lsmn, xlandusen 


!           PRINT *, 'SUM(oron): ', SUM(oron)



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



!            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
!            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))



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



!            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(surfstrn): ', SUM(surfstrn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(vdepn): ', SUM(vdepn(:,:,:,cm_index,:))





            ! Auxiliary variables for nests
            WRITE(iounit) xresoln(:)
            WRITE(iounit) yresoln(:)
            WRITE(iounit) xln(:)
            WRITE(iounit) yln(:)
            WRITE(iounit) xrn(:)
            WRITE(iounit) yrn(:)



!            PRINT *, 'SUM(yresoln): ', SUM(yresoln)
!            PRINT *, 'SUM(xrn): ', SUM(xrn)



            ! Variables for polar stereographic projection
            WRITE(iounit) xglobal, sglobal, nglobal
            WRITE(iounit) switchnorthg, switchsouthg
            WRITE(iounit) southpolemap(:)
            WRITE(iounit) northpolemap(:)





!            PRINT *, 'SUM(northpolemap): ', SUM(northpolemap)
!            PRINT *, 'xglobal: ', xglobal
!            PRINT *, 'sglobal: ', sglobal
!            PRINT *, 'nglobal: ', nglobal
!            PRINT *, 'switchsouthg: ', switchsouthg

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




!            PRINT *, 'SUM(pconv): ', SUM(pconv)
!            PRINT *, 'SUM(qconv): ', SUM(qconv)
!            PRINT *, 'SUM(fmassfrac): ', SUM(fmassfrac)
!            PRINT *, 'SUM(cbasefluxn): ', SUM(cbasefluxn)
!            PRINT *, 'tt2conv: ', tt2conv
!            PRINT *, 'nconvlev: ', nconvlev



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





            ! Get dimensions
            IF ( (temp_nxmax == nxmax) .AND. (temp_nymax == nymax) .AND. &
&                   (temp_nzmax == nzmax) .AND. &
&                   (temp_nuvzmax == nuvzmax) .AND. &
&                   (temp_nwzmax == nwzmax) ) THEN
                CONTINUE
            ELSE
                PRINT *, 'Incompatible dimensions between fp file and current FLEXPART!'
                PRINT *, '                  FP file     Compiled FP'
                PRINT *, 'nxmax:     ', temp_nxmax, '    ', nxmax 
                PRINT *, 'nymax:     ', temp_nymax, '    ', nymax 
                PRINT *, 'nzmax:     ', temp_nzmax, '    ', nzmax 
                PRINT *, 'nuvzmax:     ', temp_nuvzmax, '    ', nuvzmax 
                PRINT *, 'nwzmax:     ', temp_nwzmax, '    ', nwzmax
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


!            PRINT *, 'SUM(tt(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(tt(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))


!            PRINT *, 'SUM(clouds(0:nxmax-1, 0:nymax-1, 1:nzmax, cm_index)): ', &
!&                                        SUM(clouds(0:nxmax-1,0:nymax-1,1:nzmax, cm_index))



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





!            PRINT *, 'SUM(ps(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(ps(0:nxmax-1,0:nymax-1,1, cm_index))

!            PRINT *, 'SUM(wstar(0:nxmax-1, 0:nymax-1, 1, cm_index)): ', &
!&                                        SUM(wstar(0:nxmax-1,0:nymax-1,1, cm_index))




            ! 1d fields
            READ(iounit) z0(:)
            READ(iounit) akm(:)
            READ(iounit) bkm(:)
            READ(iounit) akz(:)
            READ(iounit) bkz(:)
            READ(iounit) aknew(:)
            READ(iounit) bknew(:)



!            PRINT *, 'SUM(bknew(1:nzmax)): ', &
!&                                        SUM(bknew(1:nzmax))





            ! Nested, scalar values (for each nest)
            READ(iounit) nxn(:)
            READ(iounit) nyn(:)
            READ(iounit) dxn(:)
            READ(iounit) dyn(:)
            READ(iounit) xlon0n(:)
            READ(iounit) ylat0n(:)



            ! Nested fields, static over time
            READ(iounit) oron, excessoron, lsmn, xlandusen 



!            PRINT *, 'SUM(oron): ', SUM(oron)




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

!            PRINT *, 'SUM(uun): ', SUM(uun(:,:,:,cm_index,:))
!            PRINT *, 'SUM(qvhn): ', SUM(qvhn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(cloudsn): ', SUM(cloudsn(:,:,:,cm_index,:))




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

!            PRINT *, 'SUM(psn): ', SUM(psn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(surfstrn): ', SUM(surfstrn(:,:,:,cm_index,:))
!            PRINT *, 'SUM(vdepn): ', SUM(vdepn(:,:,:,cm_index,:))



            ! Auxiliary variables for nests
            READ(iounit) xresoln(:)
            READ(iounit) yresoln(:)
            READ(iounit) xln(:)
            READ(iounit) yln(:)
            READ(iounit) xrn(:)


!            PRINT *, 'SUM(yresoln): ', SUM(yresoln)
!            PRINT *, 'SUM(xrn): ', SUM(xrn)



            ! Variables for polar stereographic projection
            READ(iounit) xglobal, sglobal, nglobal
            READ(iounit) switchnorthg, switchsouthg
            READ(iounit) southpolemap(:)
            READ(iounit) northpolemap(:)


!            PRINT *, 'SUM(northpolemap): ', SUM(northpolemap)
!            PRINT *, 'xglobal: ', xglobal
!            PRINT *, 'sglobal: ', sglobal
!            PRINT *, 'nglobal: ', nglobal
!            PRINT *, 'switchsouthg: ', switchsouthg




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





!            PRINT *, 'SUM(pconv): ', SUM(pconv)
!            PRINT *, 'SUM(qconv): ', SUM(qconv)
!            PRINT *, 'SUM(fmassfrac): ', SUM(fmassfrac)
!            PRINT *, 'SUM(cbasefluxn): ', SUM(cbasefluxn)
!            PRINT *, 'tt2conv: ', tt2conv
!            PRINT *, 'nconvlev: ', nconvlev



        ELSE
            STOP 'fpio_rawbin(): Illegal operation' 
            
        ENDIF
    END SUBROUTINE fpio_rawbin




    subroutine handle_nf90_err(status)

        ! Custom routine for checking NF90 error status
        ! and aborting if necessary
        use netcdf
        implicit none
        integer, intent (in) :: status

        if (status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped..."
        endif
    end subroutine handle_nf90_err


    INTEGER FUNCTION logical2integer(logical_value)
        IMPLICIT NONE

        ! Auxiliary function to convert logical values to
        ! integers.  THIS DOES NO TYPE CHECKING!!!


        LOGICAL, INTENT(IN) :: logical_value

        IF (logical_value .EQV. .TRUE.) THEN
            logical2integer = 1
        ELSE
            logical2integer = 0
        ENDIF

        RETURN

    END FUNCTION logical2integer


    LOGICAL FUNCTION integer2logical(integer_value)
        IMPLICIT NONE

        ! Auxiliary function to convert integer values to
        ! logical.  THIS DOES NO TYPE CHECKING!!!


        INTEGER, INTENT(IN) :: integer_value

        IF (integer_value .EQ. 0) THEN
            integer2logical = .FALSE.
        ELSE
            integer2logical = .TRUE.
        ENDIF

        RETURN

    END FUNCTION integer2logical


END MODULE fpmetbinary_mod
