MODULE fp2nc4io_mod

    !****************************************************************
    !                                                               *
    !  Contains data and routines for dumping selected FLEXPART     *
    !  array variables to a NetCDF4 file.                           *
    !                                                               *
    !  Don Morton (Boreal Scientific Computing LLC)                 *
    !                                                               *
    !  May 2016                                                     *
    !                                                               *
    !****************************************************************

    USE par_mod
    USE com_mod

    USE netcdf

    IMPLICIT NONE

    ! This variable should be in the range [1,9].  It has been suggested
    ! that 2 offers reasonable compression in a reasonable time.
    ! Higher values will offer more compression, but will take more time
    INTEGER, PARAMETER :: DEFAULT_DEFLATE_LEVEL = 2  
    PRIVATE DEFAULT_DEFLATE_LEVEL

    ! These are valid variable names for the user of this module to reference
    !!!  DJM - 2016-06-13 -- added specific value in DIMENSION statement.
    !!!                      can't be "*" in some Fortran versions
    CHARACTER, DIMENSION(10), PARAMETER :: VALID_VARS = &
&           (/ 't', 'u', 'v', 'w', 'q',                &
&              'T', 'U', 'V', 'W', 'Q' /)
    PRIVATE VALID_VARS

    ! Private routines in this module
    PRIVATE private_dump_3dfield
    PRIVATE private_read_3dfield
    PRIVATE to_upper


CONTAINS

    SUBROUTINE fp2nc4io_print_valid_vars

        ! Prints the list of met variables that are considered valid in this
        ! module

        IMPLICIT NONE
        INTEGER :: i
        
        DO i=1,SIZE(VALID_VARS)
            PRINT *, VALID_VARS(i)
        ENDDO

    END SUBROUTINE fp2nc4io_print_valid_vars



    LOGICAL FUNCTION fp2nc4io_vars_are_valid(num_vars, dump_vars)

        ! Returns True or False depending on whether all of the variables
        ! in dump_vars are valid names (according to VALID_VARS)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: num_vars
        CHARACTER, DIMENSION(num_vars), INTENT(IN) :: dump_vars ! var list

        LOGICAL :: all_good = .TRUE.
        INTEGER :: i

        DO i=1,num_vars
            IF( .NOT. ANY(VALID_VARS == dump_vars(i)) ) THEN
                all_good = .FALSE.
            ENDIF
        ENDDO
    
        fp2nc4io_vars_are_valid = all_good

    END FUNCTION fp2nc4io_vars_are_valid



    SUBROUTINE fp2nc4io_dump(nc4_filepath, num_vars, dump_vars, deflate_level)

        ! Writes metadata plus variables in dump_vars to NetCDF4 file
        ! All of the dumped variables come from FLEXPART modules 
        ! par_mod and com_mod

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: nc4_filepath  ! Full path to dump file
        INTEGER, INTENT(IN) :: num_vars  ! Num variables in dump_vars
        CHARACTER, DIMENSION(num_vars), INTENT(IN) :: dump_vars ! var list
        INTEGER, OPTIONAL, INTENT(IN) :: deflate_level  ! (should be 0-9)


        INTEGER :: i, j, k
        INTEGER :: ncfunc_retval  ! NetCDF function call return values
        INTEGER :: ncid           ! NetCDF file id  

        
        ! Variables used by NetCDF routines
        INTEGER :: x_dimid, y_dimid, z_dimid, dimids(3)
        INTEGER :: varid
        INTEGER :: deflevel    ! Deflate level

#ifdef TESTING
        INTEGER :: nx_test, ny_test, nz_test
        REAL, ALLOCATABLE, DIMENSION(:,:,:) :: testvar_array
        CHARACTER(LEN=NF90_MAX_NAME) :: x_dimname, y_dimname, z_dimname
        REAL :: orig_array_sum, test_array_sum 
#endif

#ifdef TESTING
        PRINT *,
        PRINT *, '*** Running in testing mode ***'
        PRINT *,
#endif

        ! Use default deflate level if it wasn't passed in, or if a bad
        ! value was passed in.
        IF (PRESENT(deflate_level)) THEN
            IF (deflate_level < 0 .OR. deflate_level > 9) THEN
                deflevel = DEFAULT_DEFLATE_LEVEL
            ELSE
                deflevel = deflate_level
            ENDIF
        ELSE
            deflevel = DEFAULT_DEFLATE_LEVEL
        ENDIF

        PRINT *, 'Using deflate level: ', deflevel 

        !!!!!!---------------------------------------------------
        !!!!!!  Now we are ready to dump to NetCDF4 file
        !!!!!!---------------------------------------------------

        ncfunc_retval = nf90_create(nc4_filepath, &
&                                   OR(NF90_CLOBBER, NF90_HDF5), ncid)
        PRINT *, 'Created file: ', TRIM(nc4_filepath)

        ! Define the dimensions, and get dimension ids passed back
        ! The values nx, ny and nz come from FP par_mod
        ncfunc_retval = nf90_def_dim(ncid, 'x', nx, x_dimid)
        ncfunc_retval = nf90_def_dim(ncid, 'y', ny, y_dimid)
        ncfunc_retval = nf90_def_dim(ncid, 'z', nz, z_dimid)
        dimids = (/ x_dimid, y_dimid, z_dimid /)

        ! Write each of the 3d variables to the NetCDF file
        DO i=1,num_vars
            CALL private_dump_3dfield(ncid, dump_vars(i), dimids, deflevel)
            PRINT *, 'Dumped 3d field: ', dump_vars(i)
        ENDDO

        ! Write the height field - variable 'height' is defined in com_mod
        ncfunc_retval = nf90_def_var(ncid, 'height', NF90_DOUBLE, &
&                                    z_dimid, varid)

        ncfunc_retval = nf90_def_var_deflate(ncid, varid,   &
&                                            shuffle=0,     &
&                                            deflate=1,     &
&                                            deflate_level=deflevel)

        ncfunc_retval = nf90_put_var(ncid, varid, height(1:nz))

        ! Write some of the scalar metadata variables
        ! dx, dy, xlon0, xlat0 are all defined in com_mod 
        ncfunc_retval = nf90_def_var(ncid, 'dx', NF90_DOUBLE, varid)
        ncfunc_retval = nf90_put_var(ncid, varid, dx)

        ncfunc_retval = nf90_def_var(ncid, 'dy', NF90_DOUBLE, varid)
        ncfunc_retval = nf90_put_var(ncid, varid, dy)

        ncfunc_retval = nf90_def_var(ncid, 'xlon0', NF90_DOUBLE, varid)
        ncfunc_retval = nf90_put_var(ncid, varid, xlon0)

        ncfunc_retval = nf90_def_var(ncid, 'ylat0', NF90_DOUBLE, varid)
        ncfunc_retval = nf90_put_var(ncid, varid, ylat0)

        ! All done, close the NetCDF file
        ncfunc_retval = nf90_close(ncid)

#ifdef TESTING
        !!!!!!!!!!!!!!  Reading  !!!!!!!!!!!!!!!!!!!!
        print *, "Opening nc file for reading"
        ncfunc_retval = nf90_open(nc4_filepath, NF90_NOWRITE, ncid)

        ! Get dimensions
        ncfunc_retval = nf90_inq_dimid(ncid, 'x', x_dimid)
        ncfunc_retval = nf90_inquire_dimension(ncid, x_dimid, x_dimname, &
&                                              nx_test)
        PRINT *, 'nx_test: ', nx_test

        ncfunc_retval = nf90_inq_dimid(ncid, 'y', y_dimid)
        ncfunc_retval = nf90_inquire_dimension(ncid, y_dimid, y_dimname, &
&                                              ny_test)
        PRINT *, 'ny_test: ', ny_test

        ncfunc_retval = nf90_inq_dimid(ncid, 'z', z_dimid)
        ncfunc_retval = nf90_inquire_dimension(ncid, z_dimid, z_dimname, &
&                                              nz_test)
        PRINT *, 'nz_test: ', nz_test

        ALLOCATE( testvar_array(0:nx_test-1, 0:ny_test-1, nz_test) )

        ! Read each variable and compare with original data
        DO i=1,num_vars
            CALL private_read_3dfield(ncid, dump_vars(i), &
&                                     nx_test, ny_test, nz_test, &
&                                     testvar_array) 


            IF (to_upper(dump_vars(i)) == 'U') THEN
                orig_array_sum = SUM( uu(0:nx_test-1, 0:ny_test-1, &
&                                        1:nz_test, 1) )
            ELSEIF (to_upper(dump_vars(i)) == 'V') THEN
                orig_array_sum = SUM( vv(0:nx_test-1, 0:ny_test-1, &
&                                        1:nz_test, 1) )
            ELSEIF (to_upper(dump_vars(i)) == 'T') THEN
                orig_array_sum = SUM( tt(0:nx_test-1, 0:ny_test-1, &
&                                        1:nz_test, 1) )
            ELSEIF (to_upper(dump_vars(i)) == 'W') THEN
                orig_array_sum = SUM( ww(0:nx_test-1, 0:ny_test-1, &
&                                        1:nz_test, 1) )
            ELSEIF (to_upper(dump_vars(i)) == 'Q') THEN
                orig_array_sum = SUM( qv(0:nx_test-1, 0:ny_test-1, &
&                                        1:nz_test, 1) )
            ENDIF 

            test_array_sum = SUM( testvar_array(0:nx_test-1, 0:ny_test-1, &
&                                               1:nz_test) ) 

            PRINT *, dump_vars(i), ': ', 'SUM of differences = ', &
&                    test_array_sum - orig_array_sum
            IF ( ABS(test_array_sum - orig_array_sum) .GT. 1.0E-3 ) THEN
                PRINT *, &
&                  'WARNING WILL ROBINSON!: Sum of differences exceeds 1.0E-3' 
            ENDIF
        ENDDO

        ncfunc_retval = nf90_close(ncid)
        PRINT *, 'Closed file: ', ncfunc_retval

#endif   

    END SUBROUTINE fp2nc4io_dump


    SUBROUTINE private_dump_3dfield(ncid, varname, dimids, deflevel) 

        ! Private routine meant to provide low level access for writing
        ! specified varname to NetCDF4 file.  It is assumed that the 
        ! NC4 file has already been opened, and that dimension id's have
        ! already been obtained

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: ncid       ! NC4 file id
        CHARACTER, INTENT(IN) :: varname    
        INTEGER, INTENT(IN) :: dimids(3)  ! NC4 dimension ids
        INTEGER, INTENT(IN) :: deflevel   ! compression level

        ! NetCDF4 variables
        CHARACTER :: nc_varname
        INTEGER :: ncfunc_retval, varid

        ! Check that we have a valid varname.  If not, buh-bye
        IF( .NOT. ANY(VALID_VARS == varname) ) THEN
            PRINT *,
            PRINT *, 'fp2nc4io:private_dump_3d_field() bad var: ', varname
            PRINT *, '  ABORTING...'
            PRINT *,
            STOP
        ENDIF

        ! Convert varname to upper case for use in NetCDF file
        nc_varname = to_upper(varname)

        ! Create the variable in the NetCDF file
        ncfunc_retval = nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
&                                    dimids, varid)

        ncfunc_retval = nf90_def_var_deflate(ncid, varid,   &
&                                            shuffle=0,     &
&                                            deflate=1,     &
&                                            deflate_level=deflevel)

        ! Write the data arrays
        ! The values nx, ny and nz come from module com_mod
        ! Likewise, the arrays uu, vv, tt, ww, qv are also from the
        ! same module, and we assume they all have the same dimensions
        ! (currently they do)
        PRINT *, 'Writing: ', nc_varname
        IF (nc_varname == 'U') THEN
            ncfunc_retval = nf90_put_var(ncid, varid, &
&                                        uu(0:nx-1, 0:ny-1, 1:nz, 1))
        ELSEIF (nc_varname == 'V') THEN
            ncfunc_retval = nf90_put_var(ncid, varid, &
&                                        vv(0:nx-1, 0:ny-1, 1:nz, 1))
        ELSEIF (nc_varname == 'T') THEN
            ncfunc_retval = nf90_put_var(ncid, varid, &
&                                        tt(0:nx-1, 0:ny-1, 1:nz, 1))
        ELSEIF (nc_varname == 'W') THEN
            ncfunc_retval = nf90_put_var(ncid, varid, &
&                                        ww(0:nx-1, 0:ny-1, 1:nz, 1))
        ELSEIF (nc_varname == 'Q') THEN
            ncfunc_retval = nf90_put_var(ncid, varid, &
&                                        qv(0:nx-1, 0:ny-1, 1:nz, 1))
        ELSE
            PRINT *,
            PRINT *, 'fp2nc4io:private_dump_3d_field() bad var: ', nc_varname
            PRINT *, '  ABORTING...'
            PRINT *,
        ENDIF

        IF (ncfunc_retval /= 0) THEN
            PRINT *,
            PRINT *, '*** WARNING ***'
            PRINT *, '   fp2nc4io:private_dump_3d_field()' 
            PRINT *, '   nf90_put_var returned error for var: ', nc_varname
            PRINT *,

        ENDIF


    END SUBROUTINE private_dump_3dfield



    SUBROUTINE private_read_3dfield(ncid, varname, xdim, ydim, zdim, var_array) 

        ! Private routine for reading full 3D array, specified by varname,
        ! from NC4 file.  Reads into preallocated array of size 
        ! xdim x ydim x zdim
        IMPLICIT NONE

        INTEGER, INTENT(IN) :: ncid       ! NC4 file id
        CHARACTER, INTENT(IN) :: varname    
        INTEGER, INTENT(IN) :: xdim, ydim, zdim  ! NC4 dimension ids
        REAL, DIMENSION(xdim, ydim, zdim) :: var_array

        CHARACTER :: nc_varname
        INTEGER :: ncfunc_retval, varid

        ! Check that we have a valid varname.  If not, buh-bye
        IF( .NOT. ANY(VALID_VARS == varname) ) THEN
            PRINT *,
            PRINT *, 'fp2nc4io:private_dump_3d_field() bad var: ', varname
            PRINT *, '  ABORTING...'
            PRINT *,
            STOP
        ENDIF

        ! Convert varname to upper case for use in NetCDF file
        nc_varname = to_upper(varname)

        ! Get the varid 
        ncfunc_retval = nf90_inq_varid(ncid, nc_varname, varid)

        ! Read the variable into var_array
        ncfunc_retval = nf90_get_var(ncid, varid, var_array)

    END SUBROUTINE private_read_3dfield


    CHARACTER FUNCTION to_upper(c)

        ! Utility function to convert lower case char to upper case

        IMPLICIT NONE

        CHARACTER, INTENT(IN) :: c

        INTEGER :: c_ascii_code

        c_ascii_code = IACHAR(c)
        IF (c_ascii_code >= IACHAR("a") .AND. c_ascii_code <= IACHAR("z")) THEN
            to_upper = ACHAR(c_ascii_code - 32)
        ELSE
            to_upper = c
        ENDIF

    END FUNCTION to_upper 



END MODULE fp2nc4io_mod
