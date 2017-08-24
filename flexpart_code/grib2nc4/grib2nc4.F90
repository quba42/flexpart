PROGRAM grib2nc4

    !*************************************************************************
    !  This program uses the met file preprocessing capabilities of          *
    !  FLEXPART to extract key 3d variables, process them into the FP        *
    !  coordinate system, and write to NetCDF4.                              *
    !                                                                        *
    !        Don Morton (Boreal Scientific Computing LLC)                    *
    !        Preprocessing methods, M. Harustak                              *
    !                                                                        *
    !        May 2016                                                        *
    !*************************************************************************
   !   M. Harustak                                                          *
   !   -) modification to generate the output in single precission          *
   !   -) possibility to add a lat lon selection to obtain the met variables*
   !      in the vertical levels defined in that location                   *
   !*************************************************************************

    USE par_mod
    USE com_mod

    USE netcdf
    USE fp2nc4io_mod   ! Specialised module to interface preprocessed 
                       ! FP met data with NetCDF4 files 

    IMPLICIT NONE

    LOGICAL :: metfile_exists, coordinates_provided, lat_provided, lon_provided
    INTEGER :: i, j, k
    INTEGER :: num_optional_vars, num_vars
    INTEGER, PARAMETER :: DEFLATE_LEVEL = 2  ! Compression level (0-9)
    CHARACTER(LEN=512) :: met_filepath, netcdf4_filepath, param_str, coord_name_str, coord_val_str
    CHARACTER, DIMENSION(:), ALLOCATABLE :: vars_list  ! list of variables
    INTEGER :: coordX, coordY, stat
    REAL :: coord_lat, coord_lon
    INTEGER :: metdata_format = UNKNOWN_METDATA  ! From FP par_mod

    !--------------------------------------------------------

    ! Read in mandatory arguments
    IF (IARGC() < 2) THEN
        PRINT *, 'Usage: grib2netcdf4 <inpath> <outpath> [lon=X lat=Y] [optional varnames]'
        STOP
    ELSE
        CALL GETARG(1, met_filepath) 
        !PRINT *, 'met_filepath: ', met_filepath
        CALL GETARG(2, netcdf4_filepath) 
    ENDIF

    ! We want to insert 'u', 'v', and 't' into vars_list, by default
    ! So, our list needs to have three elements, plus any optional args
    ! from the command line

    ! First, get the number of optional args and allocate vars_list,
    ! and fill the first three elements
    coordinates_provided = .FALSE.
    lat_provided = .FALSE.
    lon_provided = .FALSE.
    ALLOCATE( vars_list(IARGC()+3),stat=stat )

    vars_list(1) = 'u'
    vars_list(2) = 'v'
    vars_list(3) = 't'
        
    num_vars = 3
    DO i=3,IARGC()
        CALL GETARG(i,param_str)
        param_str = TRIM(param_str)
        j = SCAN(param_str,"=")
        if (j>1) then
            coord_name_str=param_str(1:j-1)
            coord_val_str=param_str(j+1:)
            IF ( coord_name_str == "lat" .or. coord_name_str == "LAT" ) THEN
                read(coord_val_str,*,iostat=stat) coord_lat
                if ( stat == 0 ) then
                    lat_provided = .TRUE.
                else
                    print *, "Incorrect coordinates: ", coord_val_str
                    stop
                endif
            ELSE IF ( coord_name_str == "lon" .or. coord_name_str == "LON" ) THEN
                read(coord_val_str,*,iostat=stat) coord_lon
                if ( stat == 0 ) then
                    lon_provided = .TRUE.
                else
                    print *, "Incorrect coordinates: ", coord_val_str
                    stop
                endif
            ENDIF
        else
            num_vars = num_vars + 1
            vars_list(num_vars) = param_str
        endif
    ENDDO
    IF (lat_provided .AND. lon_provided) THEN
        coordinates_provided = .TRUE.
    ENDIF

    ! Before proceeding, let's make sure the vars_list is good - otherwise,
    ! we don't want to waste time processing before finding out
    IF ( .NOT. fp2nc4io_vars_are_valid(num_vars, vars_list) ) THEN
        PRINT *, 'The variables list has an invalid variable...'
        PRINT *, 'Valid variables: '
        CALL fp2nc4io_print_valid_vars

        PRINT *,
        PRINT *, 'Your vars_list:'
        DO i=1,num_vars
            PRINT *, vars_list(i)
        ENDDO
        PRINT *,
        PRINT *, 'Exiting...'
        STOP
    ENDIF


    ! Insure that metfile_path is valid.  If so, put the file info
    ! in com_mod variables numbwf and wfname
    INQUIRE( FILE=met_filepath, EXIST=metfile_exists )
    IF ( metfile_exists ) THEN
        numbwf = 1
        wfname(1) = met_filepath
    ELSE
        PRINT *, 'Unable to find metfile: ', TRIM(met_filepath)
        STOP
    ENDIF

    ! Check the type of metdata using FP routine detectformat()
    CALL detectformat(metdata_format)
    IF (metdata_format == ECMWF_METDATA) THEN
        PRINT *, ("ECMWF met data detected...")
    ELSEIF (metdata_format == GFS_METDATA) THEN
        PRINT *, ("NCEP met data detected...")
    ELSE
        PRINT *, ("Unknown met data detected...")
        STOP
    ENDIF

    ! Set a couple of com_mod variables - I honestly don't know the reason 
    ! for this now, but it was done in GRIB2FLEXPART, so I'm repeating it here.
    ! The comment in there says "Reset the times of the wind fields that are
    ! kept in memory to no time"
    DO i=1,2
        memind(i) = i
        memtime(i) = 999999999
    ENDDO

    ! Read the model grid specifications,
    ! both for the mother domain and eventual nests
    !**********************************************
    if (metdata_format == ECMWF_METDATA) CALL gridcheck_ecmwf
    if (metdata_format == GFS_METDATA) CALL gridcheck_gfs

    ! This is not yet implemented for nests.  It would probably be trivial
    ! to do so
    !CALL gridcheck_nests

    ! If the coordinates are provided, then we need to obtain
    !  the corresponding grid indexes since this is what verttransform needs
    if ( coordinates_provided ) then
        coordX = (coord_lon-xlon0)/dx
        coordY = (coord_lat-ylat0)/dy
        print *, "Coordinates: "
        print *, "lon: ", coord_lon, ", lat: ",coord_lat
        print *, "x: ", coordX, ", y: ", coordY
    endif

    PRINT *, 'Calling processmetfields()...'
    call processmetfields( 1, metdata_format, coordinates_provided, coordX, coordY)

    PRINT *, 'Calling fp2nc4io_dump()...'
    call fp2nc4io_dump( netcdf4_filepath, num_vars, vars_list, DEFLATE_LEVEL)

    PRINT *, 'End of grib2nc4'


END PROGRAM grib2nc4
