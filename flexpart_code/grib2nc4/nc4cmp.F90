PROGRAM nc4cmp

    !*************************************************************************
    !  This program compares variable in two NetCDF files                    *
    ! (with dimension up to 3) and it prints average and maximal difference  *
    ! between the values. It allows for specification of optional tolerance  *
    !                                                                        *
    ! Usage:                                                                 *
    !                                                                        *
    !     ./nc4cmp <file1> <file2> <variable> [optional tolerance in %]      *
    !                                                                        *
    !        M. Harustak                                                     *
    !                                                                        *
    !        March 2017                                                      *
    !*************************************************************************


    USE netcdf

    IMPLICIT NONE

    LOGICAL :: file1_exists, file2_exists
    CHARACTER(LEN=512) :: file1, file2, precision_str, variable
    CHARACTER(LEN=NF90_MAX_NAME) :: returned_name
    REAL :: tolerance
    INTEGER :: ncid, varid, retval, xtype, numDims1, numDims2, i, j, k, stat
    INTEGER, dimension(NF90_MAX_VAR_DIMS) :: varDimIDs1, varDimIDs2, dims1, dims2
    double precision :: variable_array_0d_1, variable_array_0d_2
    double precision, allocatable, dimension(:) :: variable_array_1d_1, variable_array_1d_2 
    double precision, allocatable, dimension(:,:) :: variable_array_2d_1, variable_array_2d_2
    double precision, allocatable, dimension(:,:,:) :: variable_array_3d_1, variable_array_3d_2
    double precision :: v1, v2
    double precision :: sum_diff, max_diff
    integer :: count_diff
    logical :: different = .FALSE.

    ! Read in mandatory arguments
    IF (IARGC() < 3) THEN
        PRINT *, 'Usage: nc4cmp <file1> <file2> <variable> [optional tolerance in %]'
        STOP
    ELSE
        CALL GETARG(1, file1) 
        CALL GETARG(2, file2) 
        CALL GETARG(3, variable)
    ENDIF

    IF (IARGC() > 3) THEN
        CALL GETARG( 4, precision_str)
        READ(precision_str, '(f10.5)') tolerance
    ELSE
        tolerance = 0
    ENDIF
    print *,"Tolerance ",tolerance,"%"

    INQUIRE( FILE=file1, EXIST=file1_exists )
    IF ( file1_exists ) THEN
        PRINT *, 'File 1: ', TRIM(file1)
    ELSE
        PRINT *, 'Unable to find file: ', TRIM(file1)
        STOP
    ENDIF
    INQUIRE( FILE=file2, EXIST=file2_exists )
    IF ( file2_exists ) THEN
        PRINT *, 'File 2: ', TRIM(file2)
    ELSE
        PRINT *, 'Unable to find file: ', TRIM(file2)
        STOP
    ENDIF

    print *, "Opening ",TRIM(file1)," for reading"
    retval = nf90_open(file1, NF90_NOWRITE, ncid)
    retval = nf90_inq_varid(ncid, TRIM(variable) , varid)
    retval = nf90_inquire_variable(ncid, varid, ndims = numDims1, dimids = varDimIDs1)
    print *, "Reading variable ",TRIM(variable), ", no of dimensions: ",numDims1
    do i=1,numDims1
        retval = nf90_inquire_dimension(ncid, varDimIDs1(i),len=j)
        dims1(i) = j
        print *,"  - dimension ",i,": ", j
    enddo
    if ( numDims1 == 0) then
        retval = nf90_get_var(ncid, varid, variable_array_0d_1)
    else if ( numDims1 == 1) then
        allocate(variable_array_1d_1(dims1(1)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_1d_1)
    else if ( numDims1 == 2) then
        allocate(variable_array_2d_1(dims1(1),dims1(2)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_2d_1)
    else if ( numDims1 == 3) then
        allocate(variable_array_3d_1(dims1(1),dims1(2),dims1(3)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_3d_1)
    endif
    retval = nf90_close(ncid)
    print *, "...done"

    print *, "Opening ",TRIM(file2)," for reading"
    retval = nf90_open(file2, NF90_NOWRITE, ncid)
    retval = nf90_inq_varid(ncid, TRIM(variable) , varid)
    retval = nf90_inquire_variable(ncid, varid, ndims = numDims2, dimids = varDimIDs2)
    print *, "Reading variable ",TRIM(variable), ", no of dimensions: ",numDims2
    do i=1,numDims2
        retval = nf90_inquire_dimension(ncid, varDimIDs2(i),len=j)
        dims2(i) = j 
        print *,"  - dimension ",i,": ", j
    enddo
    if ( numDims2 == 0) then
        retval = nf90_get_var(ncid, varid, variable_array_0d_2)
    else if ( numDims2 == 1) then
        allocate(variable_array_1d_2(dims2(1)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_1d_2)
    else if ( numDims2 == 2) then
        allocate(variable_array_2d_2(dims2(1),dims2(2)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_2d_2)
    else if ( numDims2 == 3) then
        allocate(variable_array_3d_2(dims2(1),dims2(2),dims2(3)),stat=stat)
        retval = nf90_get_var(ncid, varid, variable_array_3d_2)
    endif
    retval = nf90_close(ncid)
    print *, "...done"

    if (numDims1 /= numDims2) THEN
        print *, "Number of dimensions differs..."
        STOP
    endif
    do i=1,numDims1
        if (dims1(i) /= dims2(i)) then
            print *, "Dimension ",i," differs..."
            STOP
        endif
    enddo

    if (numDims1>3) then
        print *,"Only up to 3 dimensions supported..." 
        STOP
    endif

    sum_diff = 0.0
    count_diff = 0

    if (numDims1==0) then
        v1 = variable_array_0d_1
        v2 = variable_array_0d_2
        print *, "Value 1: ",v1
        print *, "Value 2: ",v2
        if ( ( v1 == 0 .AND. v2 /= 0 ) .OR. ( v1 /= 0 .AND. v2 == 0 ) ) then
            print *," === variables differ ==="
            STOP
        else if ( v1 == 0 .AND. v2 == 0) then
            print *," === variables are within tolerance ==="
            STOP
        else if ( abs((v1-v2)/v1*100) > tolerance ) then 
            print *," === variables differ ==="
            STOP
        else 
            print *," === variables are within tolerance ==="
            STOP
        endif
    endif
    if (numDims1==1) then
        do i=1,dims1(1)
            v1 = variable_array_1d_1(i)
            v2 = variable_array_1d_2(i)
            if ( ( v1 == 0 .AND. v2 /= 0 ) .OR. ( v1 /= 0 .AND. v2 == 0 ) ) then
                different = .TRUE. 
            else if (v1/=0 .AND. v2/=0 .AND. abs((v1-v2)/v1*100) > tolerance ) then
                different = .TRUE.
            endif
            if ( v1 /= 0) then 
                sum_diff = sum_diff+abs((v1-v2)/v1*100)
                count_diff = count_diff+1
                if (abs((v1-v2)/v1*100)>max_diff) then
                    max_diff=abs((v1-v2)/v1*100)
                endif
            endif
        enddo
        if ( different ) then
            print *," === variables differ ==="
        else
            print *," === variables are within tolerance ==="
        endif
        print *," === avg(diff%): ", sum_diff/count_diff
        print *," === max(diff%): ",max_diff
        STOP
    endif
    if (numDims1==2) then
        do i=1,dims1(1)
            do j=1,dims1(2)
                v1 = variable_array_2d_1(i,j)
                v2 = variable_array_2d_2(i,j)
                if ( ( v1 == 0 .AND. v2 /= 0 ) .OR. ( v1 /= 0 .AND. v2 == 0 ) ) then
                    different = .TRUE.
                else if (v1/=0 .AND. v2/=0 .AND. abs((v1-v2)/v1*100) > tolerance ) then
                    different = .TRUE.
                endif
                if (v1/=0) then
                    sum_diff = sum_diff+abs((v1-v2)/v1*100)
                    count_diff = count_diff+1
                    if (abs((v1-v2)/v1*100)>max_diff) then
                        max_diff=abs((v1-v2)/v1*100)
                    endif
                endif
            enddo
        enddo
        if ( different ) then
            print *," === variables differ ==="
        else
            print *," === variables are within tolerance ==="
        endif
        print *," === avg(diff%): ", sum_diff/count_diff
        print *," === max(diff%): ",max_diff
        STOP
    endif
    if (numDims1==3) then
        do i=1,dims1(1)
            do j=1,dims1(2)
                do k=1,dims1(3)
                    v1 = variable_array_3d_1(i,j,k)
                    v2 = variable_array_3d_2(i,j,k)
                    if ( ( v1 == 0 .AND. v2 /= 0 ) .OR. ( v1 /= 0 .AND. v2 == 0 ) ) then
                        different = .TRUE.
                    else if (v1/=0 .AND. v2/=0 .AND. abs((v1-v2)/v1*100) > tolerance ) then
                        different = .TRUE.
                    endif
                    if (v1/=0) then
                        sum_diff = sum_diff+abs((v1-v2)/v1*100)
                        count_diff = count_diff+1
                        if (abs((v1-v2)/v1*100)>max_diff) then
                            max_diff=abs((v1-v2)/v1*100)
                        endif
                    endif
                enddo
            enddo
        enddo
        if ( different ) then
            print *," === variables differ ==="
        else
            print *," === variables are within tolerance ==="
        endif
        print *," === avg(diff%): ", sum_diff/count_diff
        print *," === max(diff%): ",max_diff
        STOP
    endif


END PROGRAM nc4cmp
