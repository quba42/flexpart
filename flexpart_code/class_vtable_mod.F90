
module class_vtable

    implicit none
    private

    ! Note that all public interfaces and variables should have a 
    ! "vtable" prefix
    public :: Vtable,                 &
              Vtable_record,          &
              vtable_load_by_name,    &
              vtable_get_fpname,      &
              vtable_detect_gribfile_type, &
              Vtable_GRIBFILE_TYPE_ECMWF_GRIB1,   &
              Vtable_GRIBFILE_TYPE_ECMWF_GRIB2,   &
              Vtable_GRIBFILE_TYPE_ECMWF_GRIB1_2, &
              Vtable_GRIBFILE_TYPE_NCEP_GRIB1,    &
              Vtable_GRIBFILE_TYPE_NCEP_GRIB2,    &
              Vtable_GRIBFILE_TYPE_UNKNOWN    


    integer, parameter :: VTABLE_MISSING_ENTRY = -9999

    ! These are codes for designating the type of GRIB file being
    ! looked at
    integer, parameter :: Vtable_GRIBFILE_TYPE_ECMWF_GRIB1 = 1,   &
                          Vtable_GRIBFILE_TYPE_ECMWF_GRIB2 = 2,   &
                          Vtable_GRIBFILE_TYPE_ECMWF_GRIB1_2 = 3, &
                          Vtable_GRIBFILE_TYPE_NCEP_GRIB1 = 4,    &
                          Vtable_GRIBFILE_TYPE_NCEP_GRIB2 = 5,    &
                          Vtable_GRIBFILE_TYPE_UNKNOWN = -99  
                          
    ! These codes depict the origin/model of the GRIB File
    integer, parameter :: GRIB_CENTRE_NCEP = 7, &
                          GRIB_CENTRE_ECMWF = 98

    type Vtable_record
        character(len=15) :: fpname
        integer :: paramid
        integer :: indicator_of_parameter
        integer :: discipline
        integer :: category
        integer :: number
        integer :: typesurface
        character(len=25) :: typelevel
        character(len=15) :: units
        character(len=10) :: shortname
        character(len=25) :: description
        integer :: grib_version
        character(len=10) :: center
    end type Vtable_record

    type Vtable
        logical :: initialized=.FALSE. 
        character(len=255) :: path_to_vtable_file
        integer :: num_entries = 0
        type(Vtable_record), allocatable :: the_entries(:)
    end type Vtable

contains

    


    integer function vtable_detect_gribfile_type(gribfilename)
        ! Given specified grib file, returns an integer code indicating its
        ! type to the calling program.  Numeric codes are defined as integer parameters
        ! in this module

        use grib_api        
        implicit none
        character(len=255), intent(in) :: gribfilename  ! Full path to grib file
        
        integer :: ifile, iret, igrib, grib_centre, grib_edition
        logical :: end_of_file
        logical :: grib1_detected = .FALSE., grib2_detected = .FALSE.
        
        call grib_open_file(ifile, gribfilename, 'r', iret)

        ! Use first record to detect centre and and grib version of first messages.  We will
        ! then assume that all following messages have same centre, but not necessarily same
        ! GRIB version
        call grib_new_from_file(ifile, igrib, iret)
        call grib_get(igrib, 'centre', grib_centre)
        call grib_get(igrib, 'edition', grib_edition)
        
        if (grib_edition .eq. 1) grib1_detected = .TRUE.
        if (grib_edition .eq. 2) grib2_detected = .TRUE.
        
        ! Now, iterate through the rest of records to determine if this is a mixed edition file
        end_of_file = .FALSE.
        do while (.NOT. end_of_file)    
            call grib_new_from_file(ifile, igrib, iret)
            if (iret .eq. GRIB_END_OF_FILE) then
                end_of_file = .TRUE.
            else

                ! Get edition from file
                call grib_get(igrib, 'edition', grib_edition)
                if (grib_edition .eq. 1) grib1_detected = .TRUE.
                if (grib_edition .eq. 2) grib2_detected = .TRUE.
            end if
        end do
        
    call grib_close_file(ifile)
        
    ! Determine the gribfile type depending on centre and edition(s)
    
    if (grib_centre .eq. GRIB_CENTRE_ECMWF) then
        if (grib1_detected .and. grib2_detected) then
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_ECMWF_GRIB1_2
        else if (grib1_detected .and. .not. grib2_detected) then
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_ECMWF_GRIB1
        else if (.not. grib1_detected .and. grib2_detected) then
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_ECMWF_GRIB2
        else
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_UNKNOWN                        
        endif
    else if (grib_centre .eq. GRIB_CENTRE_NCEP) then
        if (grib1_detected .and. .not. grib2_detected) then
             vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_NCEP_GRIB1       
        else if (.not. grib1_detected .and. grib2_detected) then
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_NCEP_GRIB2
        else
            vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_UNKNOWN                        
        endif
    else
        vtable_detect_gribfile_type = Vtable_GRIBFILE_TYPE_UNKNOWN
    endif

    end function vtable_detect_gribfile_type



    subroutine vtable_load_by_name(vtable_name, the_vtable_data)
        implicit none

        character(len=255), intent(in) :: vtable_name  ! Full path to vtable file

        logical :: lexist
        integer :: ierr
        integer :: num_vrecs = 0 
        integer :: vrec_idx
        character(len=255) :: file_line = ' ' 

        type(Vtable), intent(out) :: the_vtable_data    ! Data structure holding the vtable 

        type(Vtable_record) :: vrecord

        ! Make sure the file exists
        inquire(file=trim(vtable_name), exist=lexist)
        if (.not. lexist) then
            print *, 'file: ', trim(vtable_name), ' does not exist...'
            stop
        endif

        ! Open file
        open(10, file=trim(vtable_name), status='old', form='formatted', iostat=ierr)
        if (ierr .ne. 0) then
            print *, 'file: ', trim(vtable_name), ' open failed...'
            stop
        endif

        ! Go through the file once and count the vtable_records
        ! Read past headers
        file_line = ' '
        do while(file_line(1:1) .ne. '-')
            read(10, '(A255)', iostat=ierr) file_line
        enddo 

        ! Now we are at the '----------' line - process everything between
        ! here and the next '----------' line.  In this case, we just want to
        ! count
        file_line = ' '
        num_vrecs = 0
        do while(file_line(1:1) .ne. '-')
            read(10, '(A255)', iostat=ierr) file_line
            !print *, file_line
            num_vrecs = num_vrecs + 1
        enddo 
        num_vrecs = num_vrecs - 1

        !print *, 'num_vrecs: ', num_vrecs

        ! Rewind
        rewind(unit=10)

        ! Allocate array for storing the vtable records, and store
        ! num_entries
        !print *, 'Ready to allocate the_vtable_data'
        allocate(the_vtable_data%the_entries(num_vrecs))
        !print *, 'Allocated the_vtable_data'
        the_vtable_data%num_entries = num_vrecs

        ! Read, parse and store the vtable records
        ! Read past headers
        file_line = ' '
        do while(file_line(1:1) .ne. '-')
            read(10, '(A255)', iostat=ierr) file_line
            !print *, file_line
        enddo 

        ! Now we are at the '----------' line - process everything between
        ! here and the next '----------' line.  In this case, we just want to
        ! count
        file_line = ' '
        vrec_idx = 0
        do while(file_line(1:1) .ne. '-')
            read(10, '(A255)', iostat=ierr) file_line
            if (file_line(1:1) .ne. '-') then
                ! PROCESS THE LINE
                vrec_idx = vrec_idx + 1

                ! Parse the line and put it in the vtable structure
                the_vtable_data%the_entries(vrec_idx) = vtable_parse_record(file_line)
              
                !print *, the_vtable_data%the_entries(vrec_idx) 
                !print *, file_line
                !print *, 'hello'
            endif
        enddo 
        num_vrecs = num_vrecs - 1


        ! Close the file
        close(unit=10)

        the_vtable_data%initialized = .TRUE. 
        
        !print *, the_vtable_data%the_entries(1)
    end subroutine vtable_load_by_name




    type(Vtable_record) function vtable_parse_record(vtable_line)


    !!! Using a vtable line as input argument, parses into a Vtable_record, and returns that
    !!! record
        implicit none
        character(LEN=255), intent(in) :: vtable_line

        !!! This is a sample of what a Vtable line will look like
        !!! THIS NEEDS TO BE MODIFIED FOR NEW STRUCTURE (23 Nov 2015)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !vtable_line = 'UU       |   131   |     0      |    2     |    2   | &
        !           &   105      |  hybrid            | ms**-1    |    u    &
        !           &    | u wind             |     2      |'
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        ! Storage for Vtable record tokens
        character(25) :: token_fpname='',&
                         token_paramid='', &
                         token_indofparam='', &
                         token_discipline='', &
                         token_category='', &
                         token_number='', &
                         token_typesurface='', &
                         token_typelevel='', &
                         token_units='', &
                         token_shortname='', &
                         token_description='', &
                         token_gribversion='', &
                         token_center='' 

        ! These indices mark the locations of the '|' delimiter in a Vtable record
        integer :: delim_fpname_idx, &
                   delim_paramid_idx, &
                   delim_indofparam_idx, &
                   delim_disc_idx, &
                   delim_cat_idx, &
                   delim_numb_idx, &
                   delim_typesurf_idx, &
                   delim_typelevel_idx, &
                   delim_units_idx, &
                   delim_shortname_idx, &
                   delim_descr_idx, &
                   delim_version_idx, &
                   delim_center_idx

        type(Vtable_record) :: vrecord

        integer :: istat    ! Error indicator for some I/O routines

        ! Calculate the indices of each field so we can extract later
        delim_fpname_idx = index(vtable_line, '|')
        delim_paramid_idx = index(vtable_line(delim_fpname_idx+1:), '|') &
                               + delim_fpname_idx
        delim_indofparam_idx = index(vtable_line(delim_paramid_idx+1:), '|') &
                               + delim_paramid_idx
        delim_disc_idx = index(vtable_line(delim_indofparam_idx+1:), '|') &
                            + delim_indofparam_idx
        delim_cat_idx = index(vtable_line(delim_disc_idx+1:), '|') &
                           + delim_disc_idx
        delim_numb_idx = index(vtable_line(delim_cat_idx+1:), '|') &
                            + delim_cat_idx
        delim_typesurf_idx = index(vtable_line(delim_numb_idx+1:), '|') &
                            + delim_numb_idx
        delim_typelevel_idx = index(vtable_line(delim_typesurf_idx+1:), '|') &
                            + delim_typesurf_idx
        delim_units_idx = index(vtable_line(delim_typelevel_idx+1:), '|') &
                            + delim_typelevel_idx
        delim_shortname_idx = index(vtable_line(delim_units_idx+1:), '|') &
                            + delim_units_idx
        delim_descr_idx = index(vtable_line(delim_shortname_idx+1:), '|') &
                            + delim_shortname_idx
        delim_version_idx = index(vtable_line(delim_descr_idx+1:), '|') &
                            + delim_descr_idx
        delim_center_idx = index(vtable_line(delim_version_idx+1:), '|') &
                            + delim_version_idx

        ! Extract the tokens
        token_fpname = vtable_line(:delim_fpname_idx-1)
        token_paramid = vtable_line(delim_fpname_idx+1:delim_paramid_idx-1)
        token_indofparam = vtable_line(delim_paramid_idx+1:delim_indofparam_idx-1)
        token_discipline = vtable_line(delim_indofparam_idx+1:delim_disc_idx-1)
        token_category = vtable_line(delim_disc_idx+1:delim_cat_idx-1)
        token_number = vtable_line(delim_cat_idx+1:delim_numb_idx-1)
        token_typesurface = vtable_line(delim_numb_idx+1:delim_typesurf_idx-1)
        token_typelevel = vtable_line(delim_typesurf_idx+1:delim_typelevel_idx-1)
        token_units = vtable_line(delim_typelevel_idx+1:delim_units_idx-1)
        token_shortname = vtable_line(delim_units_idx+1:delim_shortname_idx-1)
        token_description = vtable_line(delim_shortname_idx+1:delim_descr_idx-1)
        token_gribversion = vtable_line(delim_descr_idx+1:delim_version_idx-1)
        token_center = vtable_line(delim_version_idx+1:delim_center_idx-1)

        ! Jam the data in the record for return
        vrecord%fpname = token_fpname

        read(token_paramid, *, iostat=istat) vrecord%paramid
        if (istat .ne. 0) vrecord%paramid = VTABLE_MISSING_ENTRY

        read(token_indofparam, *, iostat=istat) vrecord%indicator_of_parameter
        if (istat .ne. 0) vrecord%indicator_of_parameter = VTABLE_MISSING_ENTRY


        read(token_discipline, *, iostat=istat) vrecord%discipline
        if (istat .ne. 0) vrecord%discipline = VTABLE_MISSING_ENTRY

        read(token_category, *, iostat=istat) vrecord%category
        if (istat .ne. 0) vrecord%category = VTABLE_MISSING_ENTRY

        read(token_number, *, iostat=istat) vrecord%number
        if (istat .ne. 0) vrecord%number = VTABLE_MISSING_ENTRY

        read(token_typesurface, *, iostat=istat) vrecord%typesurface
        if (istat .ne. 0) vrecord%typesurface = VTABLE_MISSING_ENTRY

        vrecord%typelevel = token_typelevel
        vrecord%units = token_units
        vrecord%shortname = token_shortname
        vrecord%description = token_description

        read(token_gribversion, *, iostat=istat) vrecord%grib_version
        if (istat .ne. 0) vrecord%grib_version = VTABLE_MISSING_ENTRY

        vrecord%center = token_center


        !vrecord%fpname = 'UU'
        vtable_parse_record = vrecord

        !print *, "Hello vtable_parse_record()"
        !print *, vrecord
    end function vtable_parse_record

    character(len=15) function vtable_get_fpname(igrib, vtable_object)
    
        !!! Assumes that a calling routine has opened up a GRIB file and obtained the
        !!! grib id for a specific message.
        !!! Given a grib message and a Vtable, looks up the message parameters in the Vtable
        !!! and, if found, returns the fpname
        
        use grib_api
        implicit none
        
        integer, intent(in) :: igrib
        type(Vtable), intent(in) :: vtable_object
        
        integer :: parameter_id, category, number, discipline, edition, surface_type, &
                   level, indicator_of_parameter
        character(len=10) :: center
        
        integer :: idx
        logical :: record_match
        
        call grib_get(igrib, 'editionNumber', edition)
        call grib_get(igrib, 'level', level)
                
        if (edition .eq. 1) then
            call grib_get(igrib, 'indicatorOfParameter', indicator_of_parameter)
            call grib_get(igrib, 'indicatorOfTypeOfLevel', surface_type)
            !print *, '(edition, indicator_of_parameter, surftype, level): ', edition, indicator_of_parameter, surface_type,&
            !          level
        else if (edition .eq. 2) then
            call grib_get(igrib, 'parameterNumber', number)
            call grib_get(igrib, 'parameterCategory', category)
            call grib_get(igrib, 'discipline', discipline)
            call grib_get(igrib, 'typeOfFirstFixedSurface', surface_type)
            !print *, '(edition, number, cat, disc, surftype, level): ', edition, number, &
            !          category, discipline, surface_type, level
        else
            print *, 'Illegal edition: ', edition
            stop
        endif       

        ! Iterate through Vtable and look for a match
        vtable_get_fpname = 'None'
        record_match = .FALSE.
        idx = 1
        do while (.NOT. record_match .AND. idx .LE. vtable_object%num_entries) 



            if (edition .eq. 1) then
                if (vtable_object%the_entries(idx)%indicator_of_parameter .eq. indicator_of_parameter .and. &
                    vtable_object%the_entries(idx)%typesurface .eq. surface_type) then
                    vtable_get_fpname = vtable_object%the_entries(idx)%fpname
                    record_match = .TRUE.
                end if                               
            else if (edition .eq. 2) then
                if (vtable_object%the_entries(idx)%discipline .eq. discipline .and.    &
                    vtable_object%the_entries(idx)%number .eq. number .and.   &
                    vtable_object%the_entries(idx)%category .eq. category .and.   &
                    vtable_object%the_entries(idx)%typesurface .eq. surface_type) then
                
                    vtable_get_fpname = vtable_object%the_entries(idx)%fpname
                    record_match = .TRUE.
                end if
            else
                print *, 'Illegal edition: ', edition
                stop
            endif                  
            idx = idx + 1    
        end do
        
        
    end function vtable_get_fpname

    

end module class_vtable


