
FC = gfortran



# Location of FLEXPART source directory
FLEXPART_SRC = ..

GRIBAPI = /opt/grib-api
HDF5 = /opt/hdf5-1.8.16
NETCDFF = /opt/netcdf-fortran-4.4.3
NETCDF = /opt/netcdf-c-4.4.0
#GRIBAPI = /usr/local/grib-api
#HDF5 = /usr/local/hdf5-1.8.16
#NETCDFF = /usr/local/netcdf-fortran-4.4.3
#NETCDF = /usr/local/netcdf-c-4.4.0



BINARY = grib2nc4
CMP_BINARY = nc4cmp
OBJS = processmetfields.o verttransform_grib2nc4_ecmwf.o verttransform_grib2nc4_gfs.o
FPMODOBJS = ${FLEXPART_SRC}/par_mod.o ${FLEXPART_SRC}/com_mod.o ${FLEXPART_SRC}/class_vtable_mod.o ${FLEXPART_SRC}/cmapf_mod.o ${FLEXPART_SRC}/conv_mod.o 
FLXPRTOBJS = ${FLEXPART_SRC}/detectformat.o ${FLEXPART_SRC}/grib2check.o ${FLEXPART_SRC}/shift_field_0.o ${FLEXPART_SRC}/gridcheck.o \
	     ${FLEXPART_SRC}/readwind.o ${FLEXPART_SRC}/readwind_nests.o ${FLEXPART_SRC}/calcpar.o ${FLEXPART_SRC}/calcpar_nests.o \
             ${FLEXPART_SRC}/shift_field.o ${FLEXPART_SRC}/pbl_profile.o ${FLEXPART_SRC}/scalev.o ${FLEXPART_SRC}/obukhov.o \
             ${FLEXPART_SRC}/richardson.o ${FLEXPART_SRC}/ew.o ${FLEXPART_SRC}/getvdep.o ${FLEXPART_SRC}/calcpv.o ${FLEXPART_SRC}/obukhov_gfs.o \
             ${FLEXPART_SRC}/richardson_gfs.o ${FLEXPART_SRC}/getvdep_nests.o ${FLEXPART_SRC}/calcpv_nests.o ${FLEXPART_SRC}/psim.o ${FLEXPART_SRC}/psih.o \
             ${FLEXPART_SRC}/qvsat.o ${FLEXPART_SRC}/caldate.o ${FLEXPART_SRC}/getrb.o ${FLEXPART_SRC}/raerod.o ${FLEXPART_SRC}/getrc.o ${FLEXPART_SRC}/partdep.o \
             ${FLEXPART_SRC}/verttransform.o ${FLEXPART_SRC}/verttransform_nests.o ${FLEXPART_SRC}/readwind_gfs.o \
             ${FLEXPART_SRC}/calcpar_gfs.o ${FLEXPART_SRC}/verttransform_gfs.o ${FLEXPART_SRC}/gridcheck_gfs.o


VPATH = ${FLEXPART_SRC}
FFLAGS = -mcmodel=medium

INCLUDES_NETCDF = -I${NETCDFF}/include
INCLUDES = -I${GRIBAPI}/include ${INCLUDES_NETCDF}  -I${FLEXPART_SRC}


### NetCDF link flags - use the first one for dynamic libs, the second
### one for static libs
LDFLAGS_NETCDF = -L${NETCDFF}/lib -lnetcdff -L${NETCDF}/lib -lnetcdf 
#LDFLAGS_NETCDF=-static -L${NETCDFF}/lib -lnetcdff -L${NETCDF}/lib -lnetcdf -lnetcdf -L${HDF5}/lib -lhdf5_fortran -lhdf5_hl -lhdf5hl_fortran -lhdf5 -ldl -lz


LDFLAGS = -L${GRIBAPI}/lib -lgrib_api_f90 -lgrib_api ${LDFLAGS_NETCDF} -ljasper -L${FLEXPART_SRC}


#------------ Creating the binary ------------------

${BINARY} : ${BINARY}.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS}
	${FC} -o ${BINARY} ${BINARY}.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS} ${LDFLAGS}

${BINARY}.o : ${BINARY}.F90 fp2nc4io_mod.mod ${FPMODOBJS} Makefile
	${FC} -c ${BINARY}.F90 ${FFLAGS} ${INCLUDES}

#-----------  NC4 compare ------------------------
${CMP_BINARY} : ${CMP_BINARY}.o
	${FC} -o ${CMP_BINARY} ${CMP_BINARY}.o ${LDFLAGS}

${CMP_BINARY}.o : ${CMP_BINARY}.F90 Makefile
	${FC} -c ${CMP_BINARY}.F90 ${FFLAGS} ${INCLUDES}

fp2nc4io_mod.mod : ${FPMODOBJS} 
	${FC} -c fp2nc4io_mod.F90 ${FFLAGS} ${INCLUDES}

#-----------  Entries for testing ----------------
test : clean_test test/${BINARY}_test
	mv ${BINARY}_test test/
	(cd test; export LD_LIBRARY_PATH=${NETCDFF}/lib:${NETCDF}/lib:${GRIBAPI}/lib:${LD_LIBRARY_PATH}; ./${BINARY}_test EL14091912 testout.nc4 w q)

test/${BINARY}_test : ${BINARY}_test.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS} 
	${FC} -o ${BINARY}_test ${BINARY}_test.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS} ${LDFLAGS}


${BINARY}_test.o : ${BINARY}.F90 fp2nc4io_mod_test ${FPMODOBJS} Makefile
	${FC} -c ${BINARY}.F90 -o ${BINARY}_test.o ${FFLAGS} ${INCLUDES} -DTESTING

fp2nc4io_mod_test : ${FPMODOBJS}
	${FC} -c fp2nc4io_mod.F90 ${FFLAGS} ${INCLUDES} -DTESTING 

#---------------------------------------------------
clean :
	rm -f *.o *.mod

clean_test : clean
	rm -f test/${BINARY}_test

#---------------------------------------------------
%.o: %.f90
	${FC} -c ${FFLAGS} ${INCLUDES} $<

%.o: %.F90
	${FC} -c ${FFLAGS} ${INCLUDES} $<
