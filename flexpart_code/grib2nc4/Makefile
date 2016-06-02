
FC = gfortran



# Location of FLEXPART source directory
FLEXPART_SRC = ..

GRIBAPI = /opt/grib-api
NETCDFF = /opt/netcdf-fortran-4.4.3
NETCDF = /opt/netcdf-c-4.4.0



BINARY = grib2nc4
OBJS = processmetfields.o
FPMODOBJS = par_mod.o com_mod.o class_vtable_mod.o cmapf_mod.o conv_mod.o 
FLXPRTOBJS = detectformat.o grib2check.o shift_field_0.o gridcheck.o \
	     readwind.o readwind_nests.o calcpar.o calcpar_nests.o \
             shift_field.o pbl_profile.o scalev.o obukhov.o \
             richardson.o ew.o getvdep.o calcpv.o obukhov_gfs.o \
             richardson_gfs.o getvdep_nests.o calcpv_nests.o psim.o psih.o \
             qvsat.o caldate.o getrb.o raerod.o getrc.o partdep.o \
             verttransform.o verttransform_nests.o readwind_gfs.o \
             calcpar_gfs.o verttransform_gfs.o gridcheck_gfs.o


VPATH = ${FLEXPART_SRC}
FFLAGS = -mcmodel=medium

INCLUDES_NETCDF = -I${NETCDFF}/include
INCLUDES = -I${GRIBAPI}/include ${INCLUDES_NETCDF}

LDFLAGS_NETCDF = -L${NETCDFF}/lib -lnetcdff -L${NETCDF}/lib -lnetcdf 
LDFLAGS = -L${GRIBAPI}/lib -lgrib_api_f90 -lgrib_api ${LDFLAGS_NETCDF}


#------------ Creating the binary ------------------

${BINARY} : ${BINARY}.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS}
	${FC} -o ${BINARY} ${BINARY}.o fp2nc4io_mod.o ${FLXPRTOBJS} ${FPMODOBJS} ${OBJS} ${LDFLAGS}

${BINARY}.o : ${BINARY}.F90 fp2nc4io_mod.mod ${FPMODOBJS} Makefile
	${FC} -c ${BINARY}.F90 ${FFLAGS} ${INCLUDES}

fp2nc4io_mod.mod : ${FPMODOBJS} 
	${FC} -c fp2nc4io_mod.F90 ${FFLAGS} ${INCLUDES}

#-----------  Entries for testing ----------------
test : clean_test test/${BINARY}_test
	mv ${BINARY}_test test/
	(cd test; export LD_LIBRARY_PATH=${NETCDFF}/lib:${NETCDF}/lib:${LD_LIBRARY_PATH}; ./${BINARY}_test EL14091912 testout.nc4 w q)

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
