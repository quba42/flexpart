
FC = gfortran

# Location of FLEXPART source directory
FLEXPART_SRC = ..


BINARY = grib2nc4
CMP_BINARY = nc4cmp
OBJS = processmetfields.o verttransform_grib2nc4_ecmwf.o verttransform_grib2nc4_gfs.o
FPMODOBJS_ = par_mod.o com_mod.o class_vtable_mod.o cmapf_mod.o conv_mod.o 
FLXPRTOBJS_ = detectformat.o grib2check.o shift_field_0.o gridcheck.o \
	     readwind.o readwind_nests.o calcpar.o calcpar_nests.o \
             shift_field.o pbl_profile.o scalev.o obukhov.o \
             richardson.o ew.o getvdep.o calcpv.o obukhov_gfs.o \
             richardson_gfs.o getvdep_nests.o calcpv_nests.o psim.o psih.o \
             qvsat.o caldate.o getrb.o raerod.o getrc.o partdep.o \
             verttransform.o verttransform_nests.o readwind_gfs.o \
             calcpar_gfs.o verttransform_gfs.o gridcheck_gfs.o


# VPATH = ${FLEXPART_SRC}
FPMODOBJS=$(FPMODOBJS_:%=$(FLEXPART_SRC)/%)
FLXPRTOBJS=$(FLXPRTOBJS_:%=$(FLEXPART_SRC)/%)
INCPATH = -I$(PREFIX)/include
LIBPATH = -L$(PREFIX)/lib
FFLAGS = -O2 -mcmodel=medium $(INCPATH) -I$(FLEXPART_SRC)


### NetCDF link flags - use the first one for dynamic libs, the second
### one for static libs
### LDFLAGS_NETCDF = -L${NETCDFF}/lib -lnetcdff -L${NETCDF}/lib -lnetcdf 
#LDFLAGS_NETCDF=-static -L${NETCDFF}/lib -lnetcdff -L${NETCDF}/lib -lnetcdf -lnetcdf -L${HDF5}/lib -lhdf5_fortran -lhdf5_hl -lhdf5hl_fortran -lhdf5 -ldl -lz
### LDFLAGS = -L${GRIBAPI}/lib -lgrib_api_f90 -lgrib_api ${LDFLAGS_NETCDF} -ljasper -L${FLEXPART_SRC}
LDFLAGS = $(FFLAGS) $(LIBPATH) $(LIBPATH1) -lgrib_api_f90 -lgrib_api -ljasper -lnetcdff -lnetcdf -lnetcdf -lhdf5_hl -lhdf5 -ldl -lm -lcurl


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
