import FlexpartExecutable as Fexec

#src_distro = '/u1/uaf/morton/ctbto/flexpart_distros/FLEXPART_GFS_1p0_2015-03-02'

src_distro = '/tmp/fpsrc'
destdir = '/tmp/xx'
makefile = 'makefile.gfs_gfortran'
exec_name = 'FLEXPART_GFS_GFORTRAN'

good_init = False
try:
    exec_obj = Fexec.FlexpartExecutable(srcdir=src_distro, destdir=destdir,
                                        makefile=makefile, 
                                        executable_name=exec_name)
    good_init = True 
except Exception as e:
    print 'Bad instantiation: ' + str(e)
    pass

if good_init:
    print 'Executable exists: ' + str(exec_obj.executable_exists())

    success = exec_obj.compile_it()

    print 'Compile success: ' + str(success)
    print 'Executable exists: ' + str(exec_obj.executable_exists())
