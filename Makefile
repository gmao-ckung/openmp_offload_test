programName = TEST_OPENMP_OFFLOAD

FC = gfortran
#OPT = -O3 -Mflushz -Mfunc32 -Kieee #NVIDIA compiler options
#OPT = -O3 -Mfunc32 -Kieee -mp=gpu -Minfo=mp -gpu=flushz #NVIDIA compiler options
OPT = -O3 -fPIC -ffree-line-length-0 -fopenmp -foffload=nvptx-none -foffload="-O3 -lgfortran -lgomp -lm" # Gfortran compiler options
#OPT = -O3 -g -march=core-avx2 -fma -qopt-report0 -ftz -align all -fno-alias -align array32byte -traceback -assume realloc_lhs -fpe3 -fp-model consistent -assume noold_maxminloc -align dcommons #-prof-gen=srcpos# Ifort compiler options
#OPT = -xhost -qopenmp -fopenmp-targets=spir64 -O3
OBJ = mainProgram.o

FFLAGS ?=$(OPT)

%.o : %.F90
	$(FC) -c -o $@ $^ $(FFLAGS) $(INC)

$(programName) : $(OBJ)
	$(FC) -o $@ $^ $(FFLAGS) $(INC) $(LIB)

clean :
	rm -rf *.o *.mod $(programName) *~ *.out output_data s_* coverage* *.gcda *.gcno *.spi *.spl *.dyn
