MPI_PATH = /usr
CUDA_PATH = /usr/local/cuda-7.0

MPIFF = $(MPI_PATH)/bin/mpif90
NVCC = $(CUDA_PATH)/bin/nvcc

CUDA_LIB = $(CUDA_PATH)/lib64
CUDA_INC = $(CUDA_PATH)/include

FFLAGS = -fopenmp -c
CCFLAGS = -c

#EXEFLAGS = -fopenmp -o
EXEFLAGS = -fopenmp -lcuda -lcudart -lgcc -o

EXE = Solver

#%.o %.mod: %.F90
#	$(MPIFF) $(FFLAGS) $<


Modules += Grid_interface.mod Grid_data.mod IO_interface.mod IncompNS_interface.mod IncompNS_data.mod Poisson_interface.mod MPI_interface.mod MPI_data.mod Solver_interface.mod Driver_interface.mod physicaldata.mod \
	   Driver_data.mod HeatAD_interface.mod HeatAD_data.mod Multiphase_interface.mod Multiphase_data.mod

Objects += physicaldata.o Grid_data.o Grid_init.o IncompNS_init.o IncompNS_data.o Driver_init.o Driver_data.o IncompNS_solver.o Poisson_solver.o IO_write.o IO_display.o IO_display_v2.o \
          MPIsolver_init.o MPIsolver_finalize.o \
          MPI_data.o MPI_applyBC.o MPI_physicalBC_vel.o MPI_physicalBC_pres.o MPI_physicalBC_temp.o MPI_CollectResiduals.o Solver_init.o Solver_evolve.o Solver_finalize.o Grid_finalize.o Multiphase_data.o \
          Multiphase_init.o Multiphase_evolve.o Multiphase_solver.o HeatAD_data.o \
          HeatAD_init.o HeatAD_solver.o Solver.o

ALL_OBJS = $(Modules) $(Objects)

LINKER_OBJS = $(filter-out $(Modules),$(ALL_OBJS))


$(EXE): $(ALL_OBJS) 
	$(MPIFF) -L/$(CUDA_LIB)/ $(EXEFLAGS) $(EXE) $(LINKER_OBJS)


#Poisson_solver.o: Poisson_solver.F90
#	$(MPIFF) -mmic -c Poisson_solver.F90

%.o %.mod: %.F90
	$(MPIFF) $(LIB_PATH) $(FFLAGS) $<

clean:
	rm *.dat *.mod *.o $(EXE)
