
FC=mpif90 #pgf90
FFLAGS= -DpgiFortran -Mbounds#-w -fast 
INC1=/labs/climate/jiminglab2/Trevor/Tools/NetCDF/include
INC2=/labs/climate/jiminglab2/Trevor/Tools/PHDF5/hdf5-1.8.13/include
LIB1=/labs/climate/jiminglab2/Trevor/Tools/LAPACK/lapack-3.5.0
LIB2=/labs/climate/jiminglab2/Trevor/Tools/NetCDF/lib
LIB3=/labs/climate/jiminglab2/Trevor/Tools/PHDF5/hdf5-1.8.13/lib
OBJS= modLeastSquares.o modInit.o modWriteNetCDF.o modReadNetCDF.o modBroker.o  main.o 


%.o: %.[Ff]90
	$(FC) -c -I$(INC1) -I$(INC2)$@ $< $(FFLAGS) 

mainPROGRAM: $(OBJS)
	$(FC) -o $@ $^ -L$(LIB1) -llapack -lblas -L$(LIB2) -lnetcdff -L$(LIB2) -L$(LIB3) -lnetcdf -lnetcdf -lhdf5_hl -lhdf5 -lz -lcurl  

clean:
	$(RM) *.o *.mod mainPROGRAM
