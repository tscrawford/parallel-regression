
program main
   use modBroker
   use LeastSquares
   
   character(len=256) :: dirloc
   character(len=64)  :: fname,vname
   integer :: dimlat,dimlon
   integer :: beginYear,endYear,time_steps
   integer :: MatrixRows, leading_dim_matrix, wk_space, dim_work

   !!!  NOTICE TO USERS
   !  be sure to know all possible domain values in your files
   !  this program may have to be edited to check for other values
   !  besides NaN...
   !
   !  all file called as input, must be organized as yearly files.
   !  all variables must be structured; initial
   !  variable location is applied throughout file opening processes
   

   !!*********  USER INPUT ***********************************************************
   ! /labs/climate/jiminglab2/Trevor/PRISM/PRISM_CLM_forcing_data
   
   ! directory location
   dirloc = '/labs/climate/jiminglab2/Trevor/PRISM/PRISM_CLM_forcing_data/'
   
   ! file name precursor
   fname = 'PRISM_forc_hourly_Temperature_'
   
   ! variable name
   vname = 'Temp'
   
   ! beginning year
   beginYear = 1981
   
   ! ending year 
   endYear   = 2010
   
   ! grid latitude dimension
   dimlat = 649!649 ! ------>>
   
   ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   !!  WARNING - NUMBER OF PROCESSORS CANNOT EXCEED dimlon-1
   ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ! grid longitude dimension
   dimlon = 648!648
   
   ! time steps per year
   time_steps = 8760
   
   
   ! LAPACK routine input 
   
   ! Matrix row numbers must correspond with the number of timesteps 
   ! assoicated with (endYear - beginYear+1)
   !  for example: 10 years, hourly data would be set as value
   !          MatrixRows = 365*10*24 
   MatrixRows = (endYear-beginYear+1)*time_steps
   
   
   
   

   !!*********  END USER INPUT *******************
   call setLapackProperties(MatrixRows)
   
   call startBroker(dimlon,dimlat,dirloc,fname,vname,beginYear,endYear,time_steps)
   
   call endBroker()

end program main

