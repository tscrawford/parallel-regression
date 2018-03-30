module modBroker

use netcdf
use modWriteNetCDF
use modReadNetCDF
use LeastSquares
include 'mpif.h'




!--------------------------------------------------------------
!
! !MODULE: modBroker
!
! !DESCRIPTION:
! This module contains methods for brokering domain
! decomposition.  The broker will issue work to other
! nodes; in sequential order.
!
! !PUBLIC TYPES:

contains


subroutine initMPI(pid,nprocs,ierror,tag)

   integer,intent(  out) :: pid, nprocs, ierror, tag
   
   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierror)
   
   !call MPI_INFO_CREATE(MPI_)
   
   
end subroutine initMPI


subroutine startBroker(dimlon,dimlat,dirloc,fname,vname,byear,eyear,steps)
   integer,intent(in   )          :: dimlon,dimlat,byear,eyear,steps
   character(len=*),intent(in   ) :: dirloc
   character(len=*),intent(in   ) :: fname,vname
   
   ! local variables
   integer :: pid,nprocs,ierror,tag
   
   ! initialization of MPI
   call initMPI(pid,nprocs,ierror,tag)
   
   ! start the sequence
   call callingTree(pid,nprocs,dimlon,dimlat,dirloc,fname, vname,byear,eyear,steps)
   
end subroutine startBroker


subroutine endBroker()
   integer :: ierror
   
   call MPI_FINALIZE(ierror)

end subroutine endBroker



subroutine callingTree(pid, nprocs, dimlon, dimlat, dirloc, fname, vname, begyear,endyear, tsteps)
   integer, intent(in)            :: pid, nprocs
   integer,intent(in   )          :: dimlon,dimlat,begyear,endyear,tsteps
   character(len=*),intent(in   ) :: dirloc
   character(len=*),intent(in   ) :: fname,vname
   
   ! LOCAL variables
   integer,parameter :: MSG_SIZE = 4
   integer :: dest,tag,ierr, msg(MSG_SIZE), i, flag, broker, rprocs, j, pinfo, ncid, incr

   integer :: init      ! variable for initial assign domain values
   integer :: blat,blon ! broker vars for domain lat/lon
   integer :: stat(MPI_STATUS_SIZE)
   
   integer :: si,ct,sv,inct
   integer :: domNum, mode_flag,veclen,fc,rncid
   integer :: wrncid,dslp,dxint,varslp,varintr,dlat,dlon
   integer,allocatable :: filencid(:)
   character(len=256) :: fileloc
   character(len=4)   :: ystr
   real,allocatable   :: datvar(:,:,:),dat2d(:,:)
   real,allocatable   :: soln(:), datarrary(:)
   
   
   if( nprocs > dimlon)then
      print*,'cannot assign more processors than total longitude grid numbers . . . '
      call MPI_FINALIZE(ierror)
      stop
   end if
   
   
   ! setting general tag values
   tag    = 1
   flag   = 1
   broker = 0
   
   ! remaining number of processors
   rprocs = nprocs-1
   
   ! total grid spaces in domain
   domNum = dimlat*dimlon
   
   msg = 0
   
   !! *********  Description *****************************
   !
   !   
   !   msg(1) --> processor address
   !   msg(2) --> lat value (grid value)
   !   msg(3) --> lon value (grid value)
   !   msg(4) --> msg cont flag
   !
   !   general Algorithm description:
   !  
   !  (1) get total number of processors, and designate process id ZERO
   !      as the broker.  This process will assign domain values to working
   !      processors.
   !   
   !  (2) All processes will open parallel access to all files
   !      (except processor ZERO)
   !
   !  (2) Worker processors will allocate the predetermined working domain
   !      values and implement there own linear least squares solution
   !      for their assigned grid point.
   !
   !  
   !
   !
   !!************************************************
   
   !  allocate fileID array
   allocate( filencid(endyear-begyear+1) )
   
   !!******************************************************************
   ! each process gets file IDs for every file associated
   ! 
   !!************
   
   if( pid > 0 .and. pid <= (endyear-begyear) ) then

       ! casting year to character array  ystr
	   write(ystr,'(I4)') begyear+pid-1  
	  
	   ! building file location string
	   fileloc = trim(dirloc)//trim(fname)//trim(ystr)//'.nc'
	   
	   print*,trim(vname),' -->open files: ',fileloc
		 
	  
	   
	   call check( nf90_open(trim(fileloc), IOR(NF90_NOWRITE, NF90_MPIIO), rncid, &
                  comm = MPI_COMM_WORLD, info = MPI_INFO_NULL ) )
		  			  		  
	   call getData3Df(trim(vname), rncid, datvar)
	   
	   print*,'pid',pid,sizeof(datvar),rncid
	   
   end if
   
   
   
   call MPI_BARRIER(MPI_COMM_WORLD,ierr)
   stop
   
   
   
   ! set mode flags to write classic-netcdf-4 files
   mode_flag = IOR(nf90_netcdf4, nf90_classic_model)
   mode_flag = IOR(mode_flag, nf90_mpiio)
   
   !! Collectively open a new netcdf file to store linear expression values
   call check( nf90_create(trim(fname)//'LSS2.nc', mode_flag, wrncid, comm = MPI_COMM_WORLD, info = MPI_INFO_NULL) )

   call check( nf90_def_dim(wrncid, "lat", dimlat, dlat) )
   call check( nf90_def_dim(wrncid, "lon", dimlon, dlon) )
   
   call check( nf90_def_var(wrncid, "intercept",     NF90_FLOAT, (/ dlon, dlat /), varslp) )
   call check( nf90_def_var(wrncid, "slope", NF90_FLOAT, (/ dlon, dlat /), varintr) )
   
   
   call check( nf90_enddef(wrncid) )
  
  
   
   
    
   
   
   
!! ********************************************************************************
!  ******   BROKER Management 
!  ********************************************************************************
   if( pid == 0 )then
      
      !!************************************************
      ! Send to every worker - initial work tasks
      !!************
      do init=1,nprocs-1
      

         msg(1) = init  ! associated processor address
         msg(2) = 505     ! lat stays constant
         msg(3) = init  ! lon varies
         
         ! Basically working on the first row/longitude values of domain
         ! send to each worker
         call MPI_Send(msg, MSG_SIZE , MPI_INTEGER, msg(1), tag, MPI_COMM_WORLD, ierr)
         
      end do
      
      
      ! adjust values to continue domain assignments
      blon = init-1
      blat = 505
      incr    = init

      !!************************************************
      !  Worker Assignment loop:
      !  Broker keeps track of domain work-openings and continues to assign new
      !  tasks; until entire domain has been worked
      !!************
      do while( incr <= domNum )
          
         !  getting request from any source 
         call MPI_RECV(msg, MSG_SIZE, MPI_INTEGER, MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, stat, ierr)
         
         ! increment to next value
	     blon = blon + 1
	     msg(3) = blon
	     msg(2) = blat
		 
		 if( blon > dimlon )then
		    
		    blon = 1
		    blat = blat + 1
		    print*,'blon:',blon,'blat:',blat
		    ! adjust 
		    msg(3) = blon
		    msg(2) = blat
		    
		 end if    
		 
         
         !!***************************************************************
         !  Since the last domain assignment has been requested,
         !  Broker will tell all other remaining processes to finalize
         !!********************************************
         
         if (incr == domNum )then 
            
            ! send one last work assignment
            msg(4) = -1   
            msg(2) = dimlat
            msg(3) = dimlon  
            j=1
            call MPI_Send(msg, MSG_SIZE , MPI_INTEGER, j, tag, MPI_COMM_WORLD, ierr)
            
            ! kill the remaining processes
            do j=2,rprocs
               msg(4) = -2
               call MPI_Send(msg, MSG_SIZE , MPI_INTEGER, j, tag, MPI_COMM_WORLD, ierr)
            end do

         else
			
            call MPI_Send(msg, MSG_SIZE , MPI_INTEGER, msg(1), tag, MPI_COMM_WORLD, ierr)
            
         end if !******************************************************************
         
        incr=incr+1
        
      end do ! End of Broker assignment loop
   
   
   
   
   
!! ********************************************************************************
!  ******   WORKER Activities
!  ********************************************************************************
   else 
   
      ! This allocated only initially here!
	  allocate(  soln( numrows )    )
      allocate( dat2d( numrows, 2 ) )
      
      !!***********************************************
      ! Receive INITIAL work from broker
      !!************************

      call MPI_RECV(msg, MSG_SIZE, MPI_INTEGER, broker, tag, MPI_COMM_WORLD, stat, ierr)
  
	  !! **********************************************
	  ! initial set of work - calling LAPACK HERE!
	  !
	  !! TODO: 
	  ! initial call to also get data shape of array datvar
	  call getData3Df(vname, filencid(1), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/) )

	  ! making sure there's data to process
	  if( isnanf(datvar(1,1,1)) >= 0  )then
	     
	     sv=1
	     
	     ! beginning with first file...
	     do ct=1,size(filencid)
	     
	        call getData3Dfn(vname, filencid(ct), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/) )
	        
	        soln(sv:(sv+tsteps-1)) = datvar(1,1,1:tsteps)
	        
	        sv=sv+tsteps
	        
	     end do
	     
         ! transfer data into LLS Vector
         do si=1,numrows
            dat2d(si,1) = 1
            dat2d(si,2) = si
         end do
      
        
         ! LAPACK CALL
         !print'( a6 i4 a6 i4)',' lon: ',msg(3),' lat: ',msg(2)
         call leastSquaresLinear(dat2d,soln)
         
         ! WRITE to file
         call check( nf90_put_var(wrncid, varslp,  (/soln(2)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
         call check( nf90_put_var(wrncid, varintr, (/soln(1)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
         
      
      else
         ! deallocate for next set of data
         !deallocate(datvar)
         
      end if
      
       
       !!************************************************************
       ! Find out if there's more work to be done
       !!*****************************
       do while(1)
          
        
          ! send message to conclude current job
          call MPI_Send(msg, MSG_SIZE , MPI_INTEGER, broker, tag, MPI_COMM_WORLD, ierr)
         
          
          ! listen for broker work assignments
          call MPI_RECV(msg, MSG_SIZE, MPI_INTEGER, broker, tag, MPI_COMM_WORLD, stat, ierr)
          
          
          !! **************************************************
          ! doing more work - calling in here LAPACK HERE!
          ! this is checking to make sure there's work to do
          if(msg(4) >= 0)then
        
             ! check the first spot to see if it's undefined
             
             ct=1
             call getData3Dfn(vname, filencid(ct), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/) )
             
             
             if( isnanf(datvar(1,1,1)) >= 0  )then
             
                sv=1
                
                ! beginning with second file...open all remaining years
	            do ct=1,size(filencid)

	               call getData3Dfn(vname, filencid(ct), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/) )
	               
	               ! build solution vector
	               soln(sv:(sv+tsteps-1)) = datvar(1,1,:)
	               
	               sv=sv+tsteps
	               
	               
	            end do
                
	         
	            ! transfer data into LLS Vector
                do si=1,numrows
                   dat2d(si,1) = 1
                   dat2d(si,2) = si
                end do
             
      
                ! LAPACK CALL
                !print'( a6 i4 a6 i4)',' lon: ',msg(3),' lat: ',msg(2)
                call leastSquaresLinear(dat2d,soln)
                
                !print*,'soln-->',soln(1),soln(2)
                ! WRITE to file
                call check( nf90_put_var(wrncid, varslp,  (/soln(2)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
                call check( nf90_put_var(wrncid, varintr, (/soln(1)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
             
             else
                ! deallocate for next set of data
                !deallocate(datvar)
                !print*,'not getting---',msg(3),msg(2),datvar(1,1,1)
                
             end if
             
             
                
          end if
       
          
          
          ! check if there's no more work
          if( msg(4) == -1 )then !------------------------------
             
             print*,'GOT negative ONE'
             call getData3Dfn(vname, filencid(1), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/))
            
             
             if( isnanf(datvar(1,1,1)) >= 0  )then
                sv=1
                ! beginning with second file...open all remaining years
	            do ct=1,size(filencid)

	               call getData3Dfn(vname, filencid(ct), datvar, (/msg(3), msg(3), msg(2), msg(2), 1, tsteps/) )
	               
	               ! build solution vector
	               soln( sv:(sv+tsteps-1) ) = datvar(1,1,:)
	               
	               sv=sv+tsteps
	               
	               
	            end do
                 	         
	            ! transfer data into LLS Vector
                do si=1,numrows
                   dat2d(si,1) = 1
                   dat2d(si,2) = si
                end do
             
	  
	            ! deallocate
                deallocate(datvar)
      
                ! LAPACK CALL
                !print'( a6 i4 a6 i4)',' lon: ',msg(3),' lat: ',msg(2)
                call leastSquaresLinear(dat2d,soln)
                ! WRITE to file
                call check( nf90_put_var(wrncid, varslp,  (/soln(2)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
                call check( nf90_put_var(wrncid, varintr, (/soln(1)/), start=(/msg(3),msg(2)/), count=(/ 1 /)) )
                
             else
             
                deallocate(datvar)
             
             end if
             
             ! after last job, exit from the main loop to finalize
             exit
             
          end if ! --------------------------------------------
          
          
          
          if( msg(4) == -2 )then
             print*,'GOT negative TWO...'
             deallocate(datvar)
             ! No more work to do: quit/finalize
             exit
          end if
           
          
       end do
       
       
   end if ! --- end of worker routine
   
   !!**********************************************************
   !! all processes closing netcdf files
   !
   !********************************************

   ct=1
   do ct=1,size(filencid)
      call checker( nf90_close( filencid(ct) ) )
   end do 
   
   ! close written file
   call checker( nf90_close(wrncid) )
   

end subroutine callingTree


subroutine checker(status)
   integer, intent ( in) :: status
     
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop 2
   end if
   
end subroutine checker






end module modBroker 
