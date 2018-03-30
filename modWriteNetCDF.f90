
module modWriteNetCDF
    !--------------------------------------------------------------
    !
    ! !MODULE: modReadingNetCDF
    !
    ! !DESCRIPTION:
    ! Opens netCDF files by variable name.  The
    !
    ! !PUBLIC TYPES:
    use netcdf
    !use mpi

    implicit none
    !include 'netcdf.inc'
    
    contains
    
    function createNetcdfFile(filename,mpicom,pinfo, modeflag)result(ncid)

        character(len=*) , intent(in) :: filename
        integer, optional, intent(in) :: mpicom,modeflag
        integer, optional, intent(inout) :: pinfo
        
        !  LOCAL VARIABLES
        integer                      :: ncid,rtn
        
        
        ! creates a a file for parallel write operations
        if( present(mpicom) .and.  present(pinfo) ) then
        
        
           !rtn = nf90_create(filename, modeflag, ncid, comm=mpicom, info=pinfo )
        
        call handle_err(nf90_create(filename, modeflag, ncid, mpicom, &
        pinfo))
       
        ! creates a file for normal write operations
        else
           !rtn = nf90_create(path=filename, cmode=NF90_CLOBBER, ncid=ncid)
        
        end if


    end function createNetcdfFile
    
    
   
    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: defdim
    !
    ! !DESCRIPTION:
    ! defining dimensions . . .
    ! (optional) specify a subrange with integer array
    function definedim(ncid, dname, dnum) result(dimID)

        character(len=*),intent(in) :: dname
        !character(len=*),optional,intent(in) :: rec
        integer,intent(in) :: dnum,ncid
        
        ! LOCAL VARIABLE
        integer :: dimID       
        integer :: rtn
        
        !rtn = nf90_redef(ncid)
        if( dname == 'time')then
            rtn = nf90_def_dim(ncid, dname, nf90_unlimited, dimID)
        else
            rtn = nf90_def_dim(ncid, dname, dnum, dimID)
        end if
        
        !rtn = nf90_enddef(ncid)
        
    
    
    end function definedim
    
    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: createVar
    !
    ! !DESCRIPTION:
    ! creating variables . . .
    ! (optional) specify a subrange with integer array
    function createVar(ncid, vname, dimID, dnum) result(varid)

        character(len=*),intent(in)  :: vname
        integer,intent(in)           :: dnum,ncid
        integer,dimension(:), intent(in) :: dimID
        
        ! LOCAL VARIABLE    
        integer :: rtn, asize,varid,sizedimID
        
        
        rtn = nf90_redef(ncid)
        !  get size of array to intrpret variable dimensions
        sizedimID = size(dimID)
        
       rtn = nf90_def_var(ncid, vname, NF90_INT, dimID, varid)
       
       ! create parallel access to variable  
       !rtn = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE )

       rtn = nf90_enddef(ncid)
    
    
    end function createVar
    
    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: writeVar
    !
    ! !DESCRIPTION:
    ! writing variable values . . .
    ! (optional) specify a subrange with integer array
    function writeVar(ncid, varid, values, start, count) result(result)

        integer,intent(in)                         :: ncid,varid
        integer,dimension(:,:),intent(in)          :: values
        integer,dimension(:), optional, intent(in) :: start, count
        
        ! LOCAL VARIABLE    
        integer :: rtn, asize,result,sizedimID1, sizedimID2
        
        
        if( present(start) .and. present(count) ) then
           !  get size of array to interpret variable dimensions
           sizedimID1 = size(start)
           sizedimID2 = size(count)
        
           if( sizedimID1 /= sizedimID2 )then
        	  stop
           end if
           
           print*,start
           print*,count
           print*,values(3,3)
           
           result = nf90_put_var(ncid, varid, values, start=start, count=count)
           
        else
        
               result = nf90_put_var(ncid, varid, values)
               print*,'NOT PARALLEL'

           
        end if
        
    end function writeVar
    
    
    !     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
  subroutine handle_err(errcode)
    use netcdf
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90_strerror(errcode))
       stop 2
    endif
  end subroutine handle_err




end module modWRiteNetCDF



