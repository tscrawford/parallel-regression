module modReadNetCDF
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

    public :: getData3Df, openFile, getData2Df, getData1Df, getData4Df


contains

     
    !--------------------------------------------------------------
    !
    ! !FUNCTION: popenFile (NOT CURRENTLY IN USE...)
    !
    ! !DESCRIPTION:
    ! Opens netCDF files by file name for PARALLEL support; including 
    ! directory path and MPI_COMMs.
    !

    function popenFile(filename) result(ncid)
        ! !ARGUMENTS
        character (len = *), intent(in) :: filename

        ! !LOCAL VARIABLES:
        integer :: ncid, status
        print*, '------------', filename
        
        !! OPEN A FILE - inlcuding parallel IO
        status = nf90_open(filename, IOR(NF90_NOWRITE, NF90_MPIIO), ncid)


        if (status /= 0) then
            print *, 'Ah SNAP!!!'
            stop 'Error PARALLEL opening file: check file name or $PATH'
        end if

        print '(a i8 /)', 'Opening netCDF file - ID:', ncid

    end function popenFile
     
    !--------------------------------------------------------------
    !
    ! !FUNCTION: openFile
    !
    ! !DESCRIPTION:
    ! Opens netCDF files by file name; including directory path
    ! if necessary.
    !

    function openFile(filename) result(ncid)
        ! !ARGUMENTS
        character (len = *), intent(in) :: filename
        ! !LOCAL VARIABLES:
        integer :: ncid, status
        print*, '------------', filename
        
        !! OPEN A FILE - inlcuding parallel IO
        status = nf90_open(filename, NF90_NOWRITE, ncid)


        if (status /= 0) then
            print *, 'Ah SNAP!!!'
            stop 'Error opening file: check file name or $PATH'
        end if

        print '(a i6 /)', 'Opening netCDF file - ID:', ncid

    end function openFile


    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: getData2Df
    !
    ! !DESCRIPTION:
    ! gets 2D netCDF floating point data by variable name
    ! (optional) specify a subrange with integer array
    !
    subroutine getData2Df(varname, ncid, datout, rng)
        ! !ARGUMENTS
        character (len = *), intent(in) :: varname
        integer, intent(in) :: ncid
        real, allocatable, intent( out) :: datout(:,:)
        integer, optional,   intent(in)    :: rng(4)
        ! !LOCAL VARIABLES
        character (len = 25) :: dimname
        integer :: status, index, d1, d2, vx,vy
        integer, dimension(2) :: varDimIds,beg

        !  find the variable index first
        !  return value is "index"
        status = nf90_inq_varid(ncid, varname, index)

        ! get information about variable
        !  "varDimIds" gets the dimension index values used to
        !  allocate variable memory
        !
        ! SEE 3D Case for EXAMPLE!
        !

        status = nf90_inquire_variable(ncid, index, dimids = varDimIds)

        ! get variable dimensions - one at a time :)
        status = nf90_inquire_dimension(ncid, varDimIds(1), dimname, d1)
        status = nf90_inquire_dimension(ncid, varDimIds(2), dimname, d2)

        ! checking or sub-array definition
        if( present(rng) )then

            vx = rng(2) - rng(1)+1
            vy = rng(4) - rng(3)+1
            ! allocate variable memory
            allocate( datout(vx,vy) )
            print*, 'allocating (subrange) memory for 2D variable: ',varname
            print   '(a i4 i4/)', 'dimensions:', vx, vy

            !   A(lon,lat)
            !  Example:  sub array A( 23:37 , 35:40 ) is expressed ass
            !  follows:
            !
            !  nf90_get_var( ncid, index, datout &
            !              , start=(/23,35/) &
            !              , count=(/(35-23+1),(40-37+1)/)    )
            !

            beg = (/ rng(1),rng(3) /)

            ! get Variable data
            status = nf90_get_var( ncid, index, datout,start=beg,count=(/vx,vy/) )

        else

            ! allocate variable memory
            allocate(datout(d1, d2))
            print*, 'allocating memory for 2D variable: ', varname
            print   '(a i4,i4 /)', 'dimensions:', d1, d2
            ! get Variable data
            status = nf90_get_var(ncid, index, datout)

        end if

        if (status /= 0) then
            print *, 'Ahh SNAP!! Error getting 2D variable data!'
            stop 'Possible wrong variable name?'

        end if

    end subroutine getData2Df

    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: getData3Df
    !
    ! !DESCRIPTION:
    ! gets 3D netCDF floating point data by variable name
    ! (optional) specify a subrange with integer array
    !
    subroutine getData3Df(varname, ncid, datout, rng)
        ! !ARGUMENTS
        character (len = *), intent(in) :: varname
        integer, intent(in) :: ncid
        real, allocatable, intent( out) :: datout(:,:,:)
        integer, optional,   intent(in)    :: rng(6)
        ! !LOCAL VARIABLES
        character (len = 25) :: dimname
        integer :: status, index, d1, d2, d3, vx, vy ,vz
        integer, dimension(3) :: varDimIds,beg


        !  find the variable index first
        !  return value is "index"
        status = nf90_inq_varid(ncid, varname, index)
        

        ! get information about variable
        !  "varDimIds" gets the dimension index values used to
        !  allocate variable memory
        !
        ! EXAMPLE
        !
        !        dimensions:
        !        Time = UNLIMITED ; // (1 currently)
        !        DateStrLen = 19 ;
        !        west_east = 649 ;
        !        south_north = 649 ;
        !        south_north_stag = 650 ;
        !        west_east_stag = 650 ;
        !        land_cat = 24 ;
        !        soil_cat = 16 ;
        !        month = 12 ;
        ! variable:
        !        float XLAT_M(Time, south_north, west_east) ;
        !
        !   dimension indices used for XLAT_M is 1,4,3
        !   THEREFORE:  variable assignment with use those dimension
        !   values
        !   varDimIds(1) = 3  varDimIds(2) = 4  varDimIds(3) = 1
        !

        status = nf90_inquire_variable(ncid, index, dimids = varDimIds)

        ! get variable dimensions - one at a time :)
        status = nf90_inquire_dimension(ncid, varDimIds(1), dimname, d1)
        status = nf90_inquire_dimension(ncid, varDimIds(2), dimname, d2)
        status = nf90_inquire_dimension(ncid, varDimIds(3), dimname, d3)

         ! checking or sub-array definition
        if( present(rng) )then

            vx = rng(2) - rng(1)+1
            vy = rng(4) - rng(3)+1
            vz = rng(6)
            ! allocate variable memory
            allocate( datout(vx,vy,vz) )
            !print*, 'allocating (subrange) memory for 3D variable: ',varname
            !print   '(a i6 i6 i6/)', 'dimensions:', vx, vy, vz

            !
            !  Example:  sub array A( 23:37 , 35:40, 2:5 ) is expressed
            !  ass follows:
            !
            !  nf90_get_var( ncid, index, datout &
            !              , start=(/23,35,2/) &
            !              , count=(/(35-23+1),(40-37+1), (5-2+1) /)
            !              )
            !

            beg = (/ rng(1),rng(3),rng(5) /)
            
            ! get Variable data
            status = nf90_get_var( ncid, index, datout,start=beg,count=(/vx,vy,vz/) )

        else
        
print*,'dasd',ncid,d1,d2,d3,index
            
            ! allocate variable memory
            allocate(datout(d1, d2, d3))
          
          
            
            ! get Variable data
            call check( nf90_get_var(ncid, index, datout) )
            

        end if





        if (status /= 0) then
            print *, 'Ahh SNAP!! Error getting 3D variable data!'
            stop 'Possible wrong variable name?'

        end if


    end subroutine getData3Df
    
    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: getData3Dfr
    !
    ! !DESCRIPTION:
    ! gets 3D netCDF floating point data by variable name
    ! (optional) specify a subrange with integer array
    ! no memory allocation
    !
    subroutine getData3Dfn(varname, ncid, datout, rng)
        ! !ARGUMENTS
        character (len = *), intent(in) :: varname
        integer,             intent(in) :: ncid
        real,              intent( out) :: datout(:,:,:)
        integer, optional,   intent(in) :: rng(6)
        ! !LOCAL VARIABLES
        character (len = 25) :: dimname
        integer :: status, index, d1, d2, d3, vx, vy ,vz
        integer, dimension(3) :: varDimIds,beg


        !  find the variable index first
        !  return value is "index"
        status = nf90_inq_varid(ncid, varname, index)

        ! get information about variable
        !  "varDimIds" gets the dimension index values used to
        !  allocate variable memory
        !
        ! EXAMPLE
        !
        !        dimensions:
        !        Time = UNLIMITED ; // (1 currently)
        !        DateStrLen = 19 ;
        !        west_east = 649 ;
        !        south_north = 649 ;
        !        south_north_stag = 650 ;
        !        west_east_stag = 650 ;
        !        land_cat = 24 ;
        !        soil_cat = 16 ;
        !        month = 12 ;
        ! variable:
        !        float XLAT_M(Time, south_north, west_east) ;
        !
        !   dimension indices used for XLAT_M is 1,4,3
        !   THEREFORE:  variable assignment with use those dimension
        !   values
        !   varDimIds(1) = 3  varDimIds(2) = 4  varDimIds(3) = 1
        !

        status = nf90_inquire_variable(ncid, index, dimids = varDimIds)

        ! get variable dimensions - one at a time :)
        status = nf90_inquire_dimension(ncid, varDimIds(1), dimname, d1)
        status = nf90_inquire_dimension(ncid, varDimIds(2), dimname, d2)
        status = nf90_inquire_dimension(ncid, varDimIds(3), dimname, d3)

         ! checking or sub-array definition
        if( present(rng) )then

            vx = rng(2) - rng(1)+1
            vy = rng(4) - rng(3)+1
            vz = rng(6)
            ! allocate variable memory
            !allocate( datout(vx,vy,vz) )
            !print*, 'allocating (subrange) memory for 3D variable: ',varname
            !print   '(a i6 i6 i6/)', 'dimensions:', vx, vy, vz

            !
            !  Example:  sub array A( 23:37 , 35:40, 2:5 ) is expressed
            !  ass follows:
            !
            !  nf90_get_var( ncid, index, datout &
            !              , start=(/23,35,2/) &
            !              , count=(/(35-23+1),(40-37+1), (5-2+1) /)
            !              )
            !

            beg = (/ rng(1),rng(3),rng(5) /)
            
            ! get Variable data
            status = nf90_get_var( ncid, index, datout,start=beg,count=(/vx,vy,vz/) )

        else
            ! allocate variable memory
            !allocate( datout(d1, d2, d3) )
            !print*, 'allocating memory for 3D variable: ', varname
            !print '(a i4,i4,i4 /)', 'dimensions:', d1, d2, d3
            ! get Variable data
            status = nf90_get_var(ncid, index, datout)

        end if


        if (status /= 0) then
            print *, 'Ahh SNAP!! Error getting 3D variable data!'
            stop 'Possible wrong variable name?'

        end if


    end subroutine getData3Dfn
    
    
    


    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: getData4Df
    !
    ! !DESCRIPTION:
    ! gets 4D netCDF floating point data by variable name
    !
    subroutine getData4Df(varname, ncid, datout)
        ! !ARGUMENTS
        character (len = *), intent(in) :: varname
        integer, intent(in) :: ncid
        real, allocatable, intent( out) :: datout(:,:,:,:)
        ! !LOCAL VARIABLES
        character (len = 25) :: dimname
        integer :: status, index, d1, d2, d3, d4
        integer, dimension(4) :: varDimIds

        !  find the variable index first
        !  return value is "index"
        status = nf90_inq_varid(ncid, varname, index)

        ! get information about variable
        !  "varDimIds" gets the dimension index values used to
        !  allocate variable memory
        !
        ! SEE 3D Case for EXAMPLE!
        !

        status = nf90_inquire_variable(ncid, index, dimids = varDimIds)

        ! get variable dimensions - one at a time :)
        status = nf90_inquire_dimension(ncid, varDimIds(1), dimname, d1)
        status = nf90_inquire_dimension(ncid, varDimIds(2), dimname, d2)
        status = nf90_inquire_dimension(ncid, varDimIds(3), dimname, d3)
        status = nf90_inquire_dimension(ncid, varDimIds(4), dimname, d4)

        ! allocate variable memory
        allocate(datout(d1, d2, d3, d4))
        print*, 'allocating memory for 4D variable: ', varname
        print '(a i4,i4,i4,i4 /)', 'dimensions:', d1, d2, d3, d4

        ! get Variable data
        status = nf90_get_var(ncid, index, datout)

        if (status /= 0) then
            print *, 'Ahh SNAP!! Error getting 2D variable data!'
            stop 'Possible wrong variable name?'

        end if


    end subroutine getData4Df




    !--------------------------------------------------------------
    !
    ! !SUBROUTINE: getData1Df
    !
    ! !DESCRIPTION:
    ! gets 1D netCDF floating point data by variable name
    ! (optional) specify a subrange with integer array (beg,end)
    !
    subroutine getData1Df(varname, ncid, datout, rng)
        ! !ARGUMENTS
        character (len = *), intent(in)    :: varname
        integer,             intent(in)    :: ncid
        real, allocatable,   intent(  out) :: datout(:)
        integer, optional,   intent(in)    :: rng(2)

        ! !LOCAL VARIABLES
        character (len = 25) :: dimname
        integer :: status, index, d1,veclen
        integer, dimension(1) :: varDimIds,beg,end

        !  find the variable index first
        !  return value is "index"
        status = nf90_inq_varid(ncid, varname, index)

        ! get information about variable
        !  "varDimIds" gets the dimension index values used to
        !  allocate variable memory
        !
        ! SEE 3D Case for EXAMPLE!
        !
        status = nf90_inquire_variable(ncid, index, dimids = varDimIds)

        ! get variable dimensions - one at a time :)
        status = nf90_inquire_dimension(ncid, varDimIds(1), dimname, d1)

        ! checking or sub-array definition
        if( present(rng) ) then
            veclen = rng(2) - rng(1) + 1
            ! allocate variable memory
            allocate( datout(veclen) )
            print*, 'allocating (subrange) memory for 1D variable: ',varname
            print '(a i4 /)', 'dimensions:', veclen

            beg = rng(1)
            end = rng(2)
            ! get Variable data
            status = nf90_get_var(ncid, index, datout,start=beg,count=(/veclen+1/))

        else
            ! allocate variable memory
            allocate(datout(d1))
            print*, 'allocating memory for 1D variable: ', varname
            print '(a i4 /)', 'dimensions:', d1

            ! get Variable data
            call check( nf90_get_var(ncid, index, datout) )

        end if


        if (status /= 0) then
            print *, 'Ahh SNAP!! Error getting 1D variable data!'
            stop 'Possible wrong variable name?'

        end if


    end subroutine getData1Df
    
    subroutine check(status)
       integer, intent ( in) :: status
     
      if(status /= nf90_noerr) then
          print *, trim(nf90_strerror(status))
          stop 2
       end if
   
    end subroutine check

end module modReadNetCDF
