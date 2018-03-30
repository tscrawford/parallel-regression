module modInit

!use mpi
!include 'mpif.h'
contains


subroutine initMPI(rank,nprocs,ierror,tag)

   integer,intent(  out) :: rank, nprocs, ierror, tag
   
   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
   
   !call MPI_INFO_CREATE(MPI_)
   
   
end subroutine initMPI



end module modInit


