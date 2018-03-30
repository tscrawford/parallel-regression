
module LeastSquares


!--------------------------------------------------------------
!
! !MODULE: LeastSquares
!
! !DESCRIPTION:
! This module contains methods for using LAPACK rotuine SGELS
!
! !PUBLIC TYPES:


integer :: numrows



contains




subroutine setLapackProperties(M)
   integer,intent(in) :: M
  
   numrows = M
   
   if( pid == 0) then
      print*,'Matrix order    :', numrows
   end if
   
   
end subroutine setLapackProperties

! http://www.netlib.org/lapack/lug/node27.html 
! http://www.netlib.org/lapack/explore-html/d1/de1/sgels_8f_source.html 
!NAME
!      SGELS - solve overdetermined or underdetermined real linear
!      systems involving an M-by-N matrix A, or its transpose,
!      using a QR or LQ factorization of A
!
! SYNOPSIS
!      SUBROUTINE SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,
!                        LWORK, INFO )
!
!
!
!
!


subroutine leastSquaresLinear(dat,x)
   real,intent(inout) :: dat(:,:)
   real,intent(inout) :: x(:)
   integer :: INFO,i,j
   real :: WORK
   real,allocatable :: mwork(:)
   
   ! initial input values for LAPACK
   WORK  =  0
   LWORK = -1
   LDM   = numrows
   INFO  = -999
   LDM   = numrows
   
   CALL SGELS( 'N', numrows, 2, 1, dat, LDM, x, size(x), WORK, LWORK, INFO  )
   
   LWORK = MIN( 100000, INT( WORK ) )
   allocate( mwork(LWORK) )
   
   CALL SGELS( 'N', numrows, 2, 1, dat, LDM, x, size(x), mwork, LWORK, INFO  )
 

   if(INFO /= 0)then
      print*,'LAPACK not successful . . . '
   end if
   
   !print*,'exit INFO:---------------->',INFO,LWORK,WORK,mwork(1)
   !print'( a1 3f12.9)','b-intercept: ',x(1)
   !print'( a1 3f12.9)','      slope: ',x(2)
   
  
end subroutine leastSquaresLinear









end module LeastSquares

