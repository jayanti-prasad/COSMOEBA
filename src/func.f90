 FUNCTION func(x)
 use global  
 USE nrtype
 IMPLICIT NONE
 INTEGER, PARAMETER :: SSP = KIND(1.0)
 REAL(SSP), DIMENSION(ndim), INTENT(IN) :: x
 REAL(SSP), DIMENSION(ndim)  :: y 
 REAL(SSP) :: func  
 real :: cmb_likelihood
 external  cmb_likelihood 
 integer :: i 

 do i = 1, ndim 
   y(i) = x(I)
 end do 

! do i = 1, ndim
!   if (y(i) .lt. Xmin(i)) then 
!     y(i) = Xmin(i) 
!   end if 
!  if (y(i) .gt. Xmax(i)) then
!     y(i) = Xmax(i)
!   end if
! end do 
 

 func  = cmb_likelihood(y)

 END FUNCTION func

 


