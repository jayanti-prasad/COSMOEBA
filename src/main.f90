program main 
  USE nrtype
  use camb
  use global 
  implicit none 
  real, dimension (ndim) :: X,lambda    
  real ::   cmb_likelihood
  external  cmb_likelihood 
  INTEGER, PARAMETER :: SSP = KIND(1.0)
  
  INTERFACE
     SUBROUTINE amoeba(p,y,ftol,iter)
       USE nrtype
       IMPLICIT NONE
       INTEGER(I4B), INTENT(OUT) :: iter
       REAL(SP), INTENT(IN) :: ftol
       REAL(SP), DIMENSION(:), INTENT(INOUT) :: y
       REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: p
     end  SUBROUTINE amoeba
  END INTERFACE
  REAL(SSP) :: func 
  external func  
  integer :: i,j 
  INTEGER(I4B) :: iter
  REAL(SSP)  :: ftol
  REAL(SSP), DIMENSION(1:ndim+1) :: y
  REAL(SSP), DIMENSION(1:ndim+1,1:ndim)   :: p
  real, dimension(ndim,ndim) :: e
  real, parameter :: c_param=0.2,l_param=0.5  
 
  call read_cambpar("bestfit_camb.ini",PCMB_GLOBAL,output_factor) 
  
  call print_para 

  X(1) = PCMB_GLOBAL%omegab*(PCMB_GLOBAL%H0/100)**2
  X(2) = PCMB_GLOBAL%omegac*(PCMB_GLOBAL%H0/100)**2
  X(3) = PCMB_GLOBAL%H0/100.0
  X(4) = PCMB_GLOBAL%InitPower%an(1)
  X(5) = PCMB_GLOBAL%InitPower%ScalarPowerAmp(1)*1.0e9
  X(6) = PCMB_GLOBAL%Reion%optical_depth
  
  y = cmb_likelihood(X)
  
  write(*,*)"likelihood=",y
  do i =  1, ndim
     do j =  1, ndim
        if( i.eq. j) then
           e(i,j) = 1.0
        else
           e(i,j) = 0.0
        end if
     end do
  end do
 
 ! The first vertex is set here
 
  open(11,file="searchpara.in")
  do i = 1, ndim 
     read(11,*)Xmin(i),Xmax(i)
     p(1,i)  = Xmin(i) + c_param*(Xmax(i)-Xmin(i)) 
     lambda(i) = l_param*(Xmax(i)-Xmin(i))  
  end do
  close(11) 
  
  write(*,*)"before"
  
! set rest of the vertices  

  do i =  1, ndim+1
     do j = 1 , ndim
         if ( i .gt. 1) then 
           p(i,j) = p(1,j) + lambda(j) * e(i-1,j)
          end if 
        x(j) = p(i,j)
     end do
     y(i) = func(x)
     write(70,'(F14.6,6(F12.6))')y(i), (x(j),j=1,ndim)
  end do

 ! now call amoeba   

  call amoeba(p,y,ftol,iter)
  
  write(*,*)"after"
  do i =  1, ndim +1
     do j =  1, ndim 
        x(j) = p(i,j)
     end do
     write(80,'(F12.6,6(F12.6))')y(i), (x(j),j=1,ndim)
  end do
  
  write(*,*)"iter=",iter
  
end program main


subroutine print_para
  use global 

  write(*,'(A12,F12.6)')"H0=",PCMB_GLOBAL%H0
  write(*,'(A12,F12.6)')"Obh2=",PCMB_GLOBAL%omegab*(PCMB_GLOBAL%H0/100)**2
  write(*,'(A12,F12.6)')"Och2=",PCMB_GLOBAL%omegac*(PCMB_GLOBAL%H0/100)**2
  write(*,'(A12,F12.6)')"ns=",PCMB_GLOBAL%InitPower%an(1)
  write(*,'(A12,F12.6)')"As=",PCMB_GLOBAL%InitPower%ScalarPowerAmp(1)*1.0e9
  write(*,'(A12,F12.6)')"tau=",PCMB_GLOBAL%Reion%optical_depth
  
end subroutine print_para
