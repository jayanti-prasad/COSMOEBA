SUBROUTINE amoeba(p,y,ftol,iter)
 USE nrtype; USE nrutil, ONLY : assert_eq,imaxloc,iminloc,nrerror,swap
  IMPLICIT NONE
  INTEGER(I4B), INTENT(OUT) :: iter
  REAL(SP), INTENT(IN) :: ftol
  REAL(SP), DIMENSION(:), INTENT(INOUT) :: y
  REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: p
  REAL(SP)  :: func
  external func
  INTEGER(I4B), PARAMETER :: ITMAX=5000
  INTEGER(I4B) :: ihi,ndim
  REAL(SP), DIMENSION(size(p,2)) :: psum
  REAL(SP), DIMENSION(2) :: vec 
  integer :: i 

  !write(*,*)"init y" 
  !write(*,'(7(E14.6))') (y(i),i=1,ndim+1)
  
  call amoeba_private
CONTAINS
  !BL
  SUBROUTINE amoeba_private
    IMPLICIT NONE
    INTEGER(I4B) :: i,ilo,inhi,j
    REAL(SP) :: rtol,ysave,ytry,ytmp
    ndim=assert_eq(size(p,2),size(p,1)-1,size(y)-1,'amoeba')
    iter=0
    psum(:)=sum(p(:,:),dim=1)
    do
       ilo=iminloc(y(:))
       ihi=imaxloc(y(:))
       ytmp=y(ihi)
       y(ihi)=y(ilo)
       inhi=imaxloc(y(:))
       y(ihi)=ytmp
       if (ilo .eq. ihi) then 
         write(22,'(A12,A6,I4,A6,I4,A6,I4)')"ERR!","ihi=",ihi,"ilo=",ilo,"iter=",iter
         write(22,'(7(F14.6))') (y(i),i=1,ndim+1)
         do i =  1, ndim+1
              write(*,'(6(F14.6))') (p(i,j),j=1,ndim)
        end do
        return 
       end if
       rtol=2.0_sp*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo)))

        do i = 1, ndim+1
            write(22,'(I4,I4,6(F14.6F))')iter,i,y(i), (p(i,j),j=1,ndim) 
        end do 

       ! write(22,'(A12,I4,A12,E14.6)')"iter=",iter,"rtol=",rtol
       ! write(22,'(7(F14.6))') (y(i),i=1,ndim+1)
       ! do i =  1, ndim+1
       !    write(22,'(6(F14.6))') (p(i,j),j=1,ndim)
       ! end do

       if (rtol < ftol) then
          call swap(y(1),y(ilo))
          call swap(p(1,:),p(ilo,:))
          RETURN
       end if
       if (iter >= ITMAX) call nrerror('ITMAX exceeded in amoeba')
       ytry=amotry(-1.0_sp)
       iter=iter+1
       if (ytry <= y(ilo)) then
          ytry=amotry(2.0_sp)
          iter=iter+1
       else if (ytry >= y(inhi)) then
          ysave=y(ihi)
          ytry=amotry(0.5_sp)
          iter=iter+1
          if (ytry >= ysave) then
             p(:,:)=0.5_sp*(p(:,:)+spread(p(ilo,:),1,size(p,1)))
             do i=1,ndim+1
                if (i /= ilo) y(i)=func(p(i,:))
             end do
             iter=iter+ndim
             psum(:)=sum(p(:,:),dim=1)
          end if
       end if
    end do
  END SUBROUTINE amoeba_private
  !BL
  FUNCTION amotry(fac)
    IMPLICIT NONE
    REAL(SP), INTENT(IN) :: fac
    REAL(SP) :: amotry
    REAL(SP) :: fac1,fac2,ytry
    REAL(SP), DIMENSION(size(p,2)) :: ptry
   ! REAL(SP), DIMENSION(2) :: vec
   ! vec(1) =1.0
   ! vec(2) = 2.0
   !  write(*,*)"ppfunc(1,2)=",func(vec)

     fac1=(1.0_sp-fac)/ndim
    fac2=fac1-fac
    ptry(:)=psum(:)*fac1-p(ihi,:)*fac2
    ytry=func(ptry)
    if (ytry < y(ihi)) then
       y(ihi)=ytry
       psum(:)=psum(:)-p(ihi,:)+ptry(:)
       p(ihi,:)=ptry(:)
    end if
    amotry=ytry
   

  END FUNCTION amotry
END SUBROUTINE amoeba


subroutine status_msg(p,y, rtol, iter)
   real, dimension(:,:),intent(in) :: p 
  real, dimension(:),intent(in)  :: y 
  real, intent(in)  :: rtol 
  integer, intent(in) :: iter 
  integer :: i, j 

   write(*,'(A12,I4,A12,E14.6)')"iter=",iter,"rtol=",rtol 
   write(*,'(7(E14.6))') (y(i),i=1,ndim+1) 
   do i =  1, ndim+1 
   write(*,'(6(E14.6))') (p(i,j),j=1,ndim) 
   end do  

end subroutine status_msg











