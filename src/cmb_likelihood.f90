function cmb_likelihood(X)
  use CAMB
  use global 
  use wmap_likelihood_9yr
  use wmap_options
  use wmap_util
  implicit none 
  real, intent(in),dimension(ndim)  :: X  
  real :: cmb_likelihood 
  integer :: error
  integer :: il,in,i,j,l,ip !Zero if OK,ipart 
  real(8),dimension(ttmin:ttmax) :: cl_tt,cl_te,cl_ee,cl_bb,cl_tb 
  type(CAMBparams) :: PCMB
  real(8) :: like(num_WMAP),like_tot,expected_like_tot,om_k 
  integer :: tt_npix, teeebb_npix,lun,olun
  character(len=max_len) :: ScalFile,TensFile, TotFile, LensFile, LensTotFile 
  real :: Cls(ttmin:ttmax,1:4)
  logical :: GC_conventions

  GC_conventions       = .false. 
  use_TT               = .true.
  use_TE               = .true.
  use_lowl_TT          = .true.
  use_lowl_pol         = .true.
  
  call Init_Cls  ! array allocation
  
  ! set all the varaible to their global values 
     
  PCMB = PCMB_GLOBAL
     
 ! call CAMBParams_Set(PCMB)
     
  ! note that these are with h2
     
     PCMB%omegab  = X(1)
     PCMB%omegac  = X(2)
     PCMB%H0      = 100.0 * X(3) 

   !  PCMB%omegav  = X(3)   

   !  PCMB%H0 = 100.d0 * sqrt((X(1)+X(2))/(1.0d0-X(3)))
     
    ! we need the following 
     PCMB%omegab  = PCMB%omegab /(PCMB%H0/100)**2
     PCMB%omegac  = PCMB%omegac /(PCMB%H0/100)**2
     PCMB%omegav  = 1.0 -  PCMB%omegab  -  PCMB%omegac    

     PCMB%InitPower%an(1) = X(4)
     PCMB%InitPower%ScalarPowerAmp(1) = X(5)*1.0D-9
     PCMB%Reion%optical_depth=X(6)
      
     !call  CAMB_SetDefParams(PCMB)
    
      om_k =  0.0 ! 1.0-(PCMB%omegab + PCMB%omegac+PCMB%omegav)

     if (.not. CAMB_ValidateParams(PCMB)) stop 'Stopped due to parameter error'
     
     call CAMB_GetResults(PCMB, error)
     
     call CAMB_GetCls(Cls,ttmax,1,GC_conventions)
      
     do il=ttmin,ttmax 
         cl_tt(il)=output_factor * Cls(il,1)
         cl_ee(il)=output_factor * Cls(il,2)
         cl_bb(il)=output_factor * Cls(il,3)
         cl_te(il)=output_factor * Cls(il,4)
          !write(7,'(I4,4(E18.6))')il,cl_tt(il),cl_ee(il),cl_bb(il),cl_te(il)
     end do 
     
     !do l=ttmin, ttmax
     !   cl_tt(l) =  cl_tt(l) + asz_def  * cl_tt_sz(l)
     !end do
     
     call CAMB_cleanup
     
     call wmap_likelihood_dof( tt_npix, teeebb_npix )
     
     call wmap_likelihood_compute(cl_tt,cl_te,cl_ee,cl_bb,like)
     
     call wmap_likelihood_error_report
     
     like_tot = sum(like(1:num_WMAP))
    
     cmb_likelihood = like_tot 
 
    ! expected_like_tot = 7557.965820
     
    !  print '(A,F13.6)', "Computed -2ln(L)         = ",2.0 * like_tot 
    !  print '(A,F13.6)', "Expected -2ln(L)         = ",expected_like_tot
    ! print '(A,F13.6)', "      Difference         = ",2*like_tot-expected_like_tot_tmp 
   
      write(*,'(F12.3,4(F12.6),E12.4,F12.6)')-2.0 * like_tot,&
            PCMB%omegab, PCMB%omegac,PCMB%omegav,PCMB%InitPower%an(1), &
            PCMB%InitPower%ScalarPowerAmp(1), PCMB%Reion%optical_depth

end function cmb_likelihood 

