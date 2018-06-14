module global
  use camb
  Type(CAMBparams) :: PCMB_GLOBAL
  real(dl) ::  output_factor
  integer, parameter :: ndim = 6,max_len = 120 
  real, dimension (ndim) :: Xmin,Xmax
end module global 

