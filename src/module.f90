module defs 

INTERFACE
     FUNCTION func(x)
       USE nrtype
       IMPLICIT NONE
       REAL(SP), DIMENSION(:), INTENT(IN) :: x
       REAL(SP) :: func
     END FUNCTION func
  END INTERFACE
end module defs 
