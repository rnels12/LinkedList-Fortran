program batavia
  use salak
  implicit none
  integer, pointer:: ip,jp
  integer, target::i,j
  type(node), pointer::ll,ll2

  i = 3
  ip => i
  j = 17
  jp => j
  ip = jp
  j = 33
  ip => jp
  jp => i
  print *, ip, jp, i
  
!  call insertn(ll,3)
!  head => ll
!  print *, ll%data, head%data
!  ll%data = 13
!  print *, ll%data, head%data
!  call insertn(ll2,7)
!  print *, ll%data, ll2%data
 
  stop
contains
  subroutine insertn(tmp,idat)
    implicit none
    integer idat
    type(node), pointer::tmp
    
    allocate(tmp)
    nullify(tmp%next)
    tmp%data = idat
    
    
    return
  end subroutine insertn
end program batavia
