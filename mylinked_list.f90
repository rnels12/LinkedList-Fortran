program ketoprak
  use salak
  implicit none
  type(node), pointer::ll

  nullify(head)
  call insertn(9)
  call insertn(15)
  call insertn(3)
  call insertn(17)  
!  print *, head%data, head%next%data, head%next%next%data
    call printn()
  !call sizen()
  
  stop
contains
  subroutine insertn(idat)
    implicit none
    integer idat
    type(node), pointer::tmp

    allocate(tmp); tmp%data = idat; nullify(tmp%next)
    tmp%next => head; head => tmp
    return
  end subroutine insertn

  subroutine printn()
    implicit none
    type(node), pointer::tmp

    tmp => head
    if(.not.associated(tmp)) then
       print *, 'no linked-list'
       return
    else
       do while(associated(tmp))
          print *, tmp%data
          tmp => tmp%next
       end do
    end if
    return
  end subroutine printn

  subroutine sizen()
    implicit none
    integer n
    type(node), pointer::tmp

    tmp => head
    if(.not.associated(tmp)) then
       print *, 'no linked-list'
       return
    else
       n = 0
       do while(associated(tmp))
          n = n+1
          tmp => tmp%next
       end do
    end if
    print *, n
    return
  end subroutine sizen

end program ketoprak
