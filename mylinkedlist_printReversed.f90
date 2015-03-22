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
    call printReversed(head)
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

  recursive subroutine printReversed(tmp)
    implicit none
    type(node), pointer::tmp

    if(.not.associated(tmp)) return
    
    call printReversed(tmp%next)
    print*, tmp%data
    
    return
  end subroutine printReversed

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