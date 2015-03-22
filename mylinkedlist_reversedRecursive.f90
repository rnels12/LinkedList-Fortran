program ketoprak
  use salak
  implicit none
  type(node), pointer::ll

  nullify(head)
  call insertn(9)
  call insertn(15)
  call insertn(3)
  call insertn(17)
  call printn(head)
  print *,
  !  print *, head%data, head%next%data, head%next%next%data
  !ll => head
  call Reversed(head)
  call printn(head)
  
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

  recursive subroutine Reversed(tmp)
    implicit none
    type(node), pointer::tmp,tmp2,tmp3
    
    tmp3 => tmp
    if(.not.associated(tmp3%next)) then; head => tmp3; return
    end if
    call Reversed(tmp3%next)
    !print *, tmp%data
    tmp2 => tmp3%next; tmp2%next => tmp3; nullify(tmp3%next)
    return
  end subroutine Reversed

  recursive subroutine printn(tmp)
    implicit none
    type(node), pointer::tmp

    if(.not.associated(tmp)) return
    
    print*, tmp%data
    call printn(tmp%next)
    
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
