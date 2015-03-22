program ketoprak
  use salak
  implicit none
  type(node), pointer::ll

  nullify(head)
  call insertn(9,1)
  call insertn(15,2)
  call insertn(3,3)
  call insertn(17,2)
!  call insertn(31,2)
  call printn()
  print *,
  call reversed()
  call printn()  
  !call sizen()
  
  stop
contains
  subroutine reversed()
    implicit none
    type(node), pointer::tmp1,tmp2, tmp3

    tmp1 => head; tmp2 => tmp1; tmp3 => tmp2
    if(.not.associated(head)) then
       print *, 'linkedList empty'
    elseif(.not.associated(head%next)) then
       return
    else
       
       do while(associated(tmp3))
          tmp2 => tmp3
          tmp3 => tmp2%next
          tmp2%next => tmp1
          tmp1 => tmp2
       end do
       nullify(head%next); head => tmp2       
    end if
    return
  end subroutine reversed
  
  subroutine insertn(idat,nth)
    implicit none
    integer idat, nth, i
    type(node), pointer::tmp,tmp2

    if(nth < 0) then; print *, 'nth must > 0'; stop; endif
    allocate(tmp); tmp%data = idat; nullify(tmp%next)
    if(.not.associated(head) .and. nth > 1) then
       print *, 'wrong insertion number, nth', nth, '> sizeof-linkedList'
    elseif(nth == 1) then
       tmp%next => head; head => tmp
    else
       tmp2 => head
       do i = 1,nth-2
!          print *, 'here', .not.associated(tmp2%next)
          tmp2 => tmp2%next
          if(.not.associated(tmp2)) then
             print *, 'nth > sizeof-linkedList'; stop
          end if
       end do
       tmp%next => tmp2%next; tmp2%next => tmp
    end if
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
