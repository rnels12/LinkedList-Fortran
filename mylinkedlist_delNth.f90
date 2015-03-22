program ketoprak
  use salak
  implicit none
  type(node), pointer::ll

  nullify(head)
  call insertn(9,1)
  call insertn(15,2)
  call insertn(3,3)
  call insertn(17,2)
  call printn()
  print *,
  call deln(5)
  call printn()  
  !call sizen()
  
  stop
contains
  subroutine deln(nth)
    implicit none
    integer idat, nth, i
    type(node), pointer::tmp,tmp2

    if(nth < 0) then; print *, 'nth must > 0'; stop
    endif

    tmp => head
    if(.not.associated(head)) then
       print *, 'linkedList empty'
    elseif(nth == 1) then
       head => tmp%next
    else
       
       if(.not.associated(tmp%next)) then
          print *, 'nth > sizeof-linkedList'; stop
       end if

       do i = 1,nth-2
          tmp => tmp%next
          if(.not.associated(tmp%next)) then
             print *, 'nth > sizeof-linkedList'; stop
          end if
       end do
       tmp2 => tmp; tmp => tmp%next
       tmp2%next => tmp%next
       
    end if
    nullify(tmp%next)
    deallocate(tmp)
    return
  end subroutine deln
  
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
