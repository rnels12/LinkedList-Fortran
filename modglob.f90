module salak
  type node
     !     type(node), pointer :: next, prev
     !     real, dimension(100,100) :: a
     integer data
     type(node), pointer::next
  end type node
  type(node), pointer::head
end module salak
