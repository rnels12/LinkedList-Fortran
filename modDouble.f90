module salak
  type node
     integer data
     type(node), pointer::next, prev
  end type node
  type(node), pointer::head
end module salak
