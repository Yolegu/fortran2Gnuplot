module mod_Utils

    implicit none

contains

    !> Creates a string $ic from integer variable $i
    !<
    function int2Char(i) result(ic)

        integer, intent(in) :: i !< the integer we want to get the char equivalent
        integer :: nDigits !< number of digits in $i (1 if $i = 1..9, 2 if $i = 10..99, 3 if $i = 100..999)
        character(:), allocatable :: ic !< string version of $i

        if (i == 0) then
            nDigits = 1
        else
            nDigits = int(floor(log10(float(abs(i))))) + 1
        end if

        if (allocated(ic)) deallocate(ic)

        if (i < 0) then
            allocate(character(len = nDigits + 1) :: ic)
        else
            allocate(character(len = nDigits) :: ic)
        end if

        write(ic, "(I0)")i

    end function

end module
