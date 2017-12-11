module class_Serie

    use mod_Utils

    type :: dt_dash
        logical :: boln !< valeur utilisateur
        character(:), allocatable :: str !< valeur GNUPLOT
    end type

    type :: dt_color
        integer :: intg !< valeur utilisateur
        character(:), allocatable :: str !< valeur GNUPLOT
    end type

    type :: dt_symbol
        integer :: intg !< valeur utilisateur
        character(:), allocatable :: str !< valeur GNUPLOT
    end type

    type :: dt_linestyle
        integer :: intg !< valeur utilisateur
        character(:), allocatable :: str !< valeur GNUPLOT
    end type

    type :: dt_linetype
        integer :: intg !< valeur utilisateur
        character(:), allocatable :: str !< valeur GNUPLOT
    end type

    type :: dt_Serie
        real(8), allocatable :: x(:)
        real(8), allocatable :: y(:)
        type(dt_linetype) :: linetype
        type(dt_symbol) :: symbol
        type(dt_linestyle) :: linestyle
        type(dt_dash) :: dash
        type(dt_color) :: color

    contains
        procedure :: init
        procedure :: set_color
        procedure :: set_symbol
        procedure :: set_linestyle
        procedure :: set_x
        procedure :: set_y
        procedure :: set_dash
    end type

contains

    subroutine init(self, x, y, color, symbol, linestyle, dash)

        class(dt_Serie) :: self
        real(8) :: x(:), y(:)
        integer, optional :: color
        integer, optional :: symbol
        integer, optional :: linestyle
        logical, optional :: dash

        call self%set_x(x)
        call self%set_y(y)
        call self%set_color(color)
        call self%set_symbol(symbol)
        call self%set_linestyle(linestyle)
        call self%set_dash(dash)

    end subroutine

    !------

    subroutine set_x(self, x)

        class(dt_Serie) :: self
        real(8) :: x(:)

        if (allocated(self%x)) deallocate(self%x)
        allocate(self%x(size(x, 1)))
        self%x = x

    end subroutine

    !------

    subroutine set_y(self, y)

        class(dt_Serie) :: self
        real(8) :: y(:)

        if (allocated(self%y)) deallocate(self%y)
        allocate(self%y(size(y, 1)))
        self%y = y

    end subroutine

    !------

    !> Set the colors for GNU
    !<
    subroutine set_color(self, color)

        class(dt_Serie) :: self
        integer, optional :: color !< couleurs selectionnees

        self%color%intg = color

        if (present(color)) then
            if (color == 1) then
                self%color%str = "#000000" ! black
            elseif (color == 2) then
                self%color%str = "#FF0000" ! red
            elseif (color == 3) then
                self%color%str = "#00C800" ! green
            elseif (color == 4) then
                self%color%str = "#FFC000" ! orange
            elseif (color == 5) then
                self%color%str = "#0A0AFF" ! blue
            elseif (color == 6) then
                self%color%str = "#CC33FF" ! purple
            elseif (color == 7) then
                self%color%str = "#00FFFF" ! cyan
            else
                self%color%str = "#00FFFF" ! cyan
            end if
        else
            self%color%intg = 1
            self%color%str = "#000000" ! black
        end if

    end subroutine

    !------

    subroutine set_symbol(self, symbol)

        class(dt_Serie) :: self
        integer :: symbol
        integer :: n_series

        self%symbol%intg = symbol
        self%symbol%str = int2Char(symbol)

    end subroutine

    !------

    subroutine set_linestyle(self, linestyle)

        class(dt_Serie) :: self
        integer :: linestyle
        integer :: n_series

        self%linestyle%intg = linestyle

        if (linestyle == 1) then
            self%linestyle%str = 'lines'
        elseif (linestyle == 2) then
            self%linestyle%str = 'points'
        elseif (linestyle == 3) then
            self%linestyle%str = 'linespoints'
        else
            stop 'Linestyle not defined !'
        end if

    end subroutine

    !------

    subroutine set_dash(self, dash)
        class(dt_Serie) :: self
        logical :: dash

        self%dash%boln = dash

        if (dash .eqv. .true.) then
            self%dash%str = "2" ! pointillés
        else
            self%dash%str = "1" ! trait plein
        end if

    end subroutine

end module
