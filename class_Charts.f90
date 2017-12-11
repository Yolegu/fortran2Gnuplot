module class_Charts

    use class_SeriesCollection, only : dt_SeriesCollection
    use mod_Utils
    implicit none

    character(7), parameter :: rgb_hex(7) = ["#000000",& !< black
                                            &"#FF0000",& !< red
                                            &"#00C800",& !< green
                                            &"#FFC000",& !< orange
                                            &"#0A0AFF",& !< blue
                                            &"#CC33FF",& !< purple
                                            &"#00FFFF"]  !< cyan

    !! Formats
    character(4), parameter :: dq = '"'//'""'//'"'
    character(100), parameter :: fmt_key = '("set key ", A)'
    character(100), parameter :: fmt_output = '("set output " , '//dq//', A,'//dq//')'
    character(100), parameter :: fmt_terminal = '("set terminal ", A)'
    character(100), parameter :: fmt_title = '("set title", 1X, '//dq//', A,'//dq//')'
    character(100), parameter :: fmt_xlabel = '("set xlabel", 1X, '//dq//', A,'//dq//')'
    character(100), parameter :: fmt_ylabel = '("set ylabel", 1X, '//dq//', A,'//dq//')'
    character(100), parameter :: fmt_linestyle = '("set style line ", A, " lc rgb ", '//dq//', A,'//dq//&
    &' " lt 1 dt ", A," lw 2 pt ", A, " ps 1.5")'

    type :: dt_Charts
        private
        character(:), allocatable :: xlabel
        character(:), allocatable :: ylabel
        character(:), allocatable :: title
        character(:), allocatable :: terminal
        character(:), allocatable :: key
        character(:), allocatable :: nbSeriesChar
        character(:), allocatable :: pointTypeChar(:)

        logical :: has_xlabel
        logical :: has_ylabel
        logical :: has_title
        logical :: has_key

        character(:), allocatable :: filename
        character(:), allocatable :: extension
        integer :: nbSeries
    contains
        procedure :: init
        procedure :: plotxy
        procedure :: set_key
        procedure :: set_title
        procedure :: set_xlabel
        procedure :: set_ylabel
        procedure :: set_output
        procedure :: write_gnufile
        procedure :: set_nbSeriesChar
        procedure :: set_nbSeries
    end type

contains

    !> Set a string $self%nbSeriesChar containing the number of series contained in $seriesCollection
    !<
    subroutine set_nbSeriesChar(self, seriesCollection)

        class(dt_Charts) :: self
        type(dt_SeriesCollection) :: seriesCollection
        integer :: nSeries
        integer :: nDigits
        character(:), allocatable :: nbSeriesChar

        nSeries = size(seriesCollection%serie)
        nDigits = int(floor(log10(float(nSeries)))) + 1
        if (allocated(nbSeriesChar)) deallocate (nbSeriesChar)
        allocate(character(len = nDigits) :: nbSeriesChar)
        write(nbSeriesChar,'(I0)') nSeries - 1

        self%nbSeriesChar = nbSeriesChar

    end subroutine

    !------

    subroutine set_nbSeries(self, seriesCollection)

        class(dt_Charts) :: self
        type(dt_SeriesCollection) :: seriesCollection

        self%nbSeries = seriesCollection%nb_serie

    end subroutine

    !------

    subroutine init(self, seriesCollection, title, xlabel, ylabel, key)

        class(dt_Charts) :: self
        type(dt_SeriesCollection) :: seriesCollection
        character(:), allocatable, optional, intent(in) :: title
        character(:), allocatable, optional :: xlabel
        character(:), allocatable, optional :: ylabel
        logical, optional :: key

        call self%set_nbSeriesChar(seriesCollection)
        call self%set_nbSeries(seriesCollection)

        if (present(title)) then
            call self%set_title(title)
            self%has_title = .true.
        else
            self%has_title = .false.
        end if

        if (present(xlabel)) then
            call self%set_xlabel(xlabel)
            self%has_xlabel = .true.
        else
            self%has_xlabel = .false.
        end if

        if (present(ylabel)) then
            call self%set_ylabel(ylabel)
            self%has_ylabel = .true.
        else
            self%has_ylabel = .false.
        end if

        if (present(key)) then
            call self%set_key(key)
            self%has_key = .true.
        else
            self%has_key = .false.
        end if

    end subroutine

    !------

    function get_filename(full_name) result(filename)
        character(:), allocatable :: full_name
        character(:), allocatable :: filename

        filename = full_name(:index(full_name,'.') - 1)

    end function

    !------

    function get_extension(full_name) result(extension)
        character(:), allocatable :: full_name
        character(:), allocatable :: extension

        extension = full_name(index(full_name,'.') + 1:)

    end function

    !------

    !> Defines the GNUPLOT terminal to use from the output filename's extension given by the user
    !<
    subroutine set_output(self, output)
        class(dt_Charts) :: self
        character(:), allocatable, intent(in) :: output !< Output filename

        self%filename = get_filename(output)
        self%extension = get_extension(output)

        selectcase(self%extension)
            case('jpeg', 'jpg')
                self%terminal = 'jpeg'
            case('svg')
                self%terminal = 'svg'
            case('emf')
                self%terminal = 'emf'
            case('png')
                self%terminal = 'pngcairo'
        end select

    end subroutine

    !------

    subroutine set_key(self, key)
        class(dt_Charts) :: self
        logical :: key
        if (key) then
            self%key = 'on'
        else
            self%key = 'off'
        end if
    end subroutine

    !------

    subroutine set_title(self, title)
        class(dt_Charts) :: self
        character(:), allocatable :: title
        self%title = title
    end subroutine

    !------

    subroutine set_xlabel(self, xlabel)
        class(dt_Charts) :: self
        character(:), allocatable :: xlabel
        self%xlabel = xlabel
    end subroutine

    !------

    subroutine set_ylabel(self, ylabel)
        class(dt_Charts) :: self
        character(:), allocatable :: ylabel
        self%ylabel = ylabel
    end subroutine

    !------

    subroutine write_gnufile(self, seriesCollection, filename)

        class(dt_Charts) :: self
        class(dt_SeriesCollection) :: seriesCollection
        character(:), allocatable :: filename
        character(:), allocatable :: nSeriesChar
        integer :: i
        character(:), allocatable :: ic, ptc, lt
        integer :: nDigits

        open(50, file = filename//'.gnu')

        write(50,fmt_terminal)self%terminal
        write(50,fmt_output)self%filename//"."//self%extension

        write(50,fmt_key)self%key
        write(50,fmt_title)self%title
        write(50,fmt_xlabel)self%xlabel
        write(50,fmt_ylabel)self%ylabel

        write(50,'(A)')'set style increment user'

        do i = 1, self%nbSeries

                write(50,fmt_linestyle)int2Char(i), &
                & seriesCollection%serie(i)%color%str, &
                & seriesCollection%serie(i)%dash%str, &
                & seriesCollection%serie(i)%symbol%str

        end do

        if (all(seriesCollection%serie%linestyle%intg == seriesCollection%serie(1)%linestyle%intg)) then

            write(50,'(A)')'plot for [IDX=0:'//self%nbSeriesChar//'] "'//filename//'.dat" i IDX using 1:2:3 with '//&
            & seriesCollection%serie(1)%linestyle%str//' linecolor variable'

        else

            do i = 1, self%nbSeries

                if (i == 1) then
                    write(50,'(A)')'plot "'//filename//'.dat" i '//int2char(i - 1)//' using 1:2:3 with '//&
                    & seriesCollection%serie(i)%linestyle%str//' linecolor variable,\'
                elseif (i == self%nbSeries) then
                    write(50,'(5X, A)')'"'//filename//'.dat" i '//int2char(i - 1)//' using 1:2:3 with '//&
                    & seriesCollection%serie(i)%linestyle%str//' linecolor variable'
                else
                    write(50,'(5X, A)')'"'//filename//'.dat" i '//int2char(i - 1)//' using 1:2:3 with '//&
                    & seriesCollection%serie(i)%linestyle%str//' linecolor variable,\'
                end if

            end do

        end if
        close(50)

    end subroutine

    !------

    !> Writes the values stored in arrays x and y into a single .dat file that are plotted afterward by GNUPLOT.
    !! All data are written in format col1 = x, col2 = y, col3 = color (see the related subs for more details about colors).
    !! In case of multiple series, each serie is separated from the next one by two blank lines which are treated as
    !! GNUPLOT series delimiters
    !<
    subroutine write_datfile(self, filename)

        class(dt_SeriesCollection) :: self !< calling object
        character(:), allocatable, intent(in) :: filename
        integer :: nPoints
        integer :: iPoints
        integer :: iSeries
        integer :: nSeries

        open(49, file = filename//'.dat')

        do iSeries = 1, self%nb_serie

            associate (x => self%serie(iSeries)%x, &
                     & y => self%serie(iSeries)%y)

                nPoints = size(x)

                do iPoints = 1, nPoints
                    write(49,*) x(iPoints), y(iPoints), int2Char(self%serie(iSeries)%color%intg)
                end do

                write(49,*)
                write(49,*)

            end associate

        end do

        close(49)

    end subroutine

    !------

    subroutine plotxy(self, seriesCollection, output)

        class(dt_Charts) :: self
        type(dt_SeriesCollection) :: seriesCollection
        character(:), allocatable :: filename
        character(:), allocatable :: output

        ! récupération du nom de fichier (filename) sans extension utilisé pour stocker
        ! (i) les données (filename.dat)
        ! (ii) le fichier gnu (filename.gnu)
        ! (iii) le graphique (filename.jpg / filename.eps...)
        filename = get_filename(output)

        ! choix du format du graphique de sortie à partir du nom fourni par l'utilisateur
        call self%set_output(output)

        ! ecriture du fichier de données qui sera exploité par GNUPLOT
        call write_datfile(seriesCollection, filename)

        ! ecriture du fichiers qui sera execute par GNUPLOT
        call self%write_gnufile(seriesCollection, filename)

        ! appel de GNUPLOT
        call execute_command_line('gnuplot '//filename//'.gnu')

    end subroutine

end module
