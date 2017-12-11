program gnuplot_fortran
    use class_SeriesCollection
    use class_Charts
    implicit none

    type(dt_Serie), allocatable :: serie(:)
    integer :: color
    integer :: symbol
    integer :: linestyle
    real(8), allocatable :: x(:), y(:)
    logical :: dash

    type(dt_SeriesCollection) :: seriesCollection

    type(dt_Charts) :: chart
    character(:), allocatable :: title
    character(:), allocatable :: xlabel
    character(:), allocatable :: ylabel
    character(:), allocatable :: output
    logical :: key

    integer :: i, j
    integer :: nPoints

    ! Création des données à dessiner
    allocate(x(21), y(21), serie(3))

    do j = 1, 3

        if (allocated(x)) deallocate(x)
        if (allocated(y)) deallocate(y)

        if (j == 1) then

            nPoints = 11
            allocate(x(nPoints), y(nPoints))

            color = 1
            symbol = 1
            linestyle = 1
            dash = .false.

            do i = 1, nPoints
                x(i) = -1.0d0 + float(i - 1) * 0.1d0
                y(i) = 2.0d0 * x(i)
            end do

        elseif (j == 2) then

            nPoints = 21
            allocate(x(nPoints), y(nPoints))

            color = 2
            symbol = 2
            linestyle = 2
            dash = .true.

            do i = 1, nPoints
                x(i) = -1.0d0 + float(i - 1) * 0.1d0
                y(i) = x(i)**3
            end do

        elseif (j == 3) then

            nPoints = 31
            allocate(x(nPoints), y(nPoints))

            color = 3
            symbol = 3
            linestyle = 3
            dash = .false.

            do i = 1, nPoints
                x(i) = -1.0d0 + float(i - 1) * 0.1d0
                y(i) = cos(x(i))
            end do

        end if

        call serie(j)%init(x, y, color, symbol, linestyle, dash)

    end do

    call seriesCollection%init(serie)

    ! Choix des options graphiques
    key = .false.
    title = 'Title'
    xlabel = 'x'
    ylabel = 'y'
    call chart%init(seriesCollection, title, xlabel, ylabel, key)

    ! Tracé du graphique
    output = 'Test.png'
    call chart%plotxy(seriesCollection, output)

end program

