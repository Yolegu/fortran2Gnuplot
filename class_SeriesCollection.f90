module class_SeriesCollection

    use class_Serie, only : dt_Serie
    implicit none

    type :: dt_SeriesCollection
        type(dt_Serie), allocatable :: serie(:)
        integer :: nb_serie
    contains
        procedure :: init
    end type

contains

    !------

    subroutine init(self, serie)

        class(dt_SeriesCollection) :: self
        type(dt_Serie) :: serie(:)
        integer :: i

        self%nb_serie = size(serie)

        if (allocated(self%serie)) deallocate(self%serie)
        allocate(self%serie(self%nb_serie))

        do i = 1, self%nb_serie
            self%serie(i) = serie(i)
        end do

    end subroutine

end module
