module grid_module
    implicit none
    private
    public :: grid_type, init_grid, get_element, create_empty_grid, set_element

    type grid_type
        character(len=1), allocatable :: data(:,:)
        integer :: width, height
    end type grid_type

contains
    subroutine init_grid(grid, filename)
        type(grid_type), intent(out) :: grid
        character(len=*), intent(in) :: filename
        integer :: unit_num, io_stat, i, max_width
        character(len=1000) :: line

        open(newunit=unit_num, file=filename, status='old', action='read', iostat=io_stat)
        if (io_stat /= 0) then
            write(*,*) 'Error: Could not open file: ', trim(filename)
            stop
        end if

        grid%height = 0
        max_width = 0

        do
            read(unit_num, '(A)', iostat=io_stat) line
            if (io_stat /= 0) exit
            grid%height = grid%height + 1
            max_width = max(max_width, len_trim(line))
        end do

        grid%width = max_width

        allocate(grid%data(grid%width, grid%height))

        rewind(unit_num)
        do i = 1, grid%height
            read(unit_num, '(A)') line
            grid%data(1:len_trim(line), i) = transfer(line(1:len_trim(line)), grid%data(1:len_trim(line), i))
            if (len_trim(line) < grid%width) then
                grid%data(len_trim(line)+1:grid%width, i) = ' '
            end if
        end do

        close(unit_num)
    end subroutine init_grid

    subroutine create_empty_grid(grid, width, height, value)
        type(grid_type), intent(out) :: grid
        integer, intent(in) :: width, height
        character(len=1), intent(in) :: value

        grid%width = width
        grid%height = height
        allocate(grid%data(grid%width, grid%height))
        grid%data = value
    end subroutine create_empty_grid

    function get_element(grid, x, y) result(char)
        type(grid_type), intent(in) :: grid
        integer, intent(in) :: x, y
        character(len=1) :: char

        if (x < 1 .or. x > grid%width .or. y < 1 .or. y > grid%height) then
            char = ' '  ! Return space for out of bounds
        else
            char = grid%data(x, y)
        end if
    end function get_element

    subroutine set_element(grid, x, y, value)
        type(grid_type), intent(inout) :: grid
        integer, intent(in) :: x, y
        character(len=1), intent(in) :: value
        
        if (x >= 1 .and. x <= grid%width .and. y >= 1 .and. y <= grid%height) then
            grid%data(x, y) = value
        end if
    end subroutine set_element
end module grid_module

module coord_queue_module
    implicit none
    private
    public :: coord_queue_type, init_queue, enqueue, dequeue, is_empty

    integer, parameter :: MAX_QUEUE_SIZE = 1000 ! Adjust as needed

    type :: coord_type
        integer :: x, y
    end type coord_type

    type :: coord_queue_type
        type(coord_type) :: data(MAX_QUEUE_SIZE)
        integer :: head = 1
        integer :: tail = 0
        integer :: size = 0
    end type coord_queue_type

contains
    subroutine init_queue(queue)
        type(coord_queue_type), intent(out) :: queue
        queue%head = 1
        queue%tail = 0
        queue%size = 0
    end subroutine init_queue

    function enqueue(queue, x, y) result(success)
        type(coord_queue_type), intent(inout) :: queue
        integer, intent(in) :: x, y
        logical :: success

        if (queue%size >= MAX_QUEUE_SIZE) then
            success = .false.
            return
        end if

        queue%tail = mod(queue%tail, MAX_QUEUE_SIZE) + 1
        queue%data(queue%tail)%x = x
        queue%data(queue%tail)%y = y
        queue%size = queue%size + 1
        success = .true.
    end function enqueue

    function dequeue(queue, x, y) result(success)
        type(coord_queue_type), intent(inout) :: queue
        integer, intent(out) :: x, y
        logical :: success

        if (queue%size == 0) then
            success = .false.
            return
        end if

        x = queue%data(queue%head)%x
        y = queue%data(queue%head)%y
        queue%head = mod(queue%head, MAX_QUEUE_SIZE) + 1
        queue%size = queue%size - 1
        success = .true.
    end function dequeue

    function is_empty(queue) result(empty)
        type(coord_queue_type), intent(in) :: queue
        logical :: empty
        empty = (queue%size == 0)
    end function is_empty
end module coord_queue_module

module grid_queue_operations
    use grid_module
    use coord_queue_module
    implicit none
    private
    public :: enqueue_if_equal, fencing_price

contains
    function enqueue_if_equal(queue, grid, element, x, y) result(success)
        type(coord_queue_type), intent(inout) :: queue
        type(grid_type), intent(in) :: grid
        character(len=1), intent(in) :: element
        integer, intent(in) :: x, y
        character(len=1) :: neighbor
        logical :: success
        neighbor = get_element(grid, x, y)
        if (neighbor == element) then
            success = enqueue(queue, x, y)
        else
            success = .false.
        end if
    end function enqueue_if_equal

    function fencing_price(queue, grid, visited, start_x, start_y) result(price)
        type(coord_queue_type), intent(inout) :: queue
        type(grid_type), intent(in) :: grid
        type(grid_type), intent(inout) :: visited
        integer, intent(in) :: start_x, start_y
        character(len=1) :: element
        integer :: size, boundaries, corners
        logical :: success
        integer :: price
        integer :: x, y
        logical :: up, down, left, right

        success = enqueue(queue, start_x, start_y)

        size = 0
        boundaries = 0
        corners = 0

        do while (.not. is_empty(queue))
            success = dequeue(queue, x, y)

            if (get_element(visited, x, y) == 'X') then
                cycle
            end if

            call set_element(visited, x, y, 'X')

            element = get_element(grid, x, y)
            size = size + 1

            left = .not. enqueue_if_equal(queue, grid, element, x-1, y)
            if (left) then
                boundaries = boundaries + 1
            end if

            right = .not. enqueue_if_equal(queue, grid, element, x+1, y)
            if (right) then
                boundaries = boundaries + 1
            end if

            up = .not. enqueue_if_equal(queue, grid, element, x, y-1)
            if (up) then
                boundaries = boundaries + 1
            end if

            down = .not. enqueue_if_equal(queue, grid, element, x, y+1)
            if (down) then
                boundaries = boundaries + 1
            end if

            if (up .and. left) then
                corners = corners + 1
            end if

            if (.not. up .and. .not. left .and. get_element(grid, x-1, y-1) /= element) then
                corners = corners + 1
            end if

            if (up .and. right) then
                corners = corners + 1
            end if

            if (.not. up .and. .not. right .and. get_element(grid, x+1, y-1) /= element) then
                corners = corners + 1
            end if

            if (down .and. left) then
                corners = corners + 1
            end if

            if (.not. down .and. .not. left .and. get_element(grid, x-1, y+1) /= element) then
                corners = corners + 1
            end if

            if (down .and. right) then
                corners = corners + 1
            end if

            if (.not. down .and. .not. right .and. get_element(grid, x+1, y+1) /= element) then
                corners = corners + 1
            end if
        end do

        write(*,*) 'Price of connected region:', size, " x ", boundaries, " = ", size * boundaries
        write(*,*) 'Discount of connected region:', size, " x ", corners, " = ", size * corners
        price = size * corners
    end function fencing_price

end module grid_queue_operations

program main
    use grid_module
    use coord_queue_module
    use grid_queue_operations
    implicit none

    type(grid_type) :: my_grid
    type(grid_type) :: visited
    type(coord_queue_type) :: queue
    character(len=1) :: element, neighbor
    character(len=256) :: filename
    integer :: num_args
    logical :: success
    integer :: x, y
    integer :: size, boundaries
    integer :: price
    integer :: total_price

    num_args = command_argument_count()
    if (num_args /= 1) then
        write(*,*) 'Usage: ./program_name input_file.txt'
        stop
    end if

    call get_command_argument(1, filename)
    call init_grid(my_grid, trim(filename))

    call create_empty_grid(visited, my_grid%width, my_grid%height, ' ')

    call init_queue(queue)

    total_price = 0

    do y = 1, my_grid%height
        do x = 1, my_grid%width
            if (get_element(visited, x, y) == ' ') then
                price = fencing_price(queue, my_grid, visited, x, y)
                total_price = total_price + price
            end if
        end do
    end do

    write(*,*) 'Total fencing price:', total_price
end program main
