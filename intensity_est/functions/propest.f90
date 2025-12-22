! takes a kdt tree and returns the
! estimated weight 
! t = kdt tree; a = advice >= # nodes; e = 0 < epsilon < 1; 
! u = ceiling if true, otherwise floor
function propest(t, a, e, u)
    type(kdt), intent(in) :: t
    real(c_double), intent(in) :: a
    real(c_double), intent(in) :: e
    logical, intent(in) :: u
    integer :: s
    integer :: s_choose_2
    integer :: s_fact 
    integer :: s_incr
    integer :: p_fact
    integer :: p_incr

    ! s = sample size
    if (u) then                         ! m = ⌈(√24*a)/e⌉ + 1
        m = ceiling(sqrt(24*a)/e) + 1
    else                                ! m =  ⌊√24*a)/e⌋ + 1
        m = floor(sqrt(24*a)/e) + 1
    end if 

    ! calculate factorials
    s_fact = 1
    s_incr = s
    do while (s_incr > 1)
        s_fact = s_fact * s_incr
        s_incr = s_incr - 1
    end do 
    p_fact = 1
    p_incr = s - 2
    do while (p_incr > 1)
        p_fact = p_fact * p_incr
        p_incr = p_incr - 1
    end do

    ! calculate s_choose_2 = s_fact / (2 * p_fact)
    s_choose_2 = s_fact / (2 * p_fact)

    

end function propest