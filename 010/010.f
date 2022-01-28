      program elr010
      implicit none
      
      integer below
      parameter (below=2*10**6)
      
      logical prime(below-1)
      integer n, m
      integer*8 psum
      
      prime(1) = .false.
      do n = 2, below-1
        prime(n) = .true.
      enddo
      
      psum = 0
      n = 2
      
   10 if (n .lt. below) then
        if (prime(n)) then
          psum = psum + n
          m = 2 * n
   20     if (m .lt. below) then
            prime(m) = .false.
            m = m + n
            goto 20
          endif
        endif
        n = n + 1
        goto 10
      endif
      
      write (*,*) psum
      
      stop
      end
