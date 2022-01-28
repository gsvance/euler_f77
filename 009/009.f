      program elr009
      implicit none
      
      integer goal
      parameter (goal=1000)
      
      integer s, sqr(goal)
      integer a, b, c
      
      do s = 1, goal
        sqr(s) = s * s
      enddo
      
      do 30 a = 1, goal/2
        do 20 b = a+1, goal/2
          c = b+1
   10     if (sqr(a) + sqr(b) .gt. sqr(c)) then
            c = c + 1
			goto 10
          elseif (sqr(a) + sqr(b) .eq. sqr(c)) then
            if (a + b + c .eq. goal) goto 90
          endif
   20   continue
   30 continue
      
   90 write (*,*) a * b * c
      
      stop
      end
