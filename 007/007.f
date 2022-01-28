      program elr007
      implicit none
      
      integer pos
      parameter (pos=10001)
      
      integer m, tal
      logical prime
      
      tal = 0
      m = 1
      
   10 if (tal .lt. pos) then
        
        m = m + 1
        if (prime(m)) tal = tal + 1
        
        goto 10
      endif
      
      write (*,*) m
      
      stop
      end
      
c Return whether num is a prime number
      logical function prime(num)
      integer num
      
      integer d
      
      d = 2
      
   20 if (d * d .le. num) then
        
        if (mod(num, d) .eq. 0) then
          prime = .FALSE.
          goto 30
        endif
        
        d = d + 1
        
        goto 20
      endif
      
      prime = .TRUE.
      
   30 return
      end
