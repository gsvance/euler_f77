      program elr005
      implicit none
      
      integer maxdiv
      parameter (maxdiv=20)
      
      integer p, f, sml
      logical prime
      
      sml = 1
      
      do 20 p = 2, maxdiv
        
        if (prime(p)) then
          
          f = 1
          
   10     if (f .le. maxdiv) then
            f = f * p
            goto 10
          endif
          
          f = f / p
          sml = sml * f
          
        endif
        
   20 continue
      
      write (*,*) sml
      
      stop
      end
      
c Return whether p is a prime number
      logical function prime(p)
      integer p
      
      integer d
      
      d = 2
      
   30 if (mod(p, d) .eq. 0) then
        prime = .FALSE.
      else
        d = d + 1
        goto 30
      endif
      
      if (d .eq. p) prime = .TRUE.
      
      return
      end
