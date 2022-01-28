      program elr012
      implicit none
      
      integer divs
      parameter (divs=500)
      
      integer n, tr, d, ndiv
      
      tr = 0
      n = 0
      
c     Main loop start
   10   n = n + 1
        tr = tr + n
        
        ndiv = 0
        d = 1
        
   20   if (d * d .le. tr) then
          if (mod(tr, d) .eq. 0) ndiv = ndiv + 2
          if (d * d .eq. tr) ndiv = ndiv - 1
          d = d + 1
          goto 20
        endif
        
        if (ndiv .gt. divs) then
          goto 90
        else
          goto 10
        endif
c     Main loop end
      
   90 write (*,*) tr
      
      stop
      end
