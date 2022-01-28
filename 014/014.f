      program elr014
      implicit none
      
      integer under, big
      parameter (under=10**6, big=10**9)
      
      integer*8 cltz
      
      integer record(big)
      integer long, n, t
      integer*8 m
      
      record(1) = 1
      do n = 2, big
        record(n) = -1
      enddo
      
      long = 1
      
      do 90 n = 2, under-1
        
        m = n
        t = 0
        
   10   if (m .gt. big) then
          
          m = cltz(m)
          t = t + 1
          goto 10
          
        elseif (record(m) .eq. -1) then
          
          m = cltz(m)
          t = t + 1
          goto 10
          
        endif
        
        record(n) = record(m) + t
        
        if (record(n) .gt. long) long = n
        
   90 continue
      
      write (*,*) long
      
   99 stop
      end

c     Function for simple Collatz iteration
      integer*8 function cltz(nn)
      integer*8 nn
      
      integer rem
      
      rem = mod(nn, 2)
      
      if (rem .eq. 0) then
        cltz = nn / 2
      else
        cltz = 3 * nn + 1
      endif
      
      return
      end
