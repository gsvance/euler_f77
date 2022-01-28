      program elr001
      implicit none
      
      integer limit
      parameter (limit=1000)
      
      integer n, total
      
      total = 0
      
      do n = 1, limit-1
        if (mod(n, 3) .eq. 0 .or. mod(n, 5) .eq. 0) total = total + n
      enddo
      
      write (*,*) total
      
      stop
      end
