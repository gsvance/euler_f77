      program elr002
      implicit none
      
      integer limit
      parameter (limit=4*10**6)
      
      integer fn, fn1, fn2, total
      
      total = 0
      
      fn2 = 1
      fn1 = 1
      fn = -1
      
   10 if (fn .le. limit) then
        fn = fn1 + fn2
        
        if (mod(fn, 2) .eq. 0) total = total + fn
        
        fn2 = fn1
        fn1 = fn
        
        goto 10
      endif
      
      write (*,*) total
      
      stop
      end
