      program elr006
      implicit none
      
      integer first
      parameter (first=100)
      
      integer n, sumsqr, sqrsum, diff
      
      sumsqr = 0
      sqrsum = 0
      
      do 10 n = 1, first
        
        sumsqr = sumsqr + n * n
        sqrsum = sqrsum + n
        
   10 continue
      
      sqrsum = sqrsum * sqrsum
      diff = sqrsum - sumsqr
      
      write (*,*) diff
      
      stop
      end
