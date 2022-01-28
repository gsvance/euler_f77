      program elr008
      implicit none
      
      integer adj, ndigit, nrow
      parameter (adj=13, ndigit=1000, nrow=20)
      
      character*80 line
      character*1 dig
      integer digit(ndigit)
      integer ncol, r, c, i, j
      integer*8 prod, grprod
      
      open (15, file='number.txt', status='old')
      
      ncol = ndigit / nrow
      
      do 20 r = 1, nrow
        read (15,*) line
        do 10 c = 1, ncol
          i = ncol * (r - 1) + c
          dig = line(c:c)
          read (dig,*) digit(i)
   10   continue
   20 continue
      
      close (15)
      
      grprod = 1
      i = 1
      
   30 if (i + adj - 1 .le. ndigit) then
        prod = 1
        do 40 j = 0, adj-1
          prod = prod * digit(i + j)
   40   continue
        if (prod .gt. grprod) grprod = prod
        i = i + 1
        goto 30
      endif
      
      write (*,*) grprod
      
      stop
      end
