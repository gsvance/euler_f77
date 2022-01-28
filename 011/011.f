      program elr011
      implicit none
      
      integer dimen, adj
      parameter (dimen=20, adj=4)
      
      character*10 fstr
      integer grid(dimen,dimen)
      integer x, y, a
      integer grprod, prod
      logical right, down, diag1, diag2
      
      open (15, file='grid.txt', status='old')
      
      write (fstr,'(A,I2,A)') '(',dimen,'I3)'
      
      do y = 1, dimen
        read (15,fstr) (grid(x,y), x=1,dimen)
      enddo
      
      close (15)
      
      grprod = 1
      
      do 200 x = 1, dimen
        do 100 y = 1, dimen
          
          right = x + adj - 1 .le. dimen
          down = y + adj - 1 .le. dimen
          diag1 = right .and. down
          diag2 = right .and. y - adj + 1 .ge. 1
          
          if (right) then
            prod = 1
            do 10 a = 0, adj-1
              prod = prod * grid(x+a,y)
   10       continue
            if (prod .gt. grprod) grprod = prod
          endif
          
          if (down) then
            prod = 1
            do 20 a = 0, adj-1
              prod = prod * grid(x,y+a)
   20       continue
            if (prod .gt. grprod) grprod = prod
          endif
          
          if (diag1) then
            prod = 1
            do 30 a = 0, adj-1
              prod = prod * grid(x+a,y+a)
   30       continue
            if (prod .gt. grprod) grprod = prod
          endif
          
          if (diag2) then
            prod = 1
            do 40 a = 0, adj-1
              prod = prod * grid(x+a,y-a)
   40       continue
            if (prod .gt. grprod) grprod = prod
          endif
          
  100   continue
  200 continue
      
      write (*,*) grprod
      
      stop
      end
