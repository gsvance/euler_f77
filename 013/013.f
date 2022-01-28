      program elr013
      implicit none
      
      integer nnumb, ndigit, first, big
      parameter (nnumb=100, ndigit=50, first=10, big=1000)
      
      character*80 line
      character*1 dig
      integer numb(nnumb,ndigit)
      integer n, p, pp, k
      integer*8 carry, tot(big), ans
      
      open (15, file='numbers.txt', status='old')
      
      do 20 n = 1, nnumb
        read (15,*) line
        do 10 p = 1, ndigit
          pp = ndigit - p + 1
          dig = line(pp:pp)
          read (dig,*) numb(n,p)
   10   continue
   20 continue
      
      close (15)
      
      carry = 0
      
      do 40 p = 1, ndigit
        tot(p) = carry
        do 30 n = 1, nnumb
          tot(p) = tot(p) + numb(n,p)
   30   continue
        carry = tot(p) / 10
        tot(p) = mod(tot(p), 10)
   40 continue
      
      p = ndigit + 1
      
   50 if (carry .gt. 0 .and. p .le. big) then
        tot(p) = mod(carry, 10)
        carry = carry / 10
        p = p + 1
        goto 50
      endif
      
      ans = 0
      
      do k = 1, first
        ans = ans * 10
        ans = ans + tot(p - k)
      enddo
      
      write (*,*) ans
      
      stop
      end
