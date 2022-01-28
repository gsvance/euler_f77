      program elr004
      implicit none
      
      integer mini, maxi
      parameter (mini=850, maxi=999)
      
      integer a, b, prod, best
      logical palind
      
      best = 0
      
      do 20 a = mini, maxi
        do 10 b = a, maxi
          prod = a * b
          if (prod .gt. best .and. palind(prod)) best = prod
   10   continue
   20 continue
      
      write (*,*) best
      
      stop
      end
      
c Return whether the number passed in is a palindrome
      logical function palind(num)
      integer num
      
      integer rem, dig(10)
      integer i, j
      
      rem = num
      i = 0
      
   30 if (rem .gt. 0) then
        i = i + 1
        dig(i) = mod(rem, 10)
        rem = rem / 10
        
        goto 30
      endif
      
      palind = .TRUE.
      j = 1
      
   40 if (palind .and. i - j .gt. 0) then
        palind = dig(i) .eq. dig(j)
        i = i - 1
        j = j + 1
        
        goto 40
      endif
      
      return
      end
