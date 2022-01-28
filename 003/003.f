      program elr003
      implicit none
      
      integer*8 num
      parameter (num=600851475143_8)
c     ^ Appending "_8" to an integer literal makes it an 8-byte literal
      
      integer*8 div, rem
      
      div = 2
      rem = num
      
   10 if (rem .ne. 1) then
        if (mod(rem, div) .eq. 0) then
          rem = rem / div
        else
          div = div + 1
        endif
        goto 10
      endif
      
      write (*,*) div
      
      stop
      end
