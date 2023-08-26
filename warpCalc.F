c  *******************************************************************
c  Filename: warpCalc.F
c  Stephen McMillan 08/08/2020
c  version 1.00
c
c  This program was written to calculate the average possible warp 
c  travel time. The user can input a known distance, and a desired
c  warp factor, and the program will output a travel time. To make
c  the program more robust, a multiplicative factor is included
c  to allow for different spatial domains that might speed up or
c  slow down the journey.
c  
c  compiled with gfortran -o warpCalc warpCalc.F
c  *******************************************************************
      program warpspeed
      implicit none
      integer y,mo,d,h,m,w,choice,ch2
      real*8 wf,ts,dly,s,vc,c,dm,v,mly,th,td,tw,ty,tm,tmo
      real*8 dy,dd,dh,dmin,dw
      real*8 tty,ttw,ttd,tth,ttm,tts,ttot
      real*8 dv,dwf
      mly = 9.46073047258*10.d0**15.d0
      c = 299792458.d0
c  *******************************************************************
c  This section reads in from the user the distance, the desired warp
c  factor, and any spatial adjustments to be made.
c  *******************************************************************
      write(*,*)'Version 2.00'
      write(*,*)'Written by: Stephen McMillan'
      write(*,*)' '
5     continue
      write(*,*)' '
      write(*,*)'Type 1 to find travel time'
      write(*,*)'Type 2 to find a warp factor'
      write(*,*)'Type 3 to find distance traveled'
10    continue
      read(*,*)choice
      if(choice == 1) then
            goto 1000
      elseif(choice == 2) then
            goto 3000
      elseif(choice == 3) then
            goto 4000
      else
            write(*,*)'That is not an appropriate choice, try again'
            goto 10
      endif
      
1000  continue
      write(*,*)' '
      write(*,*)'Enter travel distance in ly'
      read(*,*)dly
      write(*,*)'Enter desired warp factor (24th Cent. Units)'
      read(*,*)wf
      if(wf >= 10.d0) then
            write(*,*)'Warp 10 and up is inadvisable, please try again'
            goto 1000
      endif
      write(*,*)'Enter spatial factor (>1 faster, 1 norm, <1 slow)'
      read(*,*)s

c  *******************************************************************
c  With the test variables written in, we now calculate the velocity
c  from the given warp factor. This function was determined by me
c  several years ago, and is a perfect representation for warp
c  factors <= 9. For warp factors >9, it matches very well up to
c  wf = 9.9, then begins to diverge from the known solution.
c  *******************************************************************
      if(wf > 0.d0 .AND. wf < 9.d0) then 
            vc = s*wf**(10.d0/3.d0)
      elseif(wf >= 9.d0 .AND. wf < 10.d0) then
            vc = s*wf**((10.d0/3.d0)-0.1d0*log10(10.d0-wf))
      endif
c  *******************************************************************
c  We now calculate the time in seconds for travel, and convert it to
c  time in years, weeks, days, hours and minutes for the users perusal
c  *******************************************************************
      ty = dly/vc
      tw = ty*52.d0
      td = ty*365.d0
      th = td*24.d0
      tm = th*60.d0
      ts = tm*60.d0
c  *******************************************************************
c  In this section, we code the natural time outputs
c  *******************************************************************
      y = floor(ty)
      dy = ty - dfloat(y)
      tw = dy*52.1429d0
      w = floor(tw)
      dw = tw - dfloat(w)
      td = dw*7.d0
      d = floor(td)
      dd = td - dfloat(d)
      th = dd*24.d0
      h = floor(th)
      dh = th - dfloat(h)
      tm = dh*60.d0
      m = floor(tm)
      dmin = tm - dfloat(m)
      ts = dmin*60.d0
!       ts = nint(ts * 1000.d0) * 0.001d0
c  *******************************************************************
c  We now print the results for the user to review.
c  *******************************************************************
      write(*,*)'Your travel time is:'
      if(y == 0.d0) goto 2000
      write(*,100)y,' years'
100   format(I8.0,A)
2000  if(w == 0.d0) goto 2001
      write(*,110)w,' weeks'
110   format(I8.0,A)
2001  if(d == 0.d0) goto 2002
      write(*,120)d,' days'
120   format(I8.0,A)
2002  if(h == 0.d0) goto 2003
      write(*,130)h,' hours'
130   format(I8.0,A)
2003  if(m == 0.d0) goto 2004
      write(*,150)m,' minutes'
150   format(I8.0,A)
2004  if(ts == 0.d0) goto 5000
      write(*,160)ts,' seconds'
160   format(F8.3,A)
      goto 5000
c  *******************************************************************
c  This block will calculate the needed warp factor to travel a
c  distance in a certain amount of time.
c  *******************************************************************
c  *******************************************************************
c  We now gather input for the needed travel time from one point to
c  another.
c  *******************************************************************
3000  continue
      write(*,*)''
      write(*,*)'Enter travel distance in ly'
      read(*,*)dly
      write(*,*)'Enter year component of travel time'
      read(*,*)tty
      write(*,*)'Enter weeks component of travel time'
      read(*,*)ttw
      write(*,*)'Enter days component of travel time'
      read(*,*)ttd
      write(*,*)'Enter hours component of travel time'
      read(*,*)tth
      write(*,*)'Enter minutes component of travel time'
      read(*,*)ttm
      write(*,*)'Enter seconds component of travel time'
      read(*,*)tts
      write(*,*)'Enter spatial factor (>1 faster, 1 norm, <1 slow)'
      read(*,*)s
      write(*,*)'Now calculating needed Warp Factor'
c  *******************************************************************
c  We now convert the time entered into years, the velocity
c  in terms of c
c  *******************************************************************      
      ttot = tty + ttw/52.d0 + ttd/365.d0 + tth/8760.d0
      ttot = ttot + ttm/(525600.d0) + tts/(86400.d0*365.d0)
      dv = dly/ttot
      if(dv > 0.d0 .AND. dv <= 1516.38d0) then
          dwf = (1/s)*dv**(3.d0/10.d0)
      elseif(dv > 1516.d0) then
          write(*,*)''
          write(*,*)'Warp Factors above 9 are not sustainable'
          write(*,*)'More time is needed to reach your destination'
          goto 5000
      endif
      write(*,*)'Needed warp factor is',dwf
      goto 5000
c  *******************************************************************
c  Here we do the calculations for the third type of user input,
c  finding a distance from a known warp factor, and a known travel
c  time.
c  *******************************************************************
4000  continue
c  *******************************************************************
c  We gather the user's input of how long a travel time will take.
c  *******************************************************************
      write(*,*)''
      write(*,*)'Enter year component of travel time'
      read(*,*)tty
      write(*,*)'Enter weeks component of travel time'
      read(*,*)ttw
      write(*,*)'Enter days component of travel time'
      read(*,*)ttd
      write(*,*)'Enter hours component of travel time'
      read(*,*)tth
      write(*,*)'Enter minutes component of travel time'
      read(*,*)ttm
      write(*,*)'Enter seconds component of travel time'
      read(*,*)tts
      ttot = tty + ttw/52.d0 + ttd/365.d0 + tth/8760.d0
      ttot = ttot + ttm/(525600.d0) + tts/(86400.d0*365.d0)
c  *******************************************************************
c  We gather the user's input of a known warp factor.
c  *******************************************************************
      write(*,*)'Enter known warp factor (24th Cent. Units)'
      read(*,*)wf
      write(*,*)'Enter spatial factor (>1 faster, 1 norm, <1 slow)'
      read(*,*)s
c  *******************************************************************
c  With the test variables written in, we now calculate the velocity
c  from the given warp factor. This function was determined by me
c  several years ago, and is a perfect representation for warp
c  factors <= 9. For warp factors >9, it matches very well up to
c  wf = 9.9, then begins to diverge from the known solution.
c  *******************************************************************
      if(wf > 0.d0 .AND. wf < 9.d0) then 
            vc = s*wf**(10.d0/3.d0)
      elseif(wf >= 9.d0 .AND. wf < 10.d0) then
            vc = s*wf**((10.d0/3.d0)-0.1d0*log10(10.d0-wf))
      endif
c  *******************************************************************
c  We calculate the distance in light years.
c  *******************************************************************
      dly = vc*ttot
      write(*,*)''
      write(*,*)'Your distance traveled is ',dly
      goto 5000
c  *******************************************************************
c  We now write a simple system to allow the user to acknowledge the
c  results before the program closes.
c  *******************************************************************
5000  write(*,*)' '
      write(*,*)'Calculations Complete.'
      write(*,*)'Press 1 to perform another calculation'
      write(*,*)'Or press 2 to end program'
      read(*,*)ch2
      write(*,*)''
      write(*,*)''
      write(*,*)''
      write(*,*)''
      if(ch2 == 1) goto 5
      
      end program warpspeed
