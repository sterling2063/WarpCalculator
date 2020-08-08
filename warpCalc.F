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
      integer y,mo,d,h,m,w
      real*8 wf,ts,dly,s,vc,c,dm,v,mly,th,td,tw,ty,tm,tmo
      real*8 dy,dd,dh,dmin,dw
      mly = 9.46073047258*10.d0**15.d0
      c = 299792458.d0
c  *******************************************************************
c  This section reads in from the user the distance, the desired warp
c  factor, and any spatial adjustments to be made.
c  *******************************************************************
      write(*,*)'Version 1.00'
      write(*,*)'Written by: Stephen McMillan'
      write(*,*)' '
      write(*,*)' '
      write(*,*)'Enter travel distance in ly'
      read(*,*)dly
1000  continue      
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
2004  if(ts == 0.d0) goto 2005
      write(*,160)ts,' seconds'
160   format(F8.3,A)
 
c  *******************************************************************
c  We now write a simple system to allow the user to acknowledge the
c  results before the program closes.
c  *******************************************************************
2005  write(*,*)' '
      write(*,*)'Calculation Complete. Press Enter to end program'
      read(*,*)       
      end program warpspeed
