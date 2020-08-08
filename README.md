# WarpCalculator
I have written a warp speed travel time calculator in fortran. The code was compiled using gfortran -o warpCalc warpCalc.F

The user can input a travel distance in light years, and a warp factor from 0 to 10 (TNG warp units). The code will then calculate the time it takes to arrive at your destination.

Additionally, there is a user defined input to allow for scaling of different spatial factors that might either speed up or slow down standard warp travel. In version 1.00, this
defaults to 1. The user may input a value slightly higher than 1 to represent a boost to speed, or a value slightly lower to represent a slower speed.
