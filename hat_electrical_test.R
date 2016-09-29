######################################################
### HARRY POTTER SIMPLIFIED SORTING HAT
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### HAT Electrical test
##################

library(rPython)

setwd("/Users/ryan/Documents/Project_Harry_Potter") # Set working Directory
getwd()

### Test for Eyes left , straight and right
python.load("initialize.py", get.exception = TRUE)
python.exec("ser.write('1')") # ascii 49 left
python.exec("ser.write('2')") # ascii 50 straight
python.exec("ser.write('3')") # ascii 51 right
python.exec("ser.write('2')") # ascii 50 straight


python.exec("ser.write('r')") # ascii RED 114
python.exec("ser.write('g')") # ascii GREEN 103
python.exec("ser.write('b')") # ascii BLUE 98
python.exec("ser.write('w')") # ascii WHITE 119
python.exec("ser.write('y')") # ascii YELLOW 121

python.exec("ser.write(' ')") # ascii SPACE 32 CLEARS ALL

