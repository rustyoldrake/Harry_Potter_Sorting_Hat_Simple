import serial
ser = serial.Serial('/dev/tty.usbmodem1451', 9600)
# if you get an error "no module named serial' it means you need to go and do a 'sudo pip install pyserial'
