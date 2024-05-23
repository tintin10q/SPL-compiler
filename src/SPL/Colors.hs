module SPL.Colors where

reset = "\ESC[0m" 
black str = "\ESC[30m" ++ str ++ reset
red str = "\ESC[31m" ++ str ++ reset
green  str = "\ESC[32m" ++ str ++ reset
yellow str  = "\ESC[33m" ++ str ++ reset
blue str  = "\ESC[34m" ++ str ++ reset
magenta  str = "\ESC[35" ++ str ++ reset
cyan  str = "\ESC[36" ++ str ++ reset
white  str = "\ESC[37" ++ str ++ reset
mblack  str = "\ESC[90" ++ str ++ reset
mred  str = "\ESC[91" ++ str ++ reset
mgreen  str = "\ESC[92" ++ str ++ reset
myellow  str = "\ESC[93" ++ str ++ reset
mblue  str = "\ESC[94" ++ str ++ reset
mmagenta  str = "\ESC[95" ++ str ++ reset
mcyan str  ="\ESC[96" ++ str ++ reset
mwhite str  = "\ESC[97" ++ str ++ reset