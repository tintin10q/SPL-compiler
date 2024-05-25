module SPL.Colors where

reset = "\ESC[0m" 
resetDim     = "\ESC[22m" 
resetUnderlined  = "\ESC[24m" 
resetBlink    = "\ESC[25m" 
resetReverse  = "\ESC[27m" 
resetHidden    = "\ESC[28m"


resetBold  = "\ESC[21m"
bold     str  = "\ESC[1m" ++ str ++ resetBold
dim      str  = "\ESC[2m" ++ str ++ resetDim
underlined str = "\ESC[4m" ++ str ++ resetUnderlined
blink      str = "\ESC[5m" ++ str ++ resetBlink
reverse    str = "\ESC[7m" ++ str ++ resetReverse
hidden     str = "\ESC[8m" ++ str ++ resetHidden

 
black str = "\ESC[;30m" ++ str ++ reset
red str = "\ESC[;31m" ++ str ++ reset
green  str = "\ESC[;32m" ++ str ++ reset
yellow str  = "\ESC[;33m" ++ str ++ reset
blue str  = "\ESC[;34m" ++ str ++ reset
-- For some reason magenta does not work well
-- magenta  str = "\ESC[;35" ++ str ++ reset
-- cyan  str = "\ESC[;36" ++ str ++ reset
white  str = "\ESC[;37" ++ str ++ reset

blackBG str= "\ESC[0;40m" ++ str  ++ reset
redBG str= "\ESC[0;41m"++ str  ++ reset
greenBG str= "\ESC[0;42m" ++ str  ++ reset
yellowBG str= "\ESC[0;43m" ++ str   ++ reset
aquaBG str= "\ESC[0;44m" ++ str  ++ reset
magentaBG str= "\ESC[0;45m" ++ str  ++ reset
whiteBG str = "\ESC[0;47m" ++ str  ++ reset

lightblack  str = "\ESC[;90" ++ str ++ reset
lightred  str = "\ESC[;91" ++ str ++ reset
lightgreen  str = "\ESC[;92" ++ str ++ reset
lightyellow  str = "\ESC[;93" ++ str ++ reset
lightblue  str = "\ESC[;94" ++ str ++ reset
lightmagenta  str = "\ESC[;95" ++ str ++ reset
lightcyan str  ="\ESC[;96" ++ str ++ reset
lightwhite str  = "\ESC[;97" ++ str ++ reset
