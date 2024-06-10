module SPL.Colors where

reset :: String
reset = "\ESC[0m" 
resetDim :: String
resetDim     = "\ESC[22m" 
resetUnderlined :: String
resetUnderlined  = "\ESC[24m" 
resetBlink :: String
resetBlink    = "\ESC[25m" 
resetReverse :: String
resetReverse  = "\ESC[27m" 
resetHidden :: String
resetHidden    = "\ESC[28m"


resetBold :: String
resetBold  = "\ESC[1m"
bold :: [Char] -> [Char]
dim :: [Char] -> [Char]
bold     str  = "\ESC[1m" ++ str ++ reset
dim      str  = "\ESC[2m" ++ str ++ reset
underlined :: [Char] -> [Char]
underlined str = "\ESC[4m" ++ str ++ reset
blink :: [Char] -> [Char]
blink      str = "\ESC[5m" ++ str ++ reset
reverse :: [Char] -> [Char]
reverse    str = "\ESC[7m" ++ str ++ reset
hidden :: [Char] -> [Char]
hidden     str = "\ESC[8m" ++ str ++ reset

black :: [Char] -> [Char]
black str = "\ESC[;30m" ++ str ++ reset
red :: [Char] -> [Char]
red str = "\ESC[;31m" ++ str ++ reset
green :: [Char] -> [Char]
green  str = "\ESC[;32m" ++ str ++ reset
yellow :: [Char] -> [Char]
yellow str  = "\ESC[;33m" ++ str ++ reset
blue :: [Char] -> [Char]
blue str  = "\ESC[;34m" ++ str ++ reset
-- For some reason magenta does not work well
-- magenta  str = "\ESC[;35" ++ str ++ reset
-- cyan  str = "\ESC[;36" ++ str ++ reset
white :: [Char] -> [Char]
white  str = "\ESC[;37" ++ str ++ reset

blackBG :: [Char] -> [Char]
blackBG str= "\ESC[0;40m" ++ str  ++ reset
redBG :: [Char] -> [Char]
redBG str= "\ESC[0;41m"++ str  ++ reset
greenBG :: [Char] -> [Char]
greenBG str= "\ESC[0;42m" ++ str  ++ reset
yellowBG :: [Char] -> [Char]
yellowBG str= "\ESC[0;43m" ++ str   ++ reset
aquaBG :: [Char] -> [Char]
aquaBG str= "\ESC[0;44m" ++ str  ++ reset
magentaBG :: [Char] -> [Char]
magentaBG str= "\ESC[0;45m" ++ str  ++ reset
whiteBG :: [Char] -> [Char]
whiteBG str = "\ESC[0;47m" ++ str  ++ reset

lightblack :: [Char] -> [Char]
lightblack  str = "\ESC[;90" ++ str ++ reset
lightred :: [Char] -> [Char]
lightred  str = "\ESC[;91" ++ str ++ reset
lightgreen :: [Char] -> [Char]
lightgreen  str = "\ESC[;92" ++ str ++ reset
lightyellow :: [Char] -> [Char]
lightyellow  str = "\ESC[;93" ++ str ++ reset
lightblue :: [Char] -> [Char]
lightblue  str = "\ESC[;94" ++ str ++ reset
lightmagenta :: [Char] -> [Char]
lightmagenta  str = "\ESC[;95" ++ str ++ reset
lightcyan :: [Char] -> [Char]
lightcyan str  ="\ESC[;96" ++ str ++ reset
lightwhite :: [Char] -> [Char]
lightwhite str  = "\ESC[;97" ++ str ++ reset
