import System.Environment
import System.Time
import System.Locale
import Data.Time
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Gdk.EventM

-----------------------------command line arguments-------------------------------------
parse str = case reads str::[(Int, String)] of
  [(number, "")] -> Just number
  _ -> Nothing 

parseD str = case reads str::[(Double, String)] of
  [(number, "")] -> Just number
  _ -> Nothing 

str2time str = do 
  let (h, m) = break ( \ch -> ch == ':') str
  let hour = parse h
  let min = parse (tail m)
  case (hour, min) of
    (Just h, Just m) -> Just (h * 60 + m)
    _ -> Nothing 


parseArgs [timeYellow, timeRed] = 
  let yellowArg = str2time timeYellow in
  let redArg = str2time timeRed in
  case (yellowArg, redArg) of
    (Just yellow, Just red) -> Just (yellow, red)
    _ -> Nothing
               
parseArgs [timeRed] = parseArgs [timeRed, timeRed]

parseArgs _ = Nothing

---------------------------------------------------------------------------------------

black = Color 0 0 0
green = Color 0 65000 0 
yellow = Color 65000 65000 0 
red = Color 65000 0 0

zoned2minutes (ZonedTime (LocalTime day (TimeOfDay h m s)) timeZone) = h * 60 + m

currentColor time yellowTime redTime = 
  let currMinutes = zoned2minutes time in
  if currMinutes >= redTime then red else if currMinutes >= yellowTime then yellow else green


-----------------------clock update callback-------------------------------------------
updateClock labl yellowTime redTime = do
  time <- getZonedTime
  widgetModifyFg labl StateNormal $ currentColor time yellowTime redTime
  labelSetText labl $ formatTime defaultTimeLocale "%H:%M" time
  return True

-----------------------calculate font size-------------------------------------------

getFontSizeRec layout width height size = do
    layoutSetAttributes layout [AttrSize 0 7 size]
    (Rectangle _ _ w1 h1, Rectangle _ _ w2 h2) <- layoutGetPixelExtents  layout    
    if ((width > (max w1 w2)) && (height > (max h1 h2))) 
        then getFontSizeRec layout width height (size + 10)
        else return (size - 10)

getFontSize labl width height = do
    cntxt <- widgetGetPangoContext labl
    layout <- layoutText cntxt "00:00"
    getFontSizeRec layout width height 50

    
---------------------------------------------------------------------------------------

main = do
  args <- getArgs
  case (parseArgs args) of
    Just (yellowTime, redTime) -> 
      showGui yellowTime redTime
    _ -> putStrLn "\nUsage:\n\nLateForWork xx:xx yy:yy  \n or \nLateForWork yy:yy \n\n\
\where xx:xx is the time when the clock turns yellow \nand yy:yy is the time when the clock turns red."

  where
    showGui yellowTime redTime = do
      initGUI
      window <- windowNew
      onDestroy window mainQuit

      window `on` keyPressEvent $ tryEvent $ do
        "Escape" <- eventKeyName
        liftIO mainQuit

      labl <- labelNew Nothing
      fd <- fontDescriptionNew

      containerAdd window labl

      windowFullscreen window
      widgetModifyBg window StateNormal black 

      screen <- screenGetDefault
      width <- case screen of
                   Just screen -> screenGetWidth screen
                   Nothing -> return 640
      height <- case screen of       
                   Just screen -> screenGetHeight screen
                   Nothing -> return 480

      cntxt <- widgetGetPangoContext labl
      layout <- layoutText cntxt "00:00"
      size <- getFontSize labl width height 

      fontDescriptionSetSize fd size
      widgetModifyFont labl $ Just fd

      timeoutAdd (updateClock labl yellowTime redTime >> return True) 1000

      widgetShowAll window
      mainGUI