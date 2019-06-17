import System.IO
import System.Environment
import Grammar
import SLR1tables

форматВывода :: Форматирование
форматВывода =
   Форматирование {
      форматСтраницы       = ФорматА0,
      ячеекДЕЙСТВИЕвСтроке = 20,
      ячеекПЕРЕХОДвСтроке  = 20,
      пунктовВСтроке       = 10,
      правилВСтроке        = 3
   }

main :: IO ()
main = do
   args <- getArgs
   if null args then
      putStrLn "Не задано имя результирующего файла."
   else
      let name = head args in
      withFile name WriteMode (\handle -> do
         hPutStrLn handle $ результат  грамматикаМУР128 Programm форматВывода)
