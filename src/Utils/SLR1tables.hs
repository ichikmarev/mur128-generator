{-# LANGUAGE FlexibleInstances #-} 
 
module SLR1tables ( 
   ФорматСтраницы(..),
   
   Форматирование(..),
   
   результат 
) where

import qualified Data.Set as Sets   
import qualified Data.Map as Maps
import qualified Data.List as Lists
import Data.Maybe 
import Rules

data ФорматСтраницы = 
   ФорматА0 | ФорматА1 | ФорматА2 | ФорматА3 | ФорматА4
   deriving (Eq, Ord, Enum, Bounded, Read)
   
instance Show ФорматСтраницы where
   show ФорматА0 = "a0"
   show ФорматА1 = "a1"
   show ФорматА2 = "a2"
   show ФорматА3 = "a3"
   show _        = "" 

data Форматирование = 
   Форматирование {
      форматСтраницы       :: ФорматСтраницы,
      ячеекДЕЙСТВИЕвСтроке :: Int,
      ячеекПЕРЕХОДвСтроке  :: Int,
      пунктовВСтроке       :: Int,
      правилВСтроке        :: Int 
   }
   deriving (Eq,Ord,Show,Read)
   
data LR0Пункт a b = Пункт {леваяЧасть :: a, доТочки, послеТочки :: [Символ a b]}
   deriving(Eq,Ord)
    
type Набор a b = Sets.Set (LR0Пункт a b) -- Набор LR(0)--пунктов

type ТаблНаборов a b = Maps.Map (Набор a b) Int

data ПЕРЕХОД a b = 
   ПЕРЕХОД {откуда, куда :: Int, симвПерехода :: Символ a b}  
   deriving(Eq,Ord)

type Переходы a b = Sets.Set (ПЕРЕХОД a b)

data ИндексыИНаборы a b = 
   ИндексыИНаборы {последнийИндекс :: Int,
                   индексВставленного :: Int,
                   табл :: ТаблНаборов a b}
   deriving(Eq,Ord)

data ДействиеАнализатора = Перенос Int | Свёртка Int | Допуск 
   deriving (Eq, Ord)
    
type ТаблицаДЕЙСТВИЕ a b = Maps.Map (Int,Maybe b) [ДействиеАнализатора] 

type ТаблицаПЕРЕХОД a = Maps.Map (Int,a) Int

data НаборСНомером a b = НумерованныйНабор (Набор (Maybe a) b,Int)

-- Операции над отдельными пунктами.
пунктИзПравила ::  Правило a b -> LR0Пункт a b
пунктИзПравила r = Пункт {леваяЧасть = понятие r,
                          доТочки = [],
                          послеТочки = тело r}

точкуВправо :: LR0Пункт a b -> LR0Пункт a b  
точкуВправо пункт = 
   if null после then
      пункт
   else
      Пункт {леваяЧасть = леваяЧасть пункт, 
             доТочки = (доТочки пункт) ++ [head после],
             послеТочки = tail после}
   where 
      после = послеТочки пункт
        
-- Вычисление функции 'замыкание'.
замыкание :: (Ord a,Ord b) => 
             Правила a b -> 
             Набор a b -> 
             Набор a b 
замыкание правила пункты  = 
   if новПункты == пункты then 
      пункты 
   else 
      замыкание правила новПункты 
   where 
      новПункты = Sets.foldl' (добавитьПравила правила) пункты пункты 
        
добавитьПравила :: (Ord a, Ord b) => 
                   Правила a b -> 
                   Набор a b -> 
                   LR0Пункт a b -> 
                   Набор a b
добавитьПравила прав аккум текПункт =
   if null после then
      аккум
   else 
      case head после of
         Терминал _ -> аккум
         Нетерминал nt -> Sets.union аккум $ Sets.map (пунктИзПравила) $
                          Sets.filter (\x -> (понятие x) == nt) прав
   where 
      после = послеТочки $ текПункт 
  
-- Вычисление функции 'переход'.
переход :: (Ord a,Ord b) => 
           Правила a b -> 
           Набор a b ->  
           Символ a b ->  
           Набор a b 
переход правила пункты символ = 
   замыкание правила $ Sets.map точкуВправо $ 
   Sets.filter (\x -> нужныйЛиСимволПослеТочки x символ) пункты

нужныйЛиСимволПослеТочки :: (Eq a,Eq b) => 
                            LR0Пункт a b -> 
                            Символ a b -> 
                            Bool
нужныйЛиСимволПослеТочки пункт символ = 
   (not . null $ после) && ((head после) == символ)
   where 
      после = послеТочки пункт

-- А теперь вычисление всех переходов.
добавНаборПунктов :: (Ord a, Ord b) =>  
                     ИндексыИНаборы a b -> 
                     Набор a b -> 
                     ИндексыИНаборы a b
добавНаборПунктов i пункты =
   let (x,y) = (Maps.lookup пункты текущТабл,послИнд+1) in
   case x of
      Nothing -> ИндексыИНаборы {
                    табл = Maps.insert пункты y текущТабл,
                    последнийИндекс = y,
                    индексВставленного = y}
      Just k  -> ИндексыИНаборы {
                    табл = текущТабл,
                    последнийИндекс = послИнд,
                    индексВставленного = k}
   where
      послИнд = последнийИндекс i
      текущТабл = табл i

сворачивающаяФункция :: (Ord a, Ord b) => 
                        Правила a b -> 
                        Набор a b -> 
                        (ИндексыИНаборы a b, Переходы a b) -> 
                        Символ a b -> 
                        (ИндексыИНаборы a b, Переходы a b)
сворачивающаяФункция правила пункты инп@(ин,таблПереходов) символ =
   if Sets.null новПункты then
      инп  
   else
      (новИндИНаборы,
       Sets.insert (ПЕРЕХОД{
                           откуда = fromJust $ Maps.lookup пункты $ табл ин,
                           куда = индексВставленного новИндИНаборы,
                           симвПерехода = символ}) таблПереходов) 
   where  
      новПункты = переход правила пункты символ
      новИндИНаборы = добавНаборПунктов ин новПункты

добавПереходыНабора :: (Ord a, Ord b) => 
                       Правила a b -> 
                       (ИндексыИНаборы a b, Переходы a b) -> 
                       Набор a b -> 
                       (ИндексыИНаборы a b, Переходы a b) 
добавПереходыНабора правила инп пункты =
   Lists.foldl' (сворачивающаяФункция правила пункты) инп первСимволы
   where 
      первСимволы = 
         Lists.nub $ Sets.foldr' (\x a -> (myhead . послеТочки $ x) ++ a) [] пункты

myhead :: [a] -> [a]
myhead [] = []
myhead (x:_) = [x]

mytail :: [a] -> [a]
mytail [] = []
mytail (_:xs) = xs

всеПереходы' :: (Ord a, Ord b) => 
                Правила a b -> 
                (ИндексыИНаборы a b, Переходы a b) -> 
                ТаблНаборов a b -> 
                (ИндексыИНаборы a b, Переходы a b) 
всеПереходы' правила индНабПерех@(индНаб,_) таблица =
   if последнийИндекс индНаб == последнийИндекс новИндНаб then
      (новИндНаб, новПерех)
   else
      всеПереходы' правила новИндНабПерех $ Maps.difference новТабл $ табл индНаб
   where
      новИндНабПерех@(новИндНаб, новПерех) = 
         Maps.foldlWithKey' (
            \акк пункт _ -> добавПереходыНабора правила акк пункт
         ) индНабПерех $ таблица
      новТабл = табл новИндНаб
    
всеПереходы :: (Ord a, Ord b) => 
               Правила a b ->
               Правило a b ->
               ([(Набор a b,Int)],Переходы a b)
всеПереходы правила начальноеПравило =
   (Lists.sortBy (\ki' ki'' -> (snd ki') `compare` (snd ki'')) . 
    Maps.toList . табл $ индН, 
    пер)
   where
      начальныйНабор = 
         замыкание правила $ Sets.singleton . пунктИзПравила $ начальноеПравило
      начальнаяТаблицаНаборов = Maps.singleton начальныйНабор 0
      начИндНаборы = ИндексыИНаборы {последнийИндекс = 0,
                                     индексВставленного =0,
                                     табл = начальнаяТаблицаНаборов}
      (индН, пер) = 
         всеПереходы' правила (начИндНаборы, Sets.empty) начальнаяТаблицаНаборов
   
-- Вычисление множеств FIRST и FOLLOW
изТерминала :: Символ a b -> b
изТерминала (Терминал т) = т
изТерминала _            = error "Ожидается терминальный символ."

изНетерминала :: Символ a b -> a
изНетерминала (Нетерминал н) = н
изНетерминала _            = error "Ожидается нетерминальный символ."

терминалЛи :: Символ a b -> Bool
терминалЛи (Терминал _) = True
терминалЛи _            = False 

нетерминалЛи :: Символ a b -> Bool
нетерминалЛи (Нетерминал _) = True
нетерминалЛи _               = False 

множестваFIRSTиFOLLOW :: (Ord a, Ord b) =>  
                         Правила a b ->
                         a ->
                         (Maps.Map a (Sets.Set b),Maps.Map a (Sets.Set (Maybe b)))
множестваFIRSTиFOLLOW правила начальныйНетерминал  = 
   (перв, множестваFOLLOW правила начальныеFOLLOW перв) 
   where
      нетерминалы = 
         Lists.nub . Sets.foldl' (\zs r -> (понятие r):zs) [] $ правила
      начальныеFIRST = Maps.fromList . Lists.foldl' 
         (\xs nt -> (nt, Sets.fromList . Lists.nub . Sets.foldl' 
            (\ys p -> (myhead . тело $ p) ++ ys) [] .
            Sets.filter (\q -> (понятие q) == nt) $ правила 
                   ):xs
         ) [] $ нетерминалы
      нетерминалы' = Lists.delete начальныйНетерминал нетерминалы 
      начальныеFOLLOW = 
         Maps.insert начальныйНетерминал (Sets.singleton Nothing) $
         Lists.foldl' (\ma n -> Maps.insert n Sets.empty ma) 
         Maps.empty нетерминалы'
      перв = Maps.map (\s -> Sets.map изТерминала s) $ множестваFIRST правила 
             начальныеFIRST                         
            
множестваFIRST :: (Ord a, Ord b) => 
                  Правила a b ->
                  Maps.Map a (Sets.Set (Символ a b)) ->
                  Maps.Map a (Sets.Set (Символ a b))
множестваFIRST прав текущиеFIRST =
   if текущиеFIRST == следующиеFIRST then
      Maps.map (\s -> Sets.filter терминалЛи s) текущиеFIRST
   else
      множестваFIRST прав следующиеFIRST
   where
      следующиеFIRST = 
         Maps.mapWithKey (
            \_ s -> 
               Sets.foldl' (
                  \_ y -> case y of
                               Терминал _ -> s
                               Нетерминал nt' ->  
                                  Sets.union (Sets.delete y s) $ 
                                  fromJust . Maps.lookup nt' $
                                  текущиеFIRST
               ) s s
         ) текущиеFIRST
 
множестваFOLLOW :: (Ord a, Ord b) => 
                   Правила a b ->
                   Maps.Map a (Sets.Set (Maybe b)) ->
                   Maps.Map a (Sets.Set b) ->
                   Maps.Map a (Sets.Set (Maybe b))
множестваFOLLOW правила текущиеFOLLOW всеFIRST = 
   if текущиеFOLLOW == следующиеFOLLOW then
      текущиеFOLLOW
   else
      множестваFOLLOW правила следующиеFOLLOW всеFIRST
   where
      следующиеFOLLOW = 
         Maps.mapWithKey (
            \nt s ->
               Sets.foldl' (\аккум r' -> 
                  let y = тело r' in
                  fst . Lists.foldl' (
                     \(s', xs) c ->                                    
                        let z = mytail xs in
                        (case c of 
                           Нетерминал _ -> 
                              case z of
                                 [] -> Sets.union s'. 
                                       fromJust .
                                       Maps.lookup (понятие r') 
                                       $ текущиеFOLLOW
                                 (z':_) -> case z' of
                                               Нетерминал n' -> 
                                                   Sets.union s' .
                                                   Sets.map (\x -> Just x) .
                                                   fromJust .
                                                   Maps.lookup n' $
                                                   всеFIRST
                                               Терминал t'   -> 
                                                   Sets.insert (Just t') s'
                           _           -> s'                                         
                        ,z)) 
                  (аккум, y) $ y
               ) s . 
               Sets.filter (\r -> (Нетерминал nt) `elem` (тело r)) $ правила
         ) текущиеFOLLOW

-- Вычисление таблиц ДЕЙСТВИЕ и ПЕРЕХОД.
таблицыДЕЙСТВИЕиПЕРЕХОД :: (Ord a, Ord b) => 
                           Правила a b ->
                           a ->
                           (ТаблицаДЕЙСТВИЕ a b, 
                            ТаблицаПЕРЕХОД a, 
                            Правила (Maybe a) b,
                            [(Набор (Maybe a) b,Int)])
таблицыДЕЙСТВИЕиПЕРЕХОД  правила начНетерминал = 
   (графаДЕЙСТВИЕ, графаПЕРЕХОД, расшНаборПравил, наборы)
   where
      (_, множFOLLOW) = множестваFIRSTиFOLLOW правила начНетерминал
      расшНаборПравил = 
         Sets.insert новоеНачПравило $
         Sets.map (
            \r ->
               let {xs = тело r; y = понятие r} in
               Правило {понятие = Just y,
                        тело = map (\c ->
                                        case c of
                                            Нетерминал n -> 
                                                Нетерминал $ Just  n
                                            Терминал t   -> 
                                                Терминал t  
                                   ) xs}
         ) правила
      новоеНачПравило = Правило {понятие = Nothing, 
                                 тело = [Нетерминал $ Just начНетерминал]}
      (наборы, перех) = всеПереходы расшНаборПравил новоеНачПравило
      (графаПЕРЕХОД, графаДЕЙСТВИЕ') = 
         Sets.foldl' (
            \(m',m'') p ->
               let {откудаПереход = откуда p;
                    кудаПереход   = куда p;
                    симв          = симвПерехода p} in
               if нетерминалЛи симв then
                  (Maps.insert (откудаПереход, fromJust . 
                                изНетерминала $ симв) кудаПереход m',
                   m'')
               else
                  (m',
                   Maps.insert (откудаПереход, Just $ изТерминала симв) 
                   [Перенос кудаПереход] m''
                  )
         ) (Maps.empty,Maps.empty) перех
      номерНабораДляДопуска = 
          snd . head . filter (
            \n -> 
               Sets.member
                  (Пункт {леваяЧасть = Nothing,
                          доТочки    = 
                              [Нетерминал . Just $ начНетерминал],
                          послеТочки = []})
                  $ fst n)
               $ наборы
      графаДЕЙСТВИЕ'' = 
          Maps.insert (номерНабораДляДопуска, Nothing) [Допуск] графаДЕЙСТВИЕ'       
      графаДЕЙСТВИЕ = 
         Lists.foldl' (
            \m (n',n'') -> 
               Sets.foldl' (
                  \m' p ->
                     case p of
                        Пункт {леваяЧасть = Just x, 
                               доТочки    = y, 
                               послеТочки = []} -> 
                                                   let 
                                                      {w = fromJust .
                                                           Sets.lookupIndex 
                                                           ((Just x) --> y) $
                                                           расшНаборПравил;
                                                       f = fromJust .
                                                           Maps.lookup x $
                                                           множFOLLOW }
                                                   in
                                                   Sets.foldl' (\m'' c -> 
                                                       Maps.insertWith (++)
                                                       (n'',c) [Свёртка w] m''
                                                   )
                                                   m' f                                                                      
                        _ -> m' 
               ) m n'
         ) графаДЕЙСТВИЕ'' наборы

-- Теперь экземпляры классов типов, используемые функцией, 
-- преобразующей результаты в строку.
class Показать a where
   показать :: a -> String
   
class Показать' a where
   показать' :: a -> Int -> String
   
instance (Show a) => Показать' (ТаблицаПЕРЕХОД a) where
   показать' т n = 
      " \nГрафа ПЕРЕХОД:\\\\\n" ++ 
      (init . init . init . fst . Maps.foldrWithKey' (
         \k v (str,nom) -> 
            (выводПерехода k v ++ ", " ++ 
             (if (nom `mod` n) == 0 then "\\\\\n" else "") ++ str, 
             nom + 1)
      ) ("",0) $ т)
      where 
         выводПерехода (откуда, нетерм) куда = 
            "ПЕРЕХОД[" ++ show откуда ++ ", \\textcolor{Green}{\\textit{" ++ 
            show нетерм ++ "}}]=" ++ show куда

instance (Show b) => Показать' (ТаблицаДЕЙСТВИЕ a b) where
   показать' т n = 
      " \nГрафа ДЕЙСТВИЕ:\\\\\n" ++ (init . init . init . fst . Maps.foldrWithKey' (
         \k v (str,nom) -> 
            (выводСпискаДействий k v ++ ", " ++ 
             (if (nom `mod` n) == 0 then "\\\\\n" else "") ++ str, 
             nom + 1)
      ) ("",0) $ т)
   
выводСпискаДействий :: (Show b) => 
                       (Int,Maybe b) ->
                       [ДействиеАнализатора] ->
                       String
выводСпискаДействий (n, c) ds = 
   "ДЕЙСТВИЕ[" ++ show n ++ ", " ++ выводТерминалаИлиМаркераКонца c ++ "]= \\{" ++
   (init . tail . show $ ds) ++ "\\}"
   where
      выводТерминалаИлиМаркераКонца (Just x) = show x
      выводТерминалаИлиМаркераКонца Nothing  = "$\\blacklozenge$"      

instance (Show a, Show b) => Показать (Символ (Maybe a) b) where 
   показать c =
      case c of
         Нетерминал (Just c)  -> "\\textcolor{Green}{\\textit{" ++ show c ++ "}} "
         Терминал t           -> show t ++ " "
         _                    -> ""            
    
instance (Show a, Show b) => Показать ([Символ (Maybe a) b]) where 
   показать = Lists.foldr (\x acc -> показать x ++ acc) ""

instance (Show a, Show b) => Показать (Правило (Maybe a) b) where
   показать Правило {понятие = p, тело = xs} = 
      показатьНетермРасшГрамматики p ++ "\\textcolor{Green}{$\\to$}" ++ 
      показать xs        
   
показатьНетермРасшГрамматики :: (Show a) =>
                                Maybe a ->
                                String
показатьНетермРасшГрамматики Nothing  = "\\textcolor{Green}{$\\mho$}"
показатьНетермРасшГрамматики (Just c) = 
   "\\textcolor{Green}{\\textit{" ++ show c ++ "}} "       
                
instance (Show a, Show b) => Показать' (НаборСНомером a b) where 
   показать' (НумерованныйНабор (n,i)) m = 
      "$I_{" ++ show i ++ "}$=\\{" ++ 
      (init . init . init . init . fst . 
       Sets.foldr' (
         \p (str,nom) -> 
            (show p ++ (if (nom `mod` m) == 0 then ",\\\\\n" else ", ") ++ str
            , nom+1)
            ) ("",0) $ n
      ) ++ "\\}\n"        
 
 -- (str,nom) -> 
            -- (выводСпискаДействий k v ++ ", " ++ 
             -- (if (nom `mod` n) == 0 then "\\\\\n" else "") ++ str, 
             -- nom + 1)
      -- ) ("",0) $ 

instance (Show a, Show b) => Показать' ([НаборСНомером a b]) where
   показать' ns m = 
      init . init . init . init .
      Lists.foldl' (\acc x -> показать' x m ++ ",\\\\\n" ++ acc) "" $ 
      reverse ns

instance (Ord a, Ord b, Show a, Show b) => Показать' (Правила (Maybe a) b) where
   показать' ps m = 
      " \nПронумерованные правила расширенной грамматики:\\\\\n" ++  
      (fst . Sets.foldl' (
         \(str, n) p -> 
            (str ++ show (n-1) ++ ": " ++ показать p ++
             (if (n `mod` m) == 0 then "\\\\\n" else "")
             , n + 1)
         ) ("",1) $ ps
      ) ++ " \n" 
 
-- Экземпляры класса Show         
instance Show ДействиеАнализатора where
   show (Перенос п)  = "перенос " ++ show п 
   show (Свёртка св) = "свёртка " ++ show св
   show Допуск       = "допуск"

instance (Show a, Show b) => Show (LR0Пункт (Maybe a) b) where
   show (Пункт {леваяЧасть = л, доТочки = д, послеТочки = п}) = 
      "\\textcolor{Green}{[}" ++ показатьНетермРасшГрамматики л ++ 
      "\\textcolor{Green}{$\\to$}" ++ показать д ++ 
      "\\textcolor{Green}{$\\bullet$}" ++ показать п ++ "\\textcolor{Green}{]}" 
      
-- Преамбулы документа.
преамбула ::  ФорматСтраницы -> String
преамбула ФорматА4 = преамбулаДляФорматаА4
преамбула f = "\\documentclass[" ++ show f ++ 
   частичнаяПреамбулаДляПрочихФорматов
   
преамбулаДляФорматаА4 :: String
преамбулаДляФорматаА4 =
   "\\documentclass[10pt]{article}\n\\usepackage[cp1251]{inputenc}\n"++
   "\\usepackage{longtable}\n\\usepackage{verbatim}\n\\usepackage{amsfonts}\n"++
   "\\usepackage{amssymb}\n\\usepackage{amsmath}\n\\usepackage{array}\n"++
   "\\usepackage{hhline}\n\\usepackage{indentfirst}\n\\usepackage{mathtext}\n"++
   "\\usepackage[russian]{babel}\n\\usepackage[pdftex]{graphicx}\n"++
   "\\usepackage{ccaption}\n\\usepackage[dvipsnames,usenames]{color}\n"++
   "\\usepackage{hyperref}\n\\hypersetup{unicode,breaklinks=true}\n"++
   "\\usepackage{syntax}\n\\usepackage{multicol}\n\\textheight 250mm\n"++
   "\\textwidth 171mm\n\\hoffset -25mm\n\\voffset -30mm\n"++
   "\\makeatletter\n\\@addtoreset{equation}{section}\n"++
   "\\renewcommand{\\theequation}{\\thesection.\\arabic{equation}}\n"++
   "\\@addtoreset{table}{section}\n\\@addtoreset{figure}{section}\n"++
   "\\renewcommand{\\thetable}{\\thesection.\\arabic{table}}\n"++
   "\\renewcommand{\\thefigure}{\\thesection.\\arabic{figure}}\n"++
   "\\makeatother\n\\footskip 8mm\n\\setcounter{section}{0}\n"++" \n"++
   "\\newcounter{rem}[section]\n"++
   "\\renewcommand{\\therem}{\\thesection.\\arabic{rem}}\n"++
   "\\newenvironment{Remark}{\\par\\refstepcounter{rem}"++
   "\\bf Замечание \\therem. \\it}{\\rm\\par}\n"++" \n"++
   "\\renewcommand{\\theenumi}{\\arabic{enumi}}\n"++
   "\\renewcommand{\\labelenumi}{\\theenumi)}\n"++" \n"++
   "\\newcounter{lem}[section]\n"++
   "\\renewcommand{\\thelem}{\\thesection.\\arabic{lem}}\n"++
   "\\newenvironment{Lemma}{\\par\\refstepcounter{lem}"++
   "\\bf Лемма \\thelem. \\it}{\\rm\\par}\n" ++ " \n"++
   "\\newcounter{cor}[section]\n"++
   "\\renewcommand{\\thecor}{\\thesection.\\arabic{cor}}\n"++
   "\\newenvironment{Corrolary}{\\par\\refstepcounter{cor}"++
   "\\bf Следствие \\thecor. \\it}{\\rm\\par}\n"++" \n"++
   "\\newcounter{theor}[section]\n"++
   "\\renewcommand{\\thetheor}{\\thesection.\\arabic{theor}}\n"++
   "\\newenvironment{Theorem}{\\par\\refstepcounter{theor}"++
   "\\bf Теорема \\thetheor. \\it}{\\rm\\par}\n"++" \n"++
   "\\newenvironment{Proof}{\\par\\noindent\\bf Доказательство.\\rm}{ \\par}\n"++
   " \n"++   "\\newcounter{exam}[section]\n"++
   "\\renewcommand{\\theexam}{\\thesection.\\arabic{exam}}\n"++
   "\\newenvironment{Example}{\\par\\refstepcounter{exam}"++
   "\\bf Пример \\theexam. \\rm}{\\rm\\par}\n"++" \n"++
   "\\newcounter{defin}[section]\n"++
   "\\renewcommand{\\thedefin}{\\thesection.\\arabic{defin}}\n"++
   "\\newenvironment{Definition}{\\par\\refstepcounter{defin}"++
   "\\bf Определение \\thedefin.\\it}{\\rm\\par}\n"++" \n"++
   "\\captiondelim{. }"++" \n"++
   "\\renewcommand{\\syntleft}{\\normalfont\\itshape}\n"++
   "\\renewcommand{\\syntright}{}\n"++" \n"++
   "\\makeatletter\n\\def\\@seccntformat#1{\\csname the#1\\endcsname.\\quad}\n"++
   "\\makeatother\n"++" \n"++"\\begin{document}\n"

частичнаяПреамбулаДляПрочихФорматов :: String
частичнаяПреамбулаДляПрочихФорматов =
   "]{a0poster}\n\\usepackage[cp1251]{inputenc}\n"++
   "\\usepackage{longtable}\n\\usepackage{verbatim}\n\\usepackage{amsfonts}\n"++
   "\\usepackage{amssymb}\n\\usepackage{amsmath}\n\\usepackage{array}\n"++
   "\\usepackage{hhline}\n\\usepackage{indentfirst}\n\\usepackage{mathtext}\n"++
   "\\usepackage[russian]{babel}\n\\usepackage[pdftex]{graphicx}\n"++
   "\\usepackage{ccaption}\n\\usepackage[dvipsnames,usenames]{color}\n"++
   "\\usepackage{hyperref}\n\\hypersetup{unicode,breaklinks=true}\n"++
   "\\usepackage{syntax}\n\\usepackage{multicol}\n"++
   "\\makeatletter\n\\@addtoreset{equation}{section}\n"++
   "\\renewcommand{\\theequation}{\\thesection.\\arabic{equation}}\n"++
   "\\@addtoreset{table}{section}\n\\@addtoreset{figure}{section}\n"++
   "\\renewcommand{\\thetable}{\\thesection.\\arabic{table}}\n"++
   "\\renewcommand{\\thefigure}{\\thesection.\\arabic{figure}}\n"++
   "\\makeatother\n\\footskip 8mm\n\\setcounter{section}{0}\n"++" \n"++
   "\\newcounter{rem}[section]\n"++
   "\\renewcommand{\\therem}{\\thesection.\\arabic{rem}}\n"++
   "\\newenvironment{Remark}{\\par\\refstepcounter{rem}"++
   "\\bf Замечание \\therem. \\it}{\\rm\\par}\n"++" \n"++
   "\\renewcommand{\\theenumi}{\\arabic{enumi}}\n"++
   "\\renewcommand{\\labelenumi}{\\theenumi)}\n"++" \n"++
   "\\newcounter{lem}[section]\n"++
   "\\renewcommand{\\thelem}{\\thesection.\\arabic{lem}}\n"++
   "\\newenvironment{Lemma}{\\par\\refstepcounter{lem}"++
   "\\bf Лемма \\thelem. \\it}{\\rm\\par}\n" ++ " \n"++
   "\\newcounter{cor}[section]\n"++
   "\\renewcommand{\\thecor}{\\thesection.\\arabic{cor}}\n"++
   "\\newenvironment{Corrolary}{\\par\\refstepcounter{cor}"++
   "\\bf Следствие \\thecor. \\it}{\\rm\\par}\n"++" \n"++
   "\\newcounter{theor}[section]\n"++
   "\\renewcommand{\\thetheor}{\\thesection.\\arabic{theor}}\n"++
   "\\newenvironment{Theorem}{\\par\\refstepcounter{theor}"++
   "\\bf Теорема \\thetheor. \\it}{\\rm\\par}\n"++" \n"++
   "\\newenvironment{Proof}{\\par\\noindent\\bf Доказательство.\\rm}{ \\par}\n"++
   " \n"++   "\\newcounter{exam}[section]\n"++
   "\\renewcommand{\\theexam}{\\thesection.\\arabic{exam}}\n"++
   "\\newenvironment{Example}{\\par\\refstepcounter{exam}"++
   "\\bf Пример \\theexam. \\rm}{\\rm\\par}\n"++" \n"++
   "\\newcounter{defin}[section]\n"++
   "\\renewcommand{\\thedefin}{\\thesection.\\arabic{defin}}\n"++
   "\\newenvironment{Definition}{\\par\\refstepcounter{defin}"++
   "\\bf Определение \\thedefin.\\it}{\\rm\\par}\n"++" \n"++
   "\\captiondelim{. }"++" \n"++
   "\\renewcommand{\\syntleft}{\\normalfont\\itshape}\n"++
   "\\renewcommand{\\syntright}{}\n"++" \n"++
   "\\makeatletter\n\\def\\@seccntformat#1{\\csname the#1\\endcsname.\\quad}\n"++
   "\\makeatother\n"++" \n"++"\\begin{document}\n" ++ "\\tiny"

-- Конец документа
конецДокумента :: String
конецДокумента = "\\end{document}" 

-- Функция преобразования результатов вычислений в строковое представление:
результат :: (Ord a, Ord b, Show a, Show b) =>
             [Правило a b] ->  -- список правил 
             a ->              -- начальный нетерминал
             Форматирование -> -- сведения о форматировании 
             String
результат списокПравил начНетерм формат = 
   преамбула формСтр ++ 
   показать' действия ячДЕЙСТВИЕ ++ "\n\\\\\n" ++ 
   показать' переходы ячПЕРЕХОД ++ "\n\\\\\n" ++ 
   показать' правила правилНаСтроку ++ "\n " ++ " \nСписок наборов пунктов:\\\\\n" ++
   показать' (map (\x -> НумерованныйНабор x) наборы) пунктовНаСтроку ++ "\n" ++    
   конецДокумента
   where
       формСтр         = форматСтраницы формат
       ячДЕЙСТВИЕ      = ячеекДЕЙСТВИЕвСтроке формат
       ячПЕРЕХОД       = ячеекПЕРЕХОДвСтроке формат
       пунктовНаСтроку = пунктовВСтроке формат
       правилНаСтроку  = правилВСтроке формат
       грамматика      = Sets.fromList списокПравил
       (действия, переходы, правила, наборы) =
           таблицыДЕЙСТВИЕиПЕРЕХОД грамматика начНетерм