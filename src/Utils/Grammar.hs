module Grammar (
   НетерминалыМУР128(..),

   ТерминалыМУР128(..),

   грамматикаМУР128
) where

import Rules

--ЧелослиленныйРегистр(Ринт)
--ВещественныйРегистр(Финт)
--СП
--БП(стек)
--Movim
--Loadreg
--Storereg
--PushPopIm
--Arifm3Args до ршифта + сравнения
--MovRegF
--StoreRegF
--LoadRegF
--Pup
--Arifm3F
--Arifm2F + chsf
--Mul2f
--JumpReg1
--JumprReg2
--JumpCC
--JumorCCReg
--CallRel
--CellCon
--Ret + Reti
--Reta
--RetaCon

data ТерминалыМУР128 =
  RInt        | FInt          | SP         | BP         | Movim     |
  LoadReg     | StoreReg      | PushPopIm  | Arifm3Args | MovRegF   |
  StoreRegF   | LoadRegF      | Pup        | Arifm3F    | Arifm2F   |
  Mul2f       | JumpReg1      | JumprReg2  | JumpCC     | JumpCCReg |
  CallRel     | CellCon       | Ret        | Reta       | RetaCon   |
  OpenBracket | CloseBracket  | StartBlock | EndBlock   | Id        |
  Semicolon   | Readable      | Writable   | Executable | Enter     |
  Macro       | Epsilon       | Comma      | OpenBrace  | CloseBrace|
  Prescision  | Pluse         | Minus      | Mul        | Div       |
  Mod         | Xor           | Or         | Add        | Not       |
  Single      | Double        | Quatro     | Extended   | Integer   |
  ExpSign


 deriving(Eq,Show,Ord,Read,Enum,Bounded)

data НетерминалыМУР128 =
  Programm       | EntryPoint  | Body     | Section         |
  Attr           | Args
 deriving(Eq,Show,Ord,Read,Enum,Bounded)

грамматикаМУР128 :: [Правило НетерминалыМУР128 ТерминалыМУР128]
грамматикаМУР128 =
   [ 
   ]
