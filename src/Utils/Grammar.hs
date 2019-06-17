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
  ExpSign     | Format
 deriving(Eq,Show,Ord,Read,Enum,Bounded)

data НетерминалыМУР128 =
  Programm       | EntryPoint  | Body     | Section         |
  Attr           | Args        | Register | Command         |
  Delimiter
 deriving(Eq,Show,Ord,Read,Enum,Bounded)

грамматикаМУР128 :: [Правило НетерминалыМУР128 ТерминалыМУР128]
грамматикаМУР128 =
   [
    Programm --> [Нетерминал Body],
    Body --> [Нетерминал Register, Нетерминал Command, Нетерминал Args],
    Register --> [Терминал RInt],
    Register --> [Терминал FInt],
    Register --> [Терминал SP],
    Register --> [Терминал BP],
    Command --> [Терминал Movim],
    Command --> [Терминал LoadReg],
    Command --> [Терминал StoreReg],
    Command --> [Терминал PushPopIm],
    Command --> [Терминал Arifm3Args],
    Command --> [Терминал MovRegF],
    Command --> [Терминал StoreRegF],
    Command --> [Терминал JumprReg2],
    Command --> [Терминал LoadRegF],
    Command --> [Терминал Pup],
    Command --> [Терминал Arifm3F],
    Command --> [Терминал Arifm2F],
    Command --> [Терминал Mul2f],
    Command --> [Терминал JumpReg1],
    Command --> [Терминал JumprReg2],
    Command --> [Терминал JumpCC],
    Command --> [Терминал JumpCCReg],
    Command --> [Терминал CallRel],
    Command --> [Терминал CellCon],
    Command --> [Терминал Ret],
    Command --> [Терминал Reta],
    Command --> [Терминал RetaCon],
    Attr --> [Терминал Readable, Терминал Writable, Терминал Executable],
    Delimiter --> [Терминал Comma, Терминал OpenBrace, Терминал CloseBrace, Терминал Prescision, Терминал Pluse, Терминал Minus, Терминал Mul, Терминал Div],
    Args --> [Терминал Single, Терминал Double, Терминал Quatro, Терминал Extended, Терминал Integer]
   ]
