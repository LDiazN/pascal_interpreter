{-# OPTIONS_GHC -w #-}
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (MainProgram)
	| HappyAbsSyn5 (Program)
	| HappyAbsSyn6 (Statement)
	| HappyAbsSyn7 ([Statement])
	| HappyAbsSyn10 ((Token, [Exp]))
	| HappyAbsSyn11 ([Exp])
	| HappyAbsSyn14 ([(Exp, Statement)])
	| HappyAbsSyn19 ([Declaration])
	| HappyAbsSyn20 (Declaration)
	| HappyAbsSyn21 ([(String, DataType)])
	| HappyAbsSyn23 ([(Token, DataType)])
	| HappyAbsSyn24 ([Token])
	| HappyAbsSyn25 (DataType)
	| HappyAbsSyn26 (Exp)
	| HappyAbsSyn32 (String)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151 :: () => Int -> ({-HappyReduction (Parser) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81 :: () => ({-HappyReduction (Parser) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,350) ([0,0,64,0,0,0,0,256,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,1024,0,0,36,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19456,17193,0,0,0,0,0,1,0,0,0,0,4,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,2,0,0,0,32768,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,2,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4032,8320,0,0,0,16128,33280,0,0,0,64512,2048,2,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,528,0,0,0,1008,2080,0,0,0,4032,24704,0,0,0,0,1024,0,0,0,0,0,0,0,0,4,0,0,0,0,0,8064,0,0,0,0,48,1,0,0,0,192,4,0,0,0,7168,8,0,0,0,28672,32,0,0,0,3840,33280,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,960,8320,0,0,0,16128,33280,0,0,16384,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,4096,3237,1,0,0,0,0,3,0,0,0,0,16,0,0,0,0,0,0,0,0,0,256,0,0,0,0,1024,0,0,0,0,0,32768,16,0,0,0,0,0,0,0,0,0,264,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20736,4298,0,0,0,0,32768,3,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,960,8320,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,130,0,0,0,60,520,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15360,2048,2,0,0,61440,8195,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37952,1074,0,0,0,0,61440,8195,8,0,0,0,0,72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4032,8320,0,0,0,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,8192,0,0,0,0,0,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,1024,0,0,0,0,0,0,1,0,0,0,0,2,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,10564,67,0,0,0,42256,268,0,0,0,0,14336,0,0,0,0,0,0,0,0,0,32768,3,0,0,0,0,63,130,0,0,0,252,520,0,0,0,0,0,0,0,4096,0,0,0,0,16384,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20736,4298,0,0,0,17408,17193,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyParser","MainProgram","Program","Block","Statements","Statement","Assign","FuncCall","FuncArgs","IfStatement","CaseStmnt","Cases","Case","CaseLabels","WhileDo","ForDo","Declarations","FuncDeclar","FuncArgsDec","VarDeclars","VarDeclars2","Names","DataType","Expr","SimpleExpr","AddOprs","Term","MulOprs","Factor","RelOpr","UnOper","AddOpr","MulOpr","Literl","begin","end","program","var","if","else","case","then","of","while","do","for","to","downto","break","continue","function","procedure","real","boolean","name","num","true","false","'+'","'-'","'*'","'/'","'%'","'='","'>='","'<='","'<'","'>'","'<>'","and","or","not","':='","';'","':'","','","'.'","'('","')'","%eof"]
        bit_start = st * 82
        bit_end = (st + 1) * 82
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..81]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (39) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (39) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (57) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (82) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (76) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (5) = happyGoto action_6
action_5 (19) = happyGoto action_7
action_5 _ = happyReduce_33

action_6 (79) = happyShift action_15
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (37) = happyShift action_11
action_7 (40) = happyShift action_12
action_7 (53) = happyShift action_13
action_7 (54) = happyShift action_14
action_7 (6) = happyGoto action_8
action_7 (20) = happyGoto action_9
action_7 (22) = happyGoto action_10
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_2

action_9 _ = happyReduce_34

action_10 _ = happyReduce_35

action_11 (37) = happyShift action_11
action_11 (38) = happyShift action_30
action_11 (41) = happyShift action_31
action_11 (43) = happyShift action_32
action_11 (46) = happyShift action_33
action_11 (48) = happyShift action_34
action_11 (51) = happyShift action_35
action_11 (52) = happyShift action_36
action_11 (57) = happyShift action_37
action_11 (6) = happyGoto action_21
action_11 (7) = happyGoto action_22
action_11 (8) = happyGoto action_23
action_11 (9) = happyGoto action_24
action_11 (10) = happyGoto action_25
action_11 (12) = happyGoto action_26
action_11 (13) = happyGoto action_27
action_11 (17) = happyGoto action_28
action_11 (18) = happyGoto action_29
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (57) = happyShift action_20
action_12 (23) = happyGoto action_18
action_12 (24) = happyGoto action_19
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (57) = happyShift action_17
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (57) = happyShift action_16
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_1

action_16 (80) = happyShift action_65
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (80) = happyShift action_64
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (76) = happyShift action_63
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (77) = happyShift action_61
action_19 (78) = happyShift action_62
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_42

action_21 _ = happyReduce_14

action_22 (38) = happyShift action_59
action_22 (76) = happyShift action_60
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_5

action_24 _ = happyReduce_9

action_25 _ = happyReduce_8

action_26 _ = happyReduce_10

action_27 _ = happyReduce_11

action_28 _ = happyReduce_12

action_29 _ = happyReduce_13

action_30 _ = happyReduce_3

action_31 (57) = happyShift action_49
action_31 (58) = happyShift action_50
action_31 (59) = happyShift action_51
action_31 (60) = happyShift action_52
action_31 (61) = happyShift action_53
action_31 (62) = happyShift action_54
action_31 (74) = happyShift action_55
action_31 (80) = happyShift action_56
action_31 (10) = happyGoto action_41
action_31 (26) = happyGoto action_58
action_31 (27) = happyGoto action_43
action_31 (28) = happyGoto action_44
action_31 (29) = happyGoto action_45
action_31 (30) = happyGoto action_46
action_31 (31) = happyGoto action_47
action_31 (33) = happyGoto action_48
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (57) = happyShift action_49
action_32 (58) = happyShift action_50
action_32 (59) = happyShift action_51
action_32 (60) = happyShift action_52
action_32 (61) = happyShift action_53
action_32 (62) = happyShift action_54
action_32 (74) = happyShift action_55
action_32 (80) = happyShift action_56
action_32 (10) = happyGoto action_41
action_32 (26) = happyGoto action_57
action_32 (27) = happyGoto action_43
action_32 (28) = happyGoto action_44
action_32 (29) = happyGoto action_45
action_32 (30) = happyGoto action_46
action_32 (31) = happyGoto action_47
action_32 (33) = happyGoto action_48
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (57) = happyShift action_49
action_33 (58) = happyShift action_50
action_33 (59) = happyShift action_51
action_33 (60) = happyShift action_52
action_33 (61) = happyShift action_53
action_33 (62) = happyShift action_54
action_33 (74) = happyShift action_55
action_33 (80) = happyShift action_56
action_33 (10) = happyGoto action_41
action_33 (26) = happyGoto action_42
action_33 (27) = happyGoto action_43
action_33 (28) = happyGoto action_44
action_33 (29) = happyGoto action_45
action_33 (30) = happyGoto action_46
action_33 (31) = happyGoto action_47
action_33 (33) = happyGoto action_48
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (57) = happyShift action_40
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_15

action_36 _ = happyReduce_16

action_37 (75) = happyShift action_38
action_37 (80) = happyShift action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (57) = happyShift action_49
action_38 (58) = happyShift action_50
action_38 (59) = happyShift action_51
action_38 (60) = happyShift action_52
action_38 (61) = happyShift action_53
action_38 (62) = happyShift action_54
action_38 (74) = happyShift action_55
action_38 (80) = happyShift action_56
action_38 (10) = happyGoto action_41
action_38 (26) = happyGoto action_102
action_38 (27) = happyGoto action_43
action_38 (28) = happyGoto action_44
action_38 (29) = happyGoto action_45
action_38 (30) = happyGoto action_46
action_38 (31) = happyGoto action_47
action_38 (33) = happyGoto action_48
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (57) = happyShift action_49
action_39 (58) = happyShift action_50
action_39 (59) = happyShift action_51
action_39 (60) = happyShift action_52
action_39 (61) = happyShift action_53
action_39 (62) = happyShift action_54
action_39 (74) = happyShift action_55
action_39 (80) = happyShift action_56
action_39 (81) = happyShift action_101
action_39 (10) = happyGoto action_41
action_39 (11) = happyGoto action_99
action_39 (26) = happyGoto action_100
action_39 (27) = happyGoto action_43
action_39 (28) = happyGoto action_44
action_39 (29) = happyGoto action_45
action_39 (30) = happyGoto action_46
action_39 (31) = happyGoto action_47
action_39 (33) = happyGoto action_48
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (75) = happyShift action_98
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_63

action_42 (47) = happyShift action_97
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (66) = happyShift action_91
action_43 (67) = happyShift action_92
action_43 (68) = happyShift action_93
action_43 (69) = happyShift action_94
action_43 (70) = happyShift action_95
action_43 (71) = happyShift action_96
action_43 (32) = happyGoto action_90
action_43 _ = happyReduce_46

action_44 (61) = happyShift action_86
action_44 (62) = happyShift action_87
action_44 (73) = happyShift action_88
action_44 (34) = happyGoto action_89
action_44 _ = happyReduce_50

action_45 (61) = happyShift action_86
action_45 (62) = happyShift action_87
action_45 (73) = happyShift action_88
action_45 (34) = happyGoto action_85
action_45 _ = happyReduce_48

action_46 (63) = happyShift action_80
action_46 (64) = happyShift action_81
action_46 (65) = happyShift action_82
action_46 (72) = happyShift action_83
action_46 (35) = happyGoto action_84
action_46 _ = happyReduce_54

action_47 (63) = happyShift action_80
action_47 (64) = happyShift action_81
action_47 (65) = happyShift action_82
action_47 (72) = happyShift action_83
action_47 (35) = happyGoto action_79
action_47 _ = happyReduce_53

action_48 (57) = happyShift action_49
action_48 (58) = happyShift action_50
action_48 (59) = happyShift action_51
action_48 (60) = happyShift action_52
action_48 (74) = happyShift action_55
action_48 (80) = happyShift action_56
action_48 (10) = happyGoto action_41
action_48 (29) = happyGoto action_78
action_48 (30) = happyGoto action_46
action_48 (31) = happyGoto action_47
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (80) = happyShift action_39
action_49 _ = happyReduce_58

action_50 _ = happyReduce_57

action_51 _ = happyReduce_59

action_52 _ = happyReduce_60

action_53 _ = happyReduce_70

action_54 _ = happyReduce_71

action_55 (57) = happyShift action_49
action_55 (58) = happyShift action_50
action_55 (59) = happyShift action_51
action_55 (60) = happyShift action_52
action_55 (74) = happyShift action_55
action_55 (80) = happyShift action_56
action_55 (10) = happyGoto action_41
action_55 (31) = happyGoto action_77
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (57) = happyShift action_49
action_56 (58) = happyShift action_50
action_56 (59) = happyShift action_51
action_56 (60) = happyShift action_52
action_56 (61) = happyShift action_53
action_56 (62) = happyShift action_54
action_56 (74) = happyShift action_55
action_56 (80) = happyShift action_56
action_56 (10) = happyGoto action_41
action_56 (26) = happyGoto action_76
action_56 (27) = happyGoto action_43
action_56 (28) = happyGoto action_44
action_56 (29) = happyGoto action_45
action_56 (30) = happyGoto action_46
action_56 (31) = happyGoto action_47
action_56 (33) = happyGoto action_48
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (45) = happyShift action_75
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (44) = happyShift action_74
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_4

action_60 (37) = happyShift action_11
action_60 (41) = happyShift action_31
action_60 (43) = happyShift action_32
action_60 (46) = happyShift action_33
action_60 (48) = happyShift action_34
action_60 (51) = happyShift action_35
action_60 (52) = happyShift action_36
action_60 (57) = happyShift action_37
action_60 (6) = happyGoto action_21
action_60 (8) = happyGoto action_73
action_60 (9) = happyGoto action_24
action_60 (10) = happyGoto action_25
action_60 (12) = happyGoto action_26
action_60 (13) = happyGoto action_27
action_60 (17) = happyGoto action_28
action_60 (18) = happyGoto action_29
action_60 _ = happyReduce_7

action_61 (55) = happyShift action_71
action_61 (56) = happyShift action_72
action_61 (25) = happyGoto action_70
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (57) = happyShift action_69
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_40

action_64 (57) = happyShift action_20
action_64 (21) = happyGoto action_68
action_64 (23) = happyGoto action_67
action_64 (24) = happyGoto action_19
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (57) = happyShift action_20
action_65 (21) = happyGoto action_66
action_65 (23) = happyGoto action_67
action_65 (24) = happyGoto action_19
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (76) = happyShift action_121
action_66 (81) = happyShift action_123
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_38

action_68 (76) = happyShift action_121
action_68 (81) = happyShift action_122
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_43

action_70 _ = happyReduce_41

action_71 _ = happyReduce_44

action_72 _ = happyReduce_45

action_73 _ = happyReduce_6

action_74 (37) = happyShift action_11
action_74 (41) = happyShift action_31
action_74 (43) = happyShift action_32
action_74 (46) = happyShift action_33
action_74 (48) = happyShift action_34
action_74 (51) = happyShift action_35
action_74 (52) = happyShift action_36
action_74 (57) = happyShift action_37
action_74 (6) = happyGoto action_21
action_74 (8) = happyGoto action_120
action_74 (9) = happyGoto action_24
action_74 (10) = happyGoto action_25
action_74 (12) = happyGoto action_26
action_74 (13) = happyGoto action_27
action_74 (17) = happyGoto action_28
action_74 (18) = happyGoto action_29
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (58) = happyShift action_117
action_75 (59) = happyShift action_118
action_75 (60) = happyShift action_119
action_75 (14) = happyGoto action_113
action_75 (15) = happyGoto action_114
action_75 (16) = happyGoto action_115
action_75 (36) = happyGoto action_116
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (81) = happyShift action_112
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_62

action_78 _ = happyReduce_49

action_79 (57) = happyShift action_49
action_79 (58) = happyShift action_50
action_79 (59) = happyShift action_51
action_79 (60) = happyShift action_52
action_79 (74) = happyShift action_55
action_79 (80) = happyShift action_56
action_79 (10) = happyGoto action_41
action_79 (31) = happyGoto action_111
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_75

action_81 _ = happyReduce_76

action_82 _ = happyReduce_77

action_83 _ = happyReduce_78

action_84 (57) = happyShift action_49
action_84 (58) = happyShift action_50
action_84 (59) = happyShift action_51
action_84 (60) = happyShift action_52
action_84 (74) = happyShift action_55
action_84 (80) = happyShift action_56
action_84 (10) = happyGoto action_41
action_84 (31) = happyGoto action_110
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (57) = happyShift action_49
action_85 (58) = happyShift action_50
action_85 (59) = happyShift action_51
action_85 (60) = happyShift action_52
action_85 (74) = happyShift action_55
action_85 (80) = happyShift action_56
action_85 (10) = happyGoto action_41
action_85 (29) = happyGoto action_109
action_85 (30) = happyGoto action_46
action_85 (31) = happyGoto action_47
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_72

action_87 _ = happyReduce_73

action_88 _ = happyReduce_74

action_89 (57) = happyShift action_49
action_89 (58) = happyShift action_50
action_89 (59) = happyShift action_51
action_89 (60) = happyShift action_52
action_89 (74) = happyShift action_55
action_89 (80) = happyShift action_56
action_89 (10) = happyGoto action_41
action_89 (29) = happyGoto action_108
action_89 (30) = happyGoto action_46
action_89 (31) = happyGoto action_47
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (57) = happyShift action_49
action_90 (58) = happyShift action_50
action_90 (59) = happyShift action_51
action_90 (60) = happyShift action_52
action_90 (61) = happyShift action_53
action_90 (62) = happyShift action_54
action_90 (74) = happyShift action_55
action_90 (80) = happyShift action_56
action_90 (10) = happyGoto action_41
action_90 (27) = happyGoto action_107
action_90 (28) = happyGoto action_44
action_90 (29) = happyGoto action_45
action_90 (30) = happyGoto action_46
action_90 (31) = happyGoto action_47
action_90 (33) = happyGoto action_48
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_64

action_92 _ = happyReduce_67

action_93 _ = happyReduce_66

action_94 _ = happyReduce_69

action_95 _ = happyReduce_68

action_96 _ = happyReduce_65

action_97 (37) = happyShift action_11
action_97 (41) = happyShift action_31
action_97 (43) = happyShift action_32
action_97 (46) = happyShift action_33
action_97 (48) = happyShift action_34
action_97 (51) = happyShift action_35
action_97 (52) = happyShift action_36
action_97 (57) = happyShift action_37
action_97 (6) = happyGoto action_21
action_97 (8) = happyGoto action_106
action_97 (9) = happyGoto action_24
action_97 (10) = happyGoto action_25
action_97 (12) = happyGoto action_26
action_97 (13) = happyGoto action_27
action_97 (17) = happyGoto action_28
action_97 (18) = happyGoto action_29
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (57) = happyShift action_49
action_98 (58) = happyShift action_50
action_98 (59) = happyShift action_51
action_98 (60) = happyShift action_52
action_98 (61) = happyShift action_53
action_98 (62) = happyShift action_54
action_98 (74) = happyShift action_55
action_98 (80) = happyShift action_56
action_98 (10) = happyGoto action_41
action_98 (26) = happyGoto action_105
action_98 (27) = happyGoto action_43
action_98 (28) = happyGoto action_44
action_98 (29) = happyGoto action_45
action_98 (30) = happyGoto action_46
action_98 (31) = happyGoto action_47
action_98 (33) = happyGoto action_48
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (78) = happyShift action_103
action_99 (81) = happyShift action_104
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_20

action_101 _ = happyReduce_18

action_102 _ = happyReduce_17

action_103 (57) = happyShift action_49
action_103 (58) = happyShift action_50
action_103 (59) = happyShift action_51
action_103 (60) = happyShift action_52
action_103 (61) = happyShift action_53
action_103 (62) = happyShift action_54
action_103 (74) = happyShift action_55
action_103 (80) = happyShift action_56
action_103 (10) = happyGoto action_41
action_103 (26) = happyGoto action_134
action_103 (27) = happyGoto action_43
action_103 (28) = happyGoto action_44
action_103 (29) = happyGoto action_45
action_103 (30) = happyGoto action_46
action_103 (31) = happyGoto action_47
action_103 (33) = happyGoto action_48
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_19

action_105 (49) = happyShift action_132
action_105 (50) = happyShift action_133
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_30

action_107 _ = happyReduce_47

action_108 _ = happyReduce_52

action_109 _ = happyReduce_51

action_110 _ = happyReduce_56

action_111 _ = happyReduce_55

action_112 _ = happyReduce_61

action_113 (38) = happyShift action_130
action_113 (76) = happyShift action_131
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_25

action_115 (77) = happyShift action_128
action_115 (78) = happyShift action_129
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_28

action_117 _ = happyReduce_79

action_118 _ = happyReduce_80

action_119 _ = happyReduce_81

action_120 (42) = happyShift action_127
action_120 _ = happyReduce_22

action_121 (57) = happyShift action_20
action_121 (23) = happyGoto action_126
action_121 (24) = happyGoto action_19
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (77) = happyShift action_125
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (76) = happyShift action_124
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (5) = happyGoto action_142
action_124 (19) = happyGoto action_7
action_124 _ = happyReduce_33

action_125 (55) = happyShift action_71
action_125 (56) = happyShift action_72
action_125 (25) = happyGoto action_141
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_39

action_127 (37) = happyShift action_11
action_127 (41) = happyShift action_31
action_127 (43) = happyShift action_32
action_127 (46) = happyShift action_33
action_127 (48) = happyShift action_34
action_127 (51) = happyShift action_35
action_127 (52) = happyShift action_36
action_127 (57) = happyShift action_37
action_127 (6) = happyGoto action_21
action_127 (8) = happyGoto action_140
action_127 (9) = happyGoto action_24
action_127 (10) = happyGoto action_25
action_127 (12) = happyGoto action_26
action_127 (13) = happyGoto action_27
action_127 (17) = happyGoto action_28
action_127 (18) = happyGoto action_29
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (37) = happyShift action_11
action_128 (41) = happyShift action_31
action_128 (43) = happyShift action_32
action_128 (46) = happyShift action_33
action_128 (48) = happyShift action_34
action_128 (51) = happyShift action_35
action_128 (52) = happyShift action_36
action_128 (57) = happyShift action_37
action_128 (6) = happyGoto action_21
action_128 (8) = happyGoto action_139
action_128 (9) = happyGoto action_24
action_128 (10) = happyGoto action_25
action_128 (12) = happyGoto action_26
action_128 (13) = happyGoto action_27
action_128 (17) = happyGoto action_28
action_128 (18) = happyGoto action_29
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (58) = happyShift action_117
action_129 (59) = happyShift action_118
action_129 (60) = happyShift action_119
action_129 (36) = happyGoto action_138
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_24

action_131 (58) = happyShift action_117
action_131 (59) = happyShift action_118
action_131 (60) = happyShift action_119
action_131 (15) = happyGoto action_137
action_131 (16) = happyGoto action_115
action_131 (36) = happyGoto action_116
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (57) = happyShift action_49
action_132 (58) = happyShift action_50
action_132 (59) = happyShift action_51
action_132 (60) = happyShift action_52
action_132 (61) = happyShift action_53
action_132 (62) = happyShift action_54
action_132 (74) = happyShift action_55
action_132 (80) = happyShift action_56
action_132 (10) = happyGoto action_41
action_132 (26) = happyGoto action_136
action_132 (27) = happyGoto action_43
action_132 (28) = happyGoto action_44
action_132 (29) = happyGoto action_45
action_132 (30) = happyGoto action_46
action_132 (31) = happyGoto action_47
action_132 (33) = happyGoto action_48
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (57) = happyShift action_49
action_133 (58) = happyShift action_50
action_133 (59) = happyShift action_51
action_133 (60) = happyShift action_52
action_133 (61) = happyShift action_53
action_133 (62) = happyShift action_54
action_133 (74) = happyShift action_55
action_133 (80) = happyShift action_56
action_133 (10) = happyGoto action_41
action_133 (26) = happyGoto action_135
action_133 (27) = happyGoto action_43
action_133 (28) = happyGoto action_44
action_133 (29) = happyGoto action_45
action_133 (30) = happyGoto action_46
action_133 (31) = happyGoto action_47
action_133 (33) = happyGoto action_48
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_21

action_135 (47) = happyShift action_147
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (47) = happyShift action_146
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (76) = happyShift action_145
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_29

action_139 _ = happyReduce_27

action_140 _ = happyReduce_23

action_141 (76) = happyShift action_144
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (76) = happyShift action_143
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_37

action_144 (5) = happyGoto action_150
action_144 (19) = happyGoto action_7
action_144 _ = happyReduce_33

action_145 _ = happyReduce_26

action_146 (37) = happyShift action_11
action_146 (41) = happyShift action_31
action_146 (43) = happyShift action_32
action_146 (46) = happyShift action_33
action_146 (48) = happyShift action_34
action_146 (51) = happyShift action_35
action_146 (52) = happyShift action_36
action_146 (57) = happyShift action_37
action_146 (6) = happyGoto action_21
action_146 (8) = happyGoto action_149
action_146 (9) = happyGoto action_24
action_146 (10) = happyGoto action_25
action_146 (12) = happyGoto action_26
action_146 (13) = happyGoto action_27
action_146 (17) = happyGoto action_28
action_146 (18) = happyGoto action_29
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (37) = happyShift action_11
action_147 (41) = happyShift action_31
action_147 (43) = happyShift action_32
action_147 (46) = happyShift action_33
action_147 (48) = happyShift action_34
action_147 (51) = happyShift action_35
action_147 (52) = happyShift action_36
action_147 (57) = happyShift action_37
action_147 (6) = happyGoto action_21
action_147 (8) = happyGoto action_148
action_147 (9) = happyGoto action_24
action_147 (10) = happyGoto action_25
action_147 (12) = happyGoto action_26
action_147 (13) = happyGoto action_27
action_147 (17) = happyGoto action_28
action_147 (18) = happyGoto action_29
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_32

action_149 _ = happyReduce_31

action_150 (76) = happyShift action_151
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_36

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((getId happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn5
		 (Program happy_var_2  (reverse happy_var_1)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn6
		 (Block []
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Block (reverse happy_var_2)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (ProcCall (getId . fst $ happy_var_1) (snd happy_var_1) (getPos . fst $ happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn6
		 (Break
	)

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn6
		 (Continue
	)

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn26  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (Assign (getId happy_var_1) happy_var_3 (getPos happy_var_1)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_1, [])
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 10 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1, reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3:happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (If happy_var_2 happy_var_4 Skip (-1) (getPos happy_var_1)
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (If happy_var_2 happy_var_4 happy_var_6 (-1) (getPos happy_var_1)
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Case happy_var_2 happy_var_4 Skip (-1)
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 14 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (happy_var_1 ++ happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 ([(e, happy_var_3) | e <- happy_var_1]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3:happy_var_1
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (While happy_var_2 happy_var_4 (-1) (getPos happy_var_1)
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 8 18 happyReduction_31
happyReduction_31 ((HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (For (getId happy_var_2) happy_var_4 happy_var_6 happy_var_8 "to" (-1)
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 8 18 happyReduction_32
happyReduction_32 ((HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (For (getId happy_var_2) happy_var_4 happy_var_6 happy_var_8 "downto" (-1)
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  19 happyReduction_33
happyReduction_33  =  HappyAbsSyn19
		 ([]
	)

happyReduce_34 = happySpecReduce_2  19 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2:happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  19 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 10 20 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Function (getId happy_var_2) happy_var_4 happy_var_7 happy_var_9 (getPos happy_var_2)
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 8 20 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Function (getId happy_var_2) happy_var_4 NoneT happy_var_7 (getPos happy_var_2)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  21 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 (reverse [ (getId t, dt) | (t, dt) <- happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  21 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 ++ reverse [ (getId t, dt) | (t, dt) <- happy_var_3]
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  22 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn19
		 ([ Variable (getId s) t (getPos s) | (s, t) <- happy_var_2 ]
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  23 happyReduction_41
happyReduction_41 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 ([(s,happy_var_3) | s <- happy_var_1]
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  24 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3:happy_var_1
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn25
		 (RealT
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn25
		 (BooleanT
	)

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (BinaryOp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  27 happyReduction_49
happyReduction_49 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn26
		 (NumExpr (Op1 happy_var_1 happy_var_2)
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  27 happyReduction_50
happyReduction_50 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  28 happyReduction_51
happyReduction_51 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (BinaryOp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  28 happyReduction_52
happyReduction_52 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (BinaryOp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  29 happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  29 happyReduction_54
happyReduction_54 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  30 happyReduction_55
happyReduction_55 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (BinaryOp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  30 happyReduction_56
happyReduction_56 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (BinaryOp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 (HappyTerminal (Token _ (TkReal happy_var_1)))
	 =  HappyAbsSyn26
		 (NumExpr (NumConst happy_var_1)
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  31 happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (IdExpr (getId happy_var_1) (getPos happy_var_1)
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  31 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn26
		 (BoolExpr TrueC
	)

happyReduce_60 = happySpecReduce_1  31 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn26
		 (BoolExpr FalseC
	)

happyReduce_61 = happySpecReduce_3  31 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  31 happyReduction_62
happyReduction_62 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (BoolExpr (Not happy_var_2)
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  31 happyReduction_63
happyReduction_63 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn26
		 (FunExpr ( getId . fst $ happy_var_1) (snd happy_var_1) (getPos . fst $ happy_var_1)
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  32 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn32
		 ("="
	)

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn32
		 ("<>"
	)

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn32
		 ("<="
	)

happyReduce_67 = happySpecReduce_1  32 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn32
		 (">="
	)

happyReduce_68 = happySpecReduce_1  32 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn32
		 (">"
	)

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn32
		 ("<"
	)

happyReduce_70 = happySpecReduce_1  33 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn32
		 ("+"
	)

happyReduce_71 = happySpecReduce_1  33 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn32
		 ("-"
	)

happyReduce_72 = happySpecReduce_1  34 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn32
		 ("+"
	)

happyReduce_73 = happySpecReduce_1  34 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn32
		 ("-"
	)

happyReduce_74 = happySpecReduce_1  34 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn32
		 ("or"
	)

happyReduce_75 = happySpecReduce_1  35 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn32
		 ("*"
	)

happyReduce_76 = happySpecReduce_1  35 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn32
		 ("/"
	)

happyReduce_77 = happySpecReduce_1  35 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn32
		 ("%"
	)

happyReduce_78 = happySpecReduce_1  35 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn32
		 ("and"
	)

happyReduce_79 = happySpecReduce_1  36 happyReduction_79
happyReduction_79 (HappyTerminal (Token _ (TkReal happy_var_1)))
	 =  HappyAbsSyn26
		 (NumExpr (NumConst happy_var_1)
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  36 happyReduction_80
happyReduction_80 _
	 =  HappyAbsSyn26
		 (BoolExpr (TrueC)
	)

happyReduce_81 = happySpecReduce_1  36 happyReduction_81
happyReduction_81 _
	 =  HappyAbsSyn26
		 (BoolExpr (FalseC)
	)

happyNewToken action sts stk [] =
	action 82 82 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token _ (TkGen "begin") -> cont 37;
	Token _ (TkGen "end") -> cont 38;
	Token _ (TkGen "program") -> cont 39;
	Token _ (TkGen "var") -> cont 40;
	Token _ (TkGen "if") -> cont 41;
	Token _ (TkGen "else") -> cont 42;
	Token _ (TkGen "case") -> cont 43;
	Token _ (TkGen "then") -> cont 44;
	Token _ (TkGen "of") -> cont 45;
	Token _ (TkGen "while" ) -> cont 46;
	Token _ (TkGen "do" ) -> cont 47;
	Token _ (TkGen "for" ) -> cont 48;
	Token _ (TkGen "to" ) -> cont 49;
	Token _ (TkGen "downto" ) -> cont 50;
	Token _ (TkGen "break" ) -> cont 51;
	Token _ (TkGen "continue") -> cont 52;
	Token _ (TkGen "function") -> cont 53;
	Token _ (TkGen "procedure") -> cont 54;
	Token _ (TkGen "real") -> cont 55;
	Token _ (TkGen "boolean") -> cont 56;
	Token _ (TkId  _) -> cont 57;
	Token _ (TkReal happy_dollar_dollar) -> cont 58;
	Token _ (TkGen "true") -> cont 59;
	Token _ (TkGen "false") -> cont 60;
	Token _ (TkGen "+") -> cont 61;
	Token _ (TkGen "-") -> cont 62;
	Token _ (TkGen "*") -> cont 63;
	Token _ (TkGen "/") -> cont 64;
	Token _ (TkGen "%") -> cont 65;
	Token _ (TkGen "=") -> cont 66;
	Token _ (TkGen ">=") -> cont 67;
	Token _ (TkGen "<=") -> cont 68;
	Token _ (TkGen "<") -> cont 69;
	Token _ (TkGen ">") -> cont 70;
	Token _ (TkGen "<>") -> cont 71;
	Token _ (TkGen "and") -> cont 72;
	Token _ (TkGen "or") -> cont 73;
	Token _ (TkGen "not") -> cont 74;
	Token _ (TkGen ":=") -> cont 75;
	Token _ (TkGen ";") -> cont 76;
	Token _ (TkGen ":") -> cont 77;
	Token _ (TkGen ",") -> cont 78;
	Token _ (TkGen ".") -> cont 79;
	Token _ (TkGen "(") -> cont 80;
	Token _ (TkGen ")") -> cont 81;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 82 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (thenP)
happyReturn :: () => a -> Parser a
happyReturn = (returnP)
happyThen1 m k tks = (thenP) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (returnP) a
happyError' :: () => ([(Token)], [String]) -> Parser a
happyError' = (\(tokens, _) -> happyError tokens)
happyParser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
