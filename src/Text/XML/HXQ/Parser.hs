{-# OPTIONS -fglasgow-exts -cpp #-}
module Text.XML.HXQ.Parser(Ast(..),scan,parse,call,concatenateAll,ppAst) where
import Char
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.18.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Ast) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Ast)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (String) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (String)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Ast) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Ast)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Ast) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Ast)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Ast) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Ast)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Ast) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Ast)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (( Ast -> Ast, Ast -> Ast )) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (( Ast -> Ast, Ast -> Ast ))
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (( [ Ast ], [ Ast ] )) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (( [ Ast ], [ Ast ] ))
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Ast) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Ast)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Ast) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Ast)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Ast) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Ast)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Ast) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Ast)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Ast]) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([Ast])
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Ast) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Ast)
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Ast -> Ast) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Ast -> Ast)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([ Ast ]) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([ Ast ])
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (String -> Ast -> [ Ast ] -> Ast) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (String -> Ast -> [ Ast ] -> Ast)
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (String -> Ast -> Ast) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (String -> Ast -> Ast)
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (String -> Ast) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (String -> Ast)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xfe\x00\xfe\x00\x5f\x00\xdc\x02\x25\x03\x00\x00\x44\x04\xca\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\xdb\x02\xdb\x02\x53\x01\xa9\x00\x53\x01\x53\x01\x53\x01\x00\x00\xb5\x02\x53\x01\x99\x02\x99\x02\x63\x00\x11\x00\xfb\xff\xac\x02\x4c\x01\x00\x00\x53\x01\xa2\x02\x53\x01\xe0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x02\x53\x01\xa4\x03\x53\x01\x88\x03\xa9\x02\x8d\x02\x41\x00\x00\x00\xd3\x02\xa5\x02\x53\x01\x9a\x02\xd1\x02\xa0\x02\x53\x01\xa8\x02\xa6\x02\x65\x00\xa3\x02\x00\x00\x94\x02\x00\x00\x00\x00\x44\x04\x6e\x00\x67\x00\x00\x00\x9d\x01\x47\x00\xe7\xff\x0c\x00\x53\x01\x00\x00\x15\x00\x00\x00\x9e\x02\x7b\x02\x7b\x02\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x53\x01\x43\x02\x61\x02\x5f\x02\x54\x00\x00\x00\xff\xff\x00\x00\x37\x02\x66\x00\x00\x00\x8c\x00\x8c\x00\x8c\x00\x81\x0a\x81\x0a\x81\x0a\xca\x03\x81\x0a\x28\x04\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x37\x01\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x9e\x00\x99\x0a\x44\x04\x5c\x02\x5b\x02\x84\x02\x53\x02\x00\x00\xfc\xff\x53\x01\x13\x00\xfe\xff\x48\x02\x00\x00\x00\x00\x6a\x00\x47\x02\x00\x00\x53\x01\x51\x00\x2c\x02\x53\x01\x53\x01\x53\x01\x00\x00\x53\x01\x00\x00\x66\x02\x3e\x02\x53\x01\x26\x02\x26\x02\x53\x01\x04\x03\x6d\x02\x53\x01\x3b\x02\xe7\x02\x6b\x02\x53\x01\x0c\x00\x00\x00\x07\x00\x71\x00\x67\x02\x53\x01\x02\x04\x53\x01\x21\x02\x1e\x02\x02\x04\x02\x04\xfa\x01\x8c\x00\x53\x01\x00\x00\x00\x00\x11\x02\x69\x00\x00\x00\x1f\x02\x61\x00\x00\x00\x01\x02\xe6\x03\xdc\x01\xe0\x01\xe6\x03\xf5\x01\x05\x00\xe6\x03\xdf\x01\xe6\x03\xe6\x03\xed\xff\x00\x00\xfb\xff\x2e\x00\x00\x00\x00\x02\x00\x00\x00\x00\xd1\x01\x5a\x00\x00\x00\x53\x01\xad\x01\x00\x00\x00\x00\x53\x01\x53\x01\x0d\x00\x00\x00\x2f\x00\xdd\x01\xdb\x01\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x25\x00\x00\x00\x00\x00\x00\x00\x97\x01\x00\x00\x00\x00\x00\x00\xe6\x03\x46\x03\xb6\x01\xca\x01\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\x00\x00\x99\x01\x53\x01\x54\x01\x53\x01\x00\x00\x05\x00\x53\x01\x53\x01\x53\x01\x00\x00\x53\x01\x00\x00\xe6\x03\x70\x01\x1b\x00\x77\x01\x00\x00\x8c\x00\x9e\x01\x8c\x00\x01\x00\x4b\x01\x53\x01\xfa\xff\xf9\xff\xe6\x03\xe6\x03\x00\x00\xb9\x01\x71\x01\xe6\x03\x94\x01\x00\x00\x94\x01\x00\x00\x00\x00\x53\x01\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x01\x8c\x01\x00\x00\x68\x01\x89\x01\x00\x00\x00\x00\x57\x01\x00\x00\x57\x01\x46\x03\x8a\x01\x53\x01\x00\x00\x00\x00\xe6\x03\x64\x01\x53\x01\x8c\x00\x00\x00\x53\x01\x42\x02\x84\x01\xa4\x02\x8c\x00\xe6\x03\x00\x00\x41\x01\x3f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x53\x01\x00\x00\x85\x01\x53\x01\x87\x02\x00\x00\xe6\x03\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xc2\x00\x17\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x01\x00\x00\x15\x01\x0b\x01\x6f\x0a\xf4\x04\xdd\x04\x58\x0a\x41\x0a\x00\x00\x76\x01\x2a\x0a\x03\x01\xfb\x00\x74\x01\x72\x01\xf4\x00\x00\x00\x00\x00\x00\x00\x13\x0a\x00\x00\xfc\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x09\x00\x00\xce\x09\x00\x00\x6f\x01\x66\x01\x00\x00\x4f\x01\x00\x00\x61\x01\xb7\x09\x00\x00\x00\x00\x5a\x01\xa0\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x01\xc6\x04\x00\x00\x0e\x00\x00\x00\x58\x01\xd9\x00\xdb\x00\x89\x09\x72\x09\x5b\x09\x44\x09\x2d\x09\x16\x09\xff\x08\xe8\x08\xd1\x08\xba\x08\xa3\x08\x8c\x08\x75\x08\x5e\x08\x47\x08\x30\x08\x19\x08\x02\x08\xeb\x07\xd4\x07\xbd\x07\xa6\x07\x8f\x07\x78\x07\x61\x07\x4a\x07\x33\x07\x1c\x07\x05\x07\x00\x00\x00\x00\x00\x00\xaf\x04\x00\x00\x5d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x7b\x01\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x01\x98\x04\x56\x01\x4e\x01\x49\x01\x00\x00\x00\x00\x00\x00\x21\x01\x00\x00\xee\x06\xe8\x00\x30\x01\xd7\x06\xc0\x06\xa9\x06\x00\x00\x92\x06\x00\x00\x00\x00\x33\x01\x7b\x06\x29\x01\x27\x01\x64\x06\x00\x00\x00\x00\x81\x04\x00\x00\x00\x00\x00\x00\x6a\x04\x01\x01\x00\x00\x23\x01\x00\x00\x00\x00\x4d\x06\x00\x00\x36\x06\x00\x00\x0d\x01\x00\x00\x00\x00\x98\x00\x55\x01\x1f\x06\x00\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\xd4\x00\xc9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x03\x07\x01\x00\x00\x00\x00\x2f\x03\x08\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\x00\xb2\x00\xd0\x00\x00\x00\xf1\x05\xe1\x00\xda\x05\x00\x00\xdf\x00\xc3\x05\xac\x05\xcf\x02\x00\x00\x69\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x00\x00\x38\x01\x00\x00\xd2\x00\x95\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x7e\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x75\x00\x96\x00\x00\x00\x67\x05\x00\x00\x00\x00\x00\x00\x00\x00\x50\x05\xc3\x00\x00\x00\x39\x05\x0b\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x05\x00\x00\x00\x00\x0b\x05\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xfd\xff\x6c\xff\x69\xff\xf9\xff\x9d\xff\xd4\xff\xd5\xff\x00\x00\xb0\xff\x82\xff\xd6\xff\x6f\xff\x6e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xaf\xff\xae\xff\x81\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\xff\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa7\xff\x00\x00\xa8\xff\xb1\xff\x8c\xff\xb2\xff\xb3\xff\xaa\xff\x00\x00\x00\x00\x66\xff\x00\x00\x00\x00\x00\x00\x7b\xff\x00\x00\x7f\xff\x00\x00\x91\xff\x9b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\xfe\xff\xfb\xff\x00\x00\x64\xff\x00\x00\x00\x00\x00\x00\xb8\xff\xb9\xff\xba\xff\xbb\xff\xbc\xff\xbd\xff\xbe\xff\xbf\xff\xc0\xff\xc1\xff\xc2\xff\xc3\xff\xc4\xff\xc5\xff\xc6\xff\xc7\xff\xc8\xff\xc9\xff\xca\xff\xcb\xff\xcc\xff\xcd\xff\xce\xff\xcf\xff\xd0\xff\xd1\xff\xd2\xff\xd3\xff\x9e\xff\xa5\xff\xa6\xff\x00\x00\x00\x00\x87\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\xff\x89\xff\x00\x00\x79\xff\x77\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\xff\x00\x00\x80\xff\x00\x00\x8b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\xff\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\xab\xff\xad\xff\x00\x00\x00\x00\x00\x00\x6b\xff\x6a\xff\x78\xff\x00\x00\x94\xff\x00\x00\x00\x00\x95\xff\x00\x00\xa0\xff\x00\x00\x00\x00\xa4\xff\x00\x00\x00\x00\xa9\xff\x00\x00\xd8\xff\xd9\xff\x00\x00\x6f\xff\x00\x00\x00\x00\x71\xff\x00\x00\x76\xff\x7e\xff\x00\x00\x00\x00\x83\xff\x00\x00\x00\x00\x84\xff\x85\xff\x00\x00\x00\x00\xee\xff\xb7\xff\xea\xff\x00\x00\x00\x00\xb6\xff\xb5\xff\x65\xff\xfa\xff\x00\x00\x00\x00\xe9\xff\xe8\xff\xe7\xff\x00\x00\xed\xff\xec\xff\xeb\xff\xda\xff\x96\xff\x9c\xff\x00\x00\x00\x00\x8a\xff\x92\xff\x70\xff\x6f\xff\x00\x00\x6f\xff\x75\xff\x00\x00\x00\x00\x00\x00\x7d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8e\xff\x00\x00\x8d\xff\xf8\xff\x00\x00\x00\x00\xf2\xff\xb4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\xff\xa3\xff\x7c\xff\xd7\xff\x00\x00\xa2\xff\x74\xff\x6f\xff\x73\xff\x86\xff\x93\xff\x00\x00\x9a\xff\x98\xff\x97\xff\xe4\xff\xe0\xff\x00\x00\xe6\xff\xe1\xff\x00\x00\xe5\xff\xe2\xff\x00\x00\xe3\xff\x00\x00\x96\xff\x72\xff\x00\x00\x90\xff\x8f\xff\xf7\xff\xf0\xff\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\xff\x99\xff\xde\xff\xdf\xff\xdd\xff\xdc\xff\xef\xff\xf6\xff\x00\x00\x62\xff\x00\x00\x00\x00\x00\x00\xf5\xff\x63\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x03\x00\x04\x00\x09\x00\x0c\x00\x0c\x00\x0b\x00\x09\x00\x0b\x00\x0b\x00\x10\x00\x0b\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0b\x00\x16\x00\x07\x00\x2d\x00\x16\x00\x09\x00\x10\x00\x2b\x00\x03\x00\x09\x00\x0e\x00\x0b\x00\x10\x00\x0b\x00\x38\x00\x0b\x00\x13\x00\x14\x00\x15\x00\x25\x00\x0a\x00\x2d\x00\x2d\x00\x29\x00\x2a\x00\x1f\x00\x16\x00\x3f\x00\x2c\x00\x18\x00\x0a\x00\x34\x00\x35\x00\x36\x00\x34\x00\x35\x00\x10\x00\x3b\x00\x09\x00\x39\x00\x3b\x00\x3b\x00\x3c\x00\x41\x00\x0e\x00\x10\x00\x10\x00\x41\x00\x0a\x00\x3b\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x10\x00\x2d\x00\x34\x00\x35\x00\x36\x00\x3b\x00\x52\x00\x3b\x00\x53\x00\x51\x00\x52\x00\x56\x00\x54\x00\x55\x00\x56\x00\x02\x00\x03\x00\x04\x00\x4c\x00\x09\x00\x56\x00\x56\x00\x09\x00\x0a\x00\x0b\x00\x3b\x00\x10\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0c\x00\x39\x00\x53\x00\x3b\x00\x16\x00\x56\x00\x09\x00\x0c\x00\x0b\x00\x41\x00\x0a\x00\x0a\x00\x3b\x00\x03\x00\x2d\x00\x0c\x00\x0c\x00\x07\x00\x03\x00\x25\x00\x0c\x00\x4c\x00\x3b\x00\x29\x00\x2a\x00\x38\x00\x52\x00\x03\x00\x03\x00\x18\x00\x43\x00\x07\x00\x2d\x00\x2d\x00\x34\x00\x35\x00\x39\x00\x3a\x00\x3b\x00\x39\x00\x2d\x00\x3b\x00\x2b\x00\x40\x00\x41\x00\x2d\x00\x2d\x00\x41\x00\x2d\x00\x2d\x00\x44\x00\x45\x00\x46\x00\x2d\x00\x04\x00\x2c\x00\x3b\x00\x2e\x00\x08\x00\x42\x00\x37\x00\x52\x00\x3f\x00\x51\x00\x52\x00\x11\x00\x54\x00\x55\x00\x56\x00\x02\x00\x03\x00\x04\x00\x10\x00\x11\x00\x12\x00\x13\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x57\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x03\x00\x1c\x00\x05\x00\x06\x00\x16\x00\x34\x00\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x3b\x00\x05\x00\x06\x00\x08\x00\x09\x00\x03\x00\x0b\x00\x25\x00\x1d\x00\x1e\x00\x08\x00\x29\x00\x2a\x00\x12\x00\x13\x00\x14\x00\x03\x00\x16\x00\x17\x00\x08\x00\x19\x00\x08\x00\x34\x00\x35\x00\x1d\x00\x1e\x00\x08\x00\x39\x00\x08\x00\x3b\x00\x0c\x00\x1d\x00\x1e\x00\x0d\x00\x08\x00\x41\x00\x03\x00\x1c\x00\x44\x00\x45\x00\x46\x00\x08\x00\x1d\x00\x1e\x00\x29\x00\x2a\x00\x16\x00\x17\x00\x03\x00\x1c\x00\x2f\x00\x51\x00\x52\x00\x08\x00\x54\x00\x55\x00\x56\x00\x02\x00\x03\x00\x04\x00\x08\x00\x11\x00\x1d\x00\x1e\x00\x09\x00\x0d\x00\x0b\x00\x03\x00\x08\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x1b\x00\x1d\x00\x1e\x00\x08\x00\x16\x00\x16\x00\x17\x00\x0c\x00\x01\x00\x02\x00\x03\x00\x1a\x00\x1b\x00\x08\x00\x1c\x00\x08\x00\x09\x00\x0c\x00\x0b\x00\x25\x00\x13\x00\x14\x00\x03\x00\x29\x00\x2a\x00\x12\x00\x13\x00\x14\x00\x1f\x00\x16\x00\x17\x00\x08\x00\x19\x00\x08\x00\x34\x00\x35\x00\x1d\x00\x1e\x00\x03\x00\x39\x00\x08\x00\x3b\x00\x3c\x00\x03\x00\x1b\x00\x05\x00\x06\x00\x41\x00\x1a\x00\x1b\x00\x44\x00\x45\x00\x46\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x03\x00\x03\x00\x05\x00\x06\x00\x51\x00\x52\x00\x03\x00\x54\x00\x55\x00\x56\x00\x02\x00\x03\x00\x04\x00\x03\x00\x03\x00\x05\x00\x06\x00\x09\x00\x03\x00\x0b\x00\x02\x00\x03\x00\x0e\x00\x0f\x00\x10\x00\x03\x00\x08\x00\x09\x00\x0f\x00\x0b\x00\x16\x00\x03\x00\x1c\x00\x05\x00\x06\x00\x08\x00\x12\x00\x13\x00\x14\x00\x03\x00\x16\x00\x17\x00\x03\x00\x19\x00\x03\x00\x25\x00\x03\x00\x1d\x00\x1e\x00\x29\x00\x2a\x00\x03\x00\x1c\x00\x05\x00\x06\x00\x03\x00\x0e\x00\x05\x00\x06\x00\x01\x00\x34\x00\x35\x00\x3d\x00\x3e\x00\x4c\x00\x39\x00\x4c\x00\x3b\x00\x0b\x00\x2c\x00\x07\x00\x3b\x00\x0a\x00\x41\x00\x2d\x00\x0a\x00\x44\x00\x45\x00\x46\x00\x2d\x00\x07\x00\x2b\x00\x52\x00\x2e\x00\x01\x00\x07\x00\x0a\x00\x05\x00\x2c\x00\x51\x00\x52\x00\x52\x00\x54\x00\x55\x00\x56\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x18\x00\x2d\x00\x09\x00\x06\x00\x09\x00\x4a\x00\x3b\x00\x18\x00\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x08\x00\x14\x00\x2e\x00\x2b\x00\x0b\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x4a\x00\x0b\x00\x0a\x00\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x4a\x00\x3a\x00\x52\x00\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x4a\x00\x03\x00\x4f\x00\x4d\x00\x4e\x00\x09\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0a\x00\x3b\x00\x0a\x00\x52\x00\x3b\x00\x14\x00\x12\x00\x13\x00\x14\x00\x52\x00\x16\x00\x17\x00\x3a\x00\x19\x00\x3b\x00\x31\x00\x01\x00\x1d\x00\x1e\x00\x2d\x00\x2d\x00\x53\x00\x2c\x00\x4a\x00\x2c\x00\x4b\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x52\x00\x30\x00\x3b\x00\x2d\x00\x4a\x00\x03\x00\x2d\x00\x4d\x00\x4e\x00\x2e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x3b\x00\x0b\x00\x43\x00\x0b\x00\x52\x00\x3b\x00\x12\x00\x13\x00\x14\x00\x3b\x00\x16\x00\x17\x00\x58\x00\x19\x00\x09\x00\x48\x00\x52\x00\x1d\x00\x1e\x00\x4a\x00\x43\x00\x3b\x00\x4d\x00\x4e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x52\x00\x09\x00\x53\x00\xff\xff\x4a\x00\x03\x00\xff\xff\x4d\x00\x4e\x00\xff\xff\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x32\x00\x33\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x47\x00\xff\xff\xff\xff\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x49\x00\x4a\x00\x25\x00\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x4a\x00\x25\x00\xff\xff\x4d\x00\x4e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x03\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x08\x00\x09\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x11\x00\x12\x00\x13\x00\x14\x00\x44\x01\x45\x01\xeb\x00\x14\x00\x36\x00\x15\x00\x18\x00\x48\x01\x16\x00\x17\x00\x18\x00\x15\x00\x4b\x00\x19\x00\xa8\x00\xaa\x00\x19\x00\xff\x00\xce\x00\x0f\x01\xb0\x00\x35\x00\x00\x01\x36\x00\x01\x01\x3a\x00\xab\x00\x9f\x00\x9c\x00\x0a\x00\x9d\x00\x1a\x00\x22\x01\xaf\x00\xaf\x00\x1b\x00\x1c\x00\x58\x01\x19\x00\x10\x01\x49\x01\xb1\x00\x39\x01\xa0\x00\xa1\x00\xec\x00\x1d\x00\x1e\x00\x3a\x01\x20\x00\x14\x00\x1f\x00\x37\x00\x20\x00\x21\x00\x22\x00\xfc\x00\x18\x00\xfd\x00\x22\x00\x3c\x01\x37\x00\x23\x00\x24\x00\x25\x00\xa9\x00\x3a\x01\x23\x01\xa0\x00\xa1\x00\xa2\x00\x37\x00\x27\x00\x37\x00\xed\x00\x26\x00\x27\x00\xee\x00\x28\x00\x29\x00\x2a\x00\x11\x00\x12\x00\x13\x00\x02\x01\x14\x00\x2a\x00\x4c\x00\x14\x00\x78\x00\x15\x00\x37\x00\x18\x00\x16\x00\x17\x00\x18\x00\x30\x01\x08\x01\x0c\x01\xa3\x00\x20\x00\x19\x00\xa4\x00\x39\x00\x17\x01\x3a\x00\x22\x00\xf8\x00\xae\x00\x37\x00\x36\x01\xaa\x00\x19\x01\xe7\x00\x37\x01\x51\x01\x1a\x00\xb0\x00\xfe\x00\xbf\x00\x1b\x00\x1c\x00\xac\x00\x27\x00\x36\x01\x52\x01\x4d\x00\xc0\x00\x3a\x01\xaf\x00\xaf\x00\x1d\x00\x1e\x00\xe2\x00\xe3\x00\x20\x00\x1f\x00\xaf\x00\x20\x00\xb3\x00\xe4\x00\x22\x00\xaf\x00\xaf\x00\x22\x00\xaf\x00\xaf\x00\x23\x00\x24\x00\x25\x00\xaf\x00\x1b\x01\xcb\x00\x37\x00\xcc\x00\x1c\x01\x74\x00\x4e\x00\x27\x00\xb4\x00\x26\x00\x27\x00\x50\x01\x28\x00\x29\x00\x2a\x00\x11\x00\x12\x00\x13\x00\x56\x00\x57\x00\x58\x00\x59\x00\x14\x00\x46\x00\x15\x00\x04\x00\x75\x00\x16\x00\x17\x00\x18\x00\x05\x00\xf0\x00\x41\x01\x55\x01\xf2\x00\x19\x00\xf4\x00\xf5\x00\x2a\x00\x02\x00\x03\x00\x04\x00\xf0\x00\x37\x00\x4c\x01\xf2\x00\x05\x00\x06\x00\x04\x00\x07\x00\x1a\x00\x2d\x01\x0f\x00\x05\x00\x1b\x00\x1c\x00\x08\x00\x09\x00\x0a\x00\x04\x00\x0b\x00\x0c\x00\x46\x01\x0d\x00\x05\x00\x1d\x00\x1e\x00\x0e\x00\x0f\x00\x3c\x00\x1f\x00\x3a\x00\x20\x00\x99\x00\x0a\x01\x0f\x00\x98\x00\x2a\x01\x22\x00\x04\x00\x2c\x01\x23\x00\x24\x00\x25\x00\x05\x00\x0c\x01\x0f\x00\x50\x00\x51\x00\x28\x01\x0c\x00\x04\x00\x2e\x01\x52\x00\x26\x00\x27\x00\x05\x00\x28\x00\x29\x00\x2a\x00\x11\x00\x12\x00\x13\x00\x3a\x00\x32\x01\xe0\x00\x0f\x00\x14\x00\x3b\x00\x15\x00\x05\x01\x3c\x00\x16\x00\x17\x00\x18\x00\x3d\x00\xe5\x00\x32\x00\x0f\x00\x3c\x00\x19\x00\x11\x01\x0c\x00\x47\x00\x02\x00\x03\x00\x04\x00\xce\x00\xa6\x00\x3c\x00\x0d\x01\x05\x00\x06\x00\x48\x00\x07\x00\x1a\x00\xe9\x00\x0a\x00\xcc\x00\x1b\x00\x1c\x00\x08\x00\x09\x00\x0a\x00\x1d\x01\x0b\x00\x0c\x00\xd6\x00\x0d\x00\xd7\x00\x1d\x00\x1e\x00\x0e\x00\x0f\x00\xd9\x00\x1f\x00\xdf\x00\x20\x00\x21\x00\xf0\x00\xe5\x00\x49\x01\xf2\x00\x22\x00\xa5\x00\xa6\x00\x23\x00\x24\x00\x25\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\xf0\x00\xe7\x00\x1f\x01\xf2\x00\x26\x00\x27\x00\x33\x00\x28\x00\x29\x00\x2a\x00\x11\x00\x12\x00\x13\x00\xf0\x00\x37\x00\x1a\x01\xf2\x00\x14\x00\xb7\x00\x15\x00\x75\x00\x04\x00\x16\x00\x17\x00\x18\x00\xbb\x00\x05\x00\x06\x00\x9a\x00\x07\x00\x19\x00\xf0\x00\xbd\x00\xf1\x00\xf2\x00\xc0\x00\x08\x00\x09\x00\x0a\x00\xc1\x00\x0b\x00\x0c\x00\x33\x00\x0d\x00\x37\x00\x1a\x00\x3f\x00\x0e\x00\x0f\x00\x1b\x00\x1c\x00\xf0\x00\x49\x00\xf5\x00\xf2\x00\xf0\x00\x4e\x00\xf6\x00\xf2\x00\x5b\x01\x1d\x00\x1e\x00\x30\x00\x31\x00\x54\x01\x1f\x00\x55\x01\x20\x00\x58\x01\x4f\x01\xa8\x00\x37\x00\x3d\x01\x22\x00\x3e\x01\x3f\x01\x23\x00\x24\x00\x25\x00\x40\x01\xa8\x00\x43\x01\x27\x00\x24\x01\x4b\x01\xa8\x00\x36\x01\xad\x00\x21\x01\x26\x00\x27\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x31\x01\x32\x01\xfa\x00\x11\x01\xfb\x00\x6f\x00\x37\x00\x09\x01\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x0a\x01\x13\x01\x15\x01\x14\x01\x16\x01\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x18\x01\xc7\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\xa9\x00\x27\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x04\x00\x1f\x01\x70\x00\x71\x00\xca\x00\x05\x00\x42\x00\x24\x01\x07\x00\xd1\x00\xbf\x00\xd4\x00\x27\x00\x37\x00\xdb\x00\x08\x00\x09\x00\x0a\x00\x27\x00\x0b\x00\x0c\x00\xa9\x00\x0d\x00\x37\x00\xef\x00\xf0\x00\x0e\x00\x0f\x00\xaa\x00\xb5\x00\xf9\x00\x79\x00\x6f\x00\x7a\x00\x7b\x00\x70\x00\x71\x00\x1f\x01\x5a\x01\x5d\x01\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x57\x01\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x27\x00\x9c\x00\x37\x00\xaa\x00\x6f\x00\x04\x00\xb5\x00\x70\x00\x71\x00\xb6\x00\x05\x00\x42\x00\x25\x01\x07\x00\x37\x00\xb9\x00\xba\x00\xbd\x00\x27\x00\x37\x00\x08\x00\x09\x00\x0a\x00\x37\x00\x0b\x00\x0c\x00\xff\xff\x0d\x00\x2c\x00\x2e\x00\x27\x00\x0e\x00\x0f\x00\x6f\x00\x32\x00\x37\x00\x70\x00\x71\x00\xd2\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\xd5\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x27\x00\x72\x00\x73\x00\x00\x00\x6f\x00\x04\x00\x00\x00\x70\x00\x71\x00\x00\x00\x05\x00\x03\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x42\x00\x06\x01\x07\x00\x34\x01\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x00\x00\x00\x00\xc3\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\xc5\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x42\x00\xcf\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\xd2\x00\x07\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x70\x00\x71\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\xe8\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\x76\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\xa4\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\x43\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x42\x00\x44\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x5d\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x5b\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x4b\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x4d\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x4f\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x40\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x45\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x26\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x27\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x29\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x2b\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x02\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x19\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xc7\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xc8\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xd5\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xd8\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xdb\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xdc\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xdd\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xde\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xe4\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x7b\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x7c\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x7d\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x7e\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x7f\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x80\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x81\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x82\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x83\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x84\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x85\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x86\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x87\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x88\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x89\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8a\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8b\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8c\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8d\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8e\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x8f\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x90\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x91\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x92\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x93\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x94\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x95\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x96\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x97\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xb6\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xba\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xc3\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\xc5\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x2c\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x2e\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x3e\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x40\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x41\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x04\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x05\x00\x46\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 157) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157)
	]

happy_n_terms = 89 :: Int
happy_n_nonterms = 32 :: Int

happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (happy_var_1
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_3 = happySpecReduce_2  1# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_1++[happy_var_3]
	)}}

happyReduce_5 = happyReduce 4# 1# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_1++[happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_6 = happySpecReduce_1  2# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_7 = happyReduce 5# 2# happyReduction_7
happyReduction_7 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn6
		 (Ast "variable" [happy_var_3,happy_var_5]
	) `HappyStk` happyRest}}

happyReduce_8 = happyReduce 7# 2# happyReduction_8
happyReduction_8 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_7 of { happy_var_7 -> 
	happyIn6
		 (Ast "variable" [happy_var_3,happy_var_7]
	) `HappyStk` happyRest}}

happyReduce_9 = happyReduce 9# 2# happyReduction_9
happyReduction_9 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	case happyOut8 happy_x_5 of { happy_var_5 -> 
	case happyOut13 happy_x_8 of { happy_var_8 -> 
	happyIn6
		 (Ast "function" ([Avar happy_var_3,happy_var_8]++happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_10 = happyReduce 11# 2# happyReduction_10
happyReduction_10 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	case happyOut8 happy_x_5 of { happy_var_5 -> 
	case happyOut13 happy_x_10 of { happy_var_10 -> 
	happyIn6
		 (Ast "function" ([Avar happy_var_3,happy_var_10]++happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_11 = happySpecReduce_1  3# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (QName happy_var_1) -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_12 = happySpecReduce_3  3# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (QName happy_var_1) -> 
	case happyOutTok happy_x_3 of { (QName happy_var_3) -> 
	happyIn7
		 (happy_var_1++":"++happy_var_3
	)}}

happyReduce_13 = happySpecReduce_1  4# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ([happy_var_1]
	)}

happyReduce_14 = happySpecReduce_3  4# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ([happy_var_1]
	)}

happyReduce_15 = happySpecReduce_3  4# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (happy_var_1++[happy_var_3]
	)}}

happyReduce_16 = happyReduce 5# 4# happyReduction_16
happyReduction_16 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (happy_var_1++[happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_17 = happySpecReduce_1  5# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Avar happy_var_1
	)}

happyReduce_18 = happySpecReduce_2  5# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "+" [Avar happy_var_1]
	)}

happyReduce_19 = happySpecReduce_2  5# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "*" [Avar happy_var_1]
	)}

happyReduce_20 = happySpecReduce_2  5# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "?" [Avar happy_var_1]
	)}

happyReduce_21 = happySpecReduce_1  5# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (happy_var_1
	)}

happyReduce_22 = happySpecReduce_2  5# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "+" [happy_var_1]
	)}

happyReduce_23 = happySpecReduce_2  5# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "*" [happy_var_1]
	)}

happyReduce_24 = happySpecReduce_2  5# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Ast "?" [happy_var_1]
	)}

happyReduce_25 = happySpecReduce_3  6# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn10
		 (Ast "element" []
	)

happyReduce_26 = happySpecReduce_3  6# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn10
		 (Ast "attribute" []
	)

happyReduce_27 = happySpecReduce_3  6# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Ast happy_var_1 []
	)}

happyReduce_28 = happyReduce 4# 6# happyReduction_28
happyReduction_28 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (Ast "element" happy_var_3
	) `HappyStk` happyRest}

happyReduce_29 = happyReduce 4# 6# happyReduction_29
happyReduction_29 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (Ast "attribute" happy_var_3
	) `HappyStk` happyRest}

happyReduce_30 = happySpecReduce_1  7# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn11
		 ([Avar "*"]
	)

happyReduce_31 = happySpecReduce_1  7# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ([Avar happy_var_1]
	)}

happyReduce_32 = happySpecReduce_3  7# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ([Avar "*",Avar happy_var_3]
	)}

happyReduce_33 = happySpecReduce_3  7# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ([Avar happy_var_1,Avar happy_var_3]
	)}}

happyReduce_34 = happyReduce 4# 7# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ([Avar "*",Ast "?" [Avar happy_var_3]]
	) `HappyStk` happyRest}

happyReduce_35 = happyReduce 4# 7# happyReduction_35
happyReduction_35 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ([Avar happy_var_1,Ast "?" [Avar happy_var_3]]
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_1  8# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Variable happy_var_1) -> 
	happyIn12
		 (Avar happy_var_1
	)}

happyReduce_37 = happyReduce 5# 9# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 ((snd happy_var_3) (happy_var_1 (happy_var_2 ((fst happy_var_3) happy_var_5)))
	) `HappyStk` happyRest}}}}

happyReduce_38 = happyReduce 4# 9# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (call "some" [happy_var_2 happy_var_4]
	) `HappyStk` happyRest}}

happyReduce_39 = happyReduce 4# 9# happyReduction_39
happyReduction_39 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (call "not" [call "some" [happy_var_2 (call "not" [happy_var_4])]]
	) `HappyStk` happyRest}}

happyReduce_40 = happyReduce 6# 9# happyReduction_40
happyReduction_40 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_6 of { happy_var_6 -> 
	happyIn13
		 (Ast "if" [happy_var_2,happy_var_4,happy_var_6]
	) `HappyStk` happyRest}}}

happyReduce_41 = happySpecReduce_1  9# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  9# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  9# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  9# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "to" [happy_var_1,happy_var_3]
	)}}

happyReduce_45 = happySpecReduce_3  9# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "+" [happy_var_1,happy_var_3]
	)}}

happyReduce_46 = happySpecReduce_3  9# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "-" [happy_var_1,happy_var_3]
	)}}

happyReduce_47 = happySpecReduce_3  9# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "*" [happy_var_1,happy_var_3]
	)}}

happyReduce_48 = happySpecReduce_3  9# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "div" [happy_var_1,happy_var_3]
	)}}

happyReduce_49 = happySpecReduce_3  9# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "idiv" [happy_var_1,happy_var_3]
	)}}

happyReduce_50 = happySpecReduce_3  9# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "mod" [happy_var_1,happy_var_3]
	)}}

happyReduce_51 = happySpecReduce_3  9# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "=" [happy_var_1,happy_var_3]
	)}}

happyReduce_52 = happySpecReduce_3  9# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "!=" [happy_var_1,happy_var_3]
	)}}

happyReduce_53 = happySpecReduce_3  9# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "<" [happy_var_1,happy_var_3]
	)}}

happyReduce_54 = happySpecReduce_3  9# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "<=" [happy_var_1,happy_var_3]
	)}}

happyReduce_55 = happySpecReduce_3  9# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call ">" [happy_var_1,happy_var_3]
	)}}

happyReduce_56 = happySpecReduce_3  9# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call ">=" [happy_var_1,happy_var_3]
	)}}

happyReduce_57 = happySpecReduce_3  9# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "<<" [happy_var_1,happy_var_3]
	)}}

happyReduce_58 = happySpecReduce_3  9# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call ">>" [happy_var_1,happy_var_3]
	)}}

happyReduce_59 = happySpecReduce_3  9# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "is" [happy_var_1,happy_var_3]
	)}}

happyReduce_60 = happySpecReduce_3  9# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "eq" [happy_var_1,happy_var_3]
	)}}

happyReduce_61 = happySpecReduce_3  9# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "ne" [happy_var_1,happy_var_3]
	)}}

happyReduce_62 = happySpecReduce_3  9# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "lt" [happy_var_1,happy_var_3]
	)}}

happyReduce_63 = happySpecReduce_3  9# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "le" [happy_var_1,happy_var_3]
	)}}

happyReduce_64 = happySpecReduce_3  9# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "gt" [happy_var_1,happy_var_3]
	)}}

happyReduce_65 = happySpecReduce_3  9# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "ge" [happy_var_1,happy_var_3]
	)}}

happyReduce_66 = happySpecReduce_3  9# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "and" [happy_var_1,happy_var_3]
	)}}

happyReduce_67 = happySpecReduce_3  9# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "or" [happy_var_1,happy_var_3]
	)}}

happyReduce_68 = happySpecReduce_3  9# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "not" [happy_var_1,happy_var_3]
	)}}

happyReduce_69 = happySpecReduce_3  9# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "union" [happy_var_1,happy_var_3]
	)}}

happyReduce_70 = happySpecReduce_3  9# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "intersect" [happy_var_1,happy_var_3]
	)}}

happyReduce_71 = happySpecReduce_3  9# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (call "except" [happy_var_1,happy_var_3]
	)}}

happyReduce_72 = happyReduce 4# 9# happyReduction_72
happyReduction_72 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (call "instance-of" [happy_var_1,Ast "type" [happy_var_4]]
	) `HappyStk` happyRest}}

happyReduce_73 = happyReduce 4# 9# happyReduction_73
happyReduction_73 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (call "cast-as" [happy_var_1,Ast "type" [happy_var_4]]
	) `HappyStk` happyRest}}

happyReduce_74 = happyReduce 4# 9# happyReduction_74
happyReduction_74 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (call "castable-as" [happy_var_1,Ast "type" [happy_var_4]]
	) `HappyStk` happyRest}}

happyReduce_75 = happyReduce 5# 9# happyReduction_75
happyReduction_75 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 (let v = "_tc" in Ast "let" [Avar v,happy_var_3,happy_var_5 v]
	) `HappyStk` happyRest}}

happyReduce_76 = happySpecReduce_2  9# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (call "uplus" [happy_var_2]
	)}

happyReduce_77 = happySpecReduce_2  9# happyReduction_77
happyReduction_77 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (call "uminus" [happy_var_2]
	)}

happyReduce_78 = happySpecReduce_2  9# happyReduction_78
happyReduction_78 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (call "not" [happy_var_2]
	)}

happyReduce_79 = happySpecReduce_1  9# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  9# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TInteger happy_var_1) -> 
	happyIn13
		 (Aint happy_var_1
	)}

happyReduce_81 = happySpecReduce_1  9# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TFloat happy_var_1) -> 
	happyIn13
		 (Afloat happy_var_1
	)}

happyReduce_82 = happyReduce 4# 9# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (Ast "insert" [happy_var_2,Ast "destination" [happy_var_4]]
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_3  9# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (Ast "delete" [happy_var_3]
	)}

happyReduce_84 = happyReduce 4# 9# happyReduction_84
happyReduction_84 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (Ast "replace" [happy_var_2,happy_var_4]
	) `HappyStk` happyRest}}

happyReduce_85 = happySpecReduce_1  10# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_86 = happySpecReduce_3  10# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_1++[happy_var_3]
	)}}

happyReduce_87 = happySpecReduce_2  11# happyReduction_87
happyReduction_87 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_88 = happySpecReduce_2  11# happyReduction_88
happyReduction_88 happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_89 = happySpecReduce_3  11# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (happy_var_1 . happy_var_3
	)}}

happyReduce_90 = happySpecReduce_3  11# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (happy_var_1 . happy_var_3
	)}}

happyReduce_91 = happySpecReduce_3  12# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (\x -> Ast "for" [happy_var_1,Avar "$",happy_var_3,x]
	)}}

happyReduce_92 = happyReduce 5# 12# happyReduction_92
happyReduction_92 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn16
		 (\x -> Ast "for" [happy_var_1,happy_var_3,happy_var_5,x]
	) `HappyStk` happyRest}}}

happyReduce_93 = happyReduce 5# 12# happyReduction_93
happyReduction_93 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn16
		 (\x -> happy_var_1(Ast "for" [happy_var_3,Avar "$",happy_var_5,x])
	) `HappyStk` happyRest}}}

happyReduce_94 = happyReduce 7# 12# happyReduction_94
happyReduction_94 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	case happyOut13 happy_x_7 of { happy_var_7 -> 
	happyIn16
		 (\x -> happy_var_1(Ast "for" [happy_var_3,happy_var_5,happy_var_7,x])
	) `HappyStk` happyRest}}}}

happyReduce_95 = happySpecReduce_3  13# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (\x -> Ast "let" [happy_var_1,happy_var_3,x]
	)}}

happyReduce_96 = happyReduce 5# 13# happyReduction_96
happyReduction_96 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn17
		 (\x -> happy_var_1(Ast "let" [happy_var_3,happy_var_5,x])
	) `HappyStk` happyRest}}}

happyReduce_97 = happySpecReduce_2  14# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (\x -> Ast "predicate" [happy_var_2,x]
	)}

happyReduce_98 = happySpecReduce_0  14# happyReduction_98
happyReduction_98  =  happyIn18
		 (id
	)

happyReduce_99 = happySpecReduce_3  15# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ((\x -> Ast "sortTuple" (x:(fst happy_var_3)),
                                                           \x -> Ast "sort" (x:(snd happy_var_3)))
	)}

happyReduce_100 = happySpecReduce_0  15# happyReduction_100
happyReduction_100  =  happyIn19
		 ((id,id)
	)

happyReduce_101 = happySpecReduce_2  16# happyReduction_101
happyReduction_101 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (([happy_var_1],[happy_var_2])
	)}}

happyReduce_102 = happyReduce 4# 16# happyReduction_102
happyReduction_102 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (((fst happy_var_1)++[happy_var_3],(snd happy_var_1)++[happy_var_4])
	) `HappyStk` happyRest}}}

happyReduce_103 = happySpecReduce_1  17# happyReduction_103
happyReduction_103 happy_x_1
	 =  happyIn21
		 (Avar "ascending"
	)

happyReduce_104 = happySpecReduce_1  17# happyReduction_104
happyReduction_104 happy_x_1
	 =  happyIn21
		 (Avar "descending"
	)

happyReduce_105 = happySpecReduce_0  17# happyReduction_105
happyReduction_105  =  happyIn21
		 (Avar "ascending"
	)

happyReduce_106 = happyReduce 4# 18# happyReduction_106
happyReduction_106 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (call "element" [Avar happy_var_3]
	) `HappyStk` happyRest}

happyReduce_107 = happyReduce 4# 18# happyReduction_107
happyReduction_107 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (call "attribute" [Avar happy_var_3]
	) `HappyStk` happyRest}

happyReduce_108 = happyReduce 6# 19# happyReduction_108
happyReduction_108 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_5 of { happy_var_5 -> 
	happyIn23
		 (if head happy_var_1 == Astring happy_var_5
                                                             then Ast "element_construction" (happy_var_1++[Ast "append" happy_var_3])
                                                          else parseError [TError ("Unmatched tags in element construction: "
                                                                                   ++(show (head happy_var_1))++" '"++happy_var_5++"'")]
	) `HappyStk` happyRest}}}

happyReduce_109 = happyReduce 5# 19# happyReduction_109
happyReduction_109 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (if head happy_var_1 == Astring happy_var_4
                                                             then Ast "element_construction" (happy_var_1++[Ast "append" []])
                                                          else parseError [TError ("Unmatched tags in element construction: "
                                                                                   ++(show (head happy_var_1))++" '"++happy_var_4++"'")]
	) `HappyStk` happyRest}}

happyReduce_110 = happySpecReduce_2  19# happyReduction_110
happyReduction_110 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (Ast "element_construction" (happy_var_1++[Ast "append" []])
	)}

happyReduce_111 = happyReduce 7# 19# happyReduction_111
happyReduction_111 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_6 of { happy_var_6 -> 
	happyIn23
		 (Ast "element_construction" [happy_var_3,Ast "attributes" [],concatenateAll happy_var_6]
	) `HappyStk` happyRest}}

happyReduce_112 = happyReduce 7# 19# happyReduction_112
happyReduction_112 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_6 of { happy_var_6 -> 
	happyIn23
		 (Ast "attribute_construction" [happy_var_3,concatenateAll happy_var_6]
	) `HappyStk` happyRest}}

happyReduce_113 = happyReduce 5# 19# happyReduction_113
happyReduction_113 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (Ast "element_construction" [Astring happy_var_2,Ast "attributes" [],concatenateAll happy_var_4]
	) `HappyStk` happyRest}}

happyReduce_114 = happyReduce 5# 19# happyReduction_114
happyReduction_114 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (Ast "attribute_construction" [Astring happy_var_2,concatenateAll happy_var_4]
	) `HappyStk` happyRest}}

happyReduce_115 = happySpecReduce_2  20# happyReduction_115
happyReduction_115 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 ([Astring happy_var_2,Ast "attributes" []]
	)}

happyReduce_116 = happySpecReduce_3  20# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ([Astring happy_var_2,Ast "attributes" happy_var_3]
	)}}

happyReduce_117 = happySpecReduce_3  21# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ([concatenateAll happy_var_2]
	)}

happyReduce_118 = happySpecReduce_1  21# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TString happy_var_1) -> 
	happyIn25
		 ([Astring happy_var_1]
	)}

happyReduce_119 = happySpecReduce_1  21# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { (XMLtext happy_var_1) -> 
	happyIn25
		 ([Astring happy_var_1]
	)}

happyReduce_120 = happySpecReduce_1  21# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ([happy_var_1]
	)}

happyReduce_121 = happyReduce 4# 21# happyReduction_121
happyReduction_121 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (happy_var_1++[concatenateAll happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_122 = happySpecReduce_2  21# happyReduction_122
happyReduction_122 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (TString happy_var_2) -> 
	happyIn25
		 (happy_var_1++[Astring happy_var_2]
	)}}

happyReduce_123 = happySpecReduce_2  21# happyReduction_123
happyReduction_123 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (XMLtext happy_var_2) -> 
	happyIn25
		 (happy_var_1++[Astring happy_var_2]
	)}}

happyReduce_124 = happySpecReduce_2  21# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_1++[happy_var_2]
	)}}

happyReduce_125 = happySpecReduce_1  22# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (if length happy_var_1 == 0 then Astring ""
                                                          else if length happy_var_1 == 1 then head happy_var_1 else Ast "append" happy_var_1
	)}

happyReduce_126 = happySpecReduce_1  23# happyReduction_126
happyReduction_126 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TString happy_var_1) -> 
	happyIn27
		 (if happy_var_1=="" then [] else [Astring happy_var_1]
	)}

happyReduce_127 = happySpecReduce_3  23# happyReduction_127
happyReduction_127 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 ([concatAll happy_var_2]
	)}

happyReduce_128 = happySpecReduce_2  23# happyReduction_128
happyReduction_128 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (TString happy_var_2) -> 
	happyIn27
		 (if happy_var_2=="" then happy_var_1 else happy_var_1++[Astring happy_var_2]
	)}}

happyReduce_129 = happyReduce 4# 23# happyReduction_129
happyReduction_129 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_1++[concatAll happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_130 = happySpecReduce_3  24# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ([Ast "pair" [Astring happy_var_1,happy_var_3]]
	)}}

happyReduce_131 = happyReduce 4# 24# happyReduction_131
happyReduction_131 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn28
		 (happy_var_1++[Ast "pair" [Astring happy_var_2,happy_var_4]]
	) `HappyStk` happyRest}}}

happyReduce_132 = happySpecReduce_2  25# happyReduction_132
happyReduction_132 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (happy_var_1 "child" (Avar ".") happy_var_2
	)}}

happyReduce_133 = happySpecReduce_3  25# happyReduction_133
happyReduction_133 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (happy_var_2 "attribute" (Avar ".") happy_var_3
	)}}

happyReduce_134 = happySpecReduce_3  25# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (happy_var_3 (happy_var_1 "child" (Avar ".") happy_var_2)
	)}}}

happyReduce_135 = happyReduce 4# 25# happyReduction_135
happyReduction_135 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn29
		 (happy_var_4 (happy_var_2 "attribute" (Avar ".") happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_136 = happySpecReduce_1  26# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_137 = happySpecReduce_2  26# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (happy_var_2 . happy_var_1
	)}}

happyReduce_138 = happySpecReduce_3  27# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (\e -> happy_var_2 "child" e happy_var_3
	)}}

happyReduce_139 = happyReduce 4# 27# happyReduction_139
happyReduction_139 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_3 of { happy_var_3 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (\e -> happy_var_3 "attribute" e happy_var_4
	) `HappyStk` happyRest}}

happyReduce_140 = happyReduce 4# 27# happyReduction_140
happyReduction_140 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_3 of { happy_var_3 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (\e -> happy_var_3 "descendant" e happy_var_4
	) `HappyStk` happyRest}}

happyReduce_141 = happyReduce 5# 27# happyReduction_141
happyReduction_141 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_4 of { happy_var_4 -> 
	case happyOut32 happy_x_5 of { happy_var_5 -> 
	happyIn31
		 (\e -> happy_var_4 "attribute-descendant" e happy_var_5
	) `HappyStk` happyRest}}

happyReduce_142 = happySpecReduce_2  27# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  happyIn31
		 (\e -> Ast "step" [Avar "parent",Astring "*",e]
	)

happyReduce_143 = happyReduce 4# 28# happyReduction_143
happyReduction_143 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_144 = happySpecReduce_0  28# happyReduction_144
happyReduction_144  =  happyIn32
		 ([]
	)

happyReduce_145 = happySpecReduce_1  29# happyReduction_145
happyReduction_145 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (\t e ps -> if null ps
								     then happy_var_1 t e
                                                                     else Ast "filter" (happy_var_1 t e:ps)
	)}

happyReduce_146 = happySpecReduce_1  29# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn33
		 (\t e ps -> Ast "step" ((Avar t):(Astring "*"):e:ps)
	)

happyReduce_147 = happySpecReduce_1  29# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (\t e ps -> if elem happy_var_1 path_steps
                                                                     then parseError [TError ("Axis "++happy_var_1++" is missing a node step")]
                                                                     else Ast "step" ((Avar t):(Astring happy_var_1):e:ps)
	)}

happyReduce_148 = happyReduce 4# 29# happyReduction_148
happyReduction_148 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (QName happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (\t e ps -> if elem happy_var_1 path_steps
                                                                     then if t == "child"
                                                                          then Ast "step" ((Avar happy_var_1):(Astring happy_var_4):e:ps)
                                                                          else parseError [TError ("The navigation step must be /"++happy_var_1++"::"++happy_var_4)]
                                                                     else parseError [TError ("Not a valid axis name: "++happy_var_1)]
	) `HappyStk` happyRest}}

happyReduce_149 = happyReduce 4# 29# happyReduction_149
happyReduction_149 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (QName happy_var_1) -> 
	happyIn33
		 (\t e ps -> if elem happy_var_1 path_steps
                                                                     then if t == "child"
                                                                          then Ast "step" ((Avar happy_var_1):(Astring "*"):e:ps)
                                                                          else parseError [TError ("The navigation step must be /"++happy_var_1++"::*")]
                                                                     else parseError [TError ("Not a valid axis name: "++happy_var_1)]
	) `HappyStk` happyRest}

happyReduce_150 = happySpecReduce_1  30# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (\_ _ -> happy_var_1
	)}

happyReduce_151 = happySpecReduce_1  30# happyReduction_151
happyReduction_151 happy_x_1
	 =  happyIn34
		 (\_ e -> e
	)

happyReduce_152 = happySpecReduce_3  30# happyReduction_152
happyReduction_152 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (\t e -> if e == Avar "."
                                                                  then concatenateAll happy_var_2
	                                                          else Ast "context" [e,Astring t,concatenateAll happy_var_2]
	)}

happyReduce_153 = happySpecReduce_2  30# happyReduction_153
happyReduction_153 happy_x_2
	happy_x_1
	 =  happyIn34
		 (\_ _ -> call "empty" []
	)

happyReduce_154 = happyReduce 4# 30# happyReduction_154
happyReduction_154 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (\t e -> if e == Avar "."
                                                                     then call happy_var_1 happy_var_3
                                                                  else Ast "context" [e,Astring t,call happy_var_1 happy_var_3]
	) `HappyStk` happyRest}}

happyReduce_155 = happySpecReduce_3  30# happyReduction_155
happyReduction_155 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (\_ e -> if elem happy_var_1 ["last","position","true","false","empty","select"]
                                                                  then call happy_var_1 []
                                                                  else call happy_var_1 [e]
	)}

happyReduce_156 = happyReduce 7# 31# happyReduction_156
happyReduction_156 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_7 of { happy_var_7 -> 
	happyIn35
		 (\v -> Ast "if" [call "instance-of" [Avar v,Ast "type" [happy_var_2]],happy_var_4,happy_var_7]
	) `HappyStk` happyRest}}}

happyReduce_157 = happyReduce 5# 31# happyReduction_157
happyReduction_157 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut35 happy_x_5 of { happy_var_5 -> 
	happyIn35
		 (\v -> Ast "if" [call "instance-of" [Avar v,Ast "type" [happy_var_2]],happy_var_4,happy_var_5 v]
	) `HappyStk` happyRest}}}

happyNewToken action sts stk [] =
	happyDoAction 88# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	RETURN -> cont 1#;
	SOME -> cont 2#;
	EVERY -> cont 3#;
	IF -> cont 4#;
	THEN -> cont 5#;
	ELSE -> cont 6#;
	LB -> cont 7#;
	RB -> cont 8#;
	LP -> cont 9#;
	RP -> cont 10#;
	LSB -> cont 11#;
	RSB -> cont 12#;
	TO -> cont 13#;
	PLUS -> cont 14#;
	MINUS -> cont 15#;
	TIMES -> cont 16#;
	DIV -> cont 17#;
	IDIV -> cont 18#;
	MOD -> cont 19#;
	TEQ -> cont 20#;
	TNE -> cont 21#;
	TLT -> cont 22#;
	TLE -> cont 23#;
	TGT -> cont 24#;
	TGE -> cont 25#;
	PRE -> cont 26#;
	POST -> cont 27#;
	IS -> cont 28#;
	SEQ -> cont 29#;
	SNE -> cont 30#;
	SLT -> cont 31#;
	SLE -> cont 32#;
	SGT -> cont 33#;
	SGE -> cont 34#;
	AND -> cont 35#;
	OR -> cont 36#;
	NOT -> cont 37#;
	UNION -> cont 38#;
	INTERSECT -> cont 39#;
	EXCEPT -> cont 40#;
	FOR -> cont 41#;
	LET -> cont 42#;
	IN -> cont 43#;
	AS -> cont 44#;
	COMMA -> cont 45#;
	ASSIGN -> cont 46#;
	WHERE -> cont 47#;
	ORDER -> cont 48#;
	BY -> cont 49#;
	ASCENDING -> cont 50#;
	DESCENDING -> cont 51#;
	ELEMENT -> cont 52#;
	ATTRIBUTE -> cont 53#;
	STAG -> cont 54#;
	ETAG -> cont 55#;
	SATISFIES -> cont 56#;
	ATSIGN -> cont 57#;
	SLASH -> cont 58#;
	QName happy_dollar_dollar -> cont 59#;
	DECLARE -> cont 60#;
	FUNCTION -> cont 61#;
	VARIABLE -> cont 62#;
	AT -> cont 63#;
	DOTS -> cont 64#;
	DOT -> cont 65#;
	SEMI -> cont 66#;
	COLON -> cont 67#;
	INSERT -> cont 68#;
	DELETE -> cont 69#;
	REPLACE -> cont 70#;
	INTO -> cont 71#;
	FROM -> cont 72#;
	WITH -> cont 73#;
	INSTANCE -> cont 74#;
	OF -> cont 75#;
	QMARK -> cont 76#;
	CAST -> cont 77#;
	CASTABLE -> cont 78#;
	CASE -> cont 79#;
	DEFAULT -> cont 80#;
	TYPESWITCH -> cont 81#;
	Variable happy_dollar_dollar -> cont 82#;
	XMLtext happy_dollar_dollar -> cont 83#;
	TInteger happy_dollar_dollar -> cont 84#;
	TFloat happy_dollar_dollar -> cont 85#;
	TString happy_dollar_dollar -> cont 86#;
	TokenEOF -> cont 87#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


-- Abstract Syntax Tree for XQueries
data Ast = Ast String [Ast]
         | Avar String
         | Aint Int
         | Afloat Double
         | Astring String
         deriving Eq


instance Show Ast
  where show (Ast s []) = s ++ "()"
        show (Ast s (x:xs)) = s ++ "(" ++ show x
                              ++ foldr (\a r -> ","++show a++r) "" xs
                              ++ ")"
        show (Avar s) = s
        show (Aint n) = show n
        show (Afloat n) = show n
        show (Astring s) = "\'" ++ s ++ "\'"


screenSize = 80::Int

prettyAst :: Ast -> Int -> (String,Int)
prettyAst (Avar s) p = (s,(length s)+p)
prettyAst (Aint n) p = let s = show n in (s,(length s)+p)
prettyAst (Afloat n) p = let s = show n in (s,(length s)+p)
prettyAst (Astring s) p = ("\'" ++ s ++ "\'",(length s)+p+2)
prettyAst (Ast s args) p
    = let (ps,np) = prettyArgs args
      in (s++"("++ps++")",np+1)
    where prettyArgs [] = ("",p+1)
          prettyArgs xs = let ss = show (head xs) ++ foldr (\a r -> ","++show a++r) "" (tail xs)
                              np = (length s)+p+1
                          in if (length ss)+p < screenSize
                             then (ss,(length ss)+p)
                             else let ds = map (\x -> let (s,ep) = prettyAst x np
                                                      in (s ++ ",\n" ++ space np,ep)) (init xs)
                                      (ls,lp) = prettyAst (last xs) np
                                  in (concatMap fst ds ++ ls,lp)
          space n = replicate n ' '


ppAst :: Ast -> String
ppAst e = let (s,_) = prettyAst e 0 in s


call :: String -> [Ast] -> Ast
call name args = Ast "call" ((Avar name):args)


concatenateAll :: [Ast] -> Ast
concatenateAll [x] = x
concatenateAll (x:xs) = foldl (\a r -> call "concatenate" [a,r]) x xs
concatenateAll _ = call "empty" []


concatAll :: [Ast] -> Ast
concatAll [x] = call "string" [x]
concatAll (x:xs) = foldl (\a r -> call "concatenate" [call "string" [a],r]) x xs
concatAll _ = call "empty" []


path_steps = ["child", "descendant", "attribute", "self", "descendant-or-self", "following-sibling", "following",
              "attribute-descendant", "parent", "ancestor", "preceding-sibling", "preceding", "ancestor-or-self" ]


data Token
  = RETURN | SOME | EVERY | IF | THEN | ELSE | LB | RB | LP | RP | LSB | RSB
  | TO | PLUS | MINUS | TIMES | DIV | IDIV | MOD | AS | QMARK
  | TEQ | TNE | TLT | TLE | TGT | TGE | SEQ | SNE | SLT | SLE | SGT | SGE
  | AND | OR | NOT | UNION | INTERSECT | EXCEPT | FOR | LET | IN | COMMA
  | ASSIGN | WHERE | ORDER | BY | ASCENDING | DESCENDING | ELEMENT
  | ATTRIBUTE | STAG | ETAG | SATISFIES | ATSIGN | SLASH | DECLARE | SEMI | COLON
  | FUNCTION | VARIABLE | AT | DOT | DOTS | TokenEOF | PRE | POST | IS
  | INSERT | INTO | DELETE | FROM | REPLACE | WITH | INSTANCE | OF
  | CAST | CASTABLE | CASE | DEFAULT | TYPESWITCH
  | QName String | Variable String | XMLtext String | TInteger Int
  | TFloat Double | TString String | TError String
    deriving Eq


instance Show Token
    where show (QName s) = "QName("++s++")"
	  show (Variable s) = "Variable("++s++")"
	  show (XMLtext s) = "XMLtext("++s++")"
	  show (TInteger n) = "Integer("++(show n)++")"
	  show (TFloat n) = "Double("++(show n)++")"
	  show (TString s) = "String("++s++")"
	  show (TError s) = "'"++s++"'"
          show t = case filter (\(n,_) -> n==t) tokenList of
                     (_,b):_ -> b
                     _ -> "Illegal token"


printToken (QName s) = s
printToken (Variable s) = "$"++s
printToken (XMLtext s) = "'"++s++"'"
printToken (TInteger n) = show n
printToken (TFloat n) = show n
printToken (TString s) = "\""++s++"\""
printToken (TError s) = "error("++s++")"
printToken t = case filter (\(n,_) -> n==t) tokenList of
           (_,b):_ -> b
           _ -> "Illegal token"


tokenList :: [(Token,String)]
tokenList = [(TokenEOF,"EOF"),(RETURN,"return"),(SOME,"some"),(EVERY,"every"),(IF,"if"),(THEN,"then"),(ELSE,"else"),
             (LB,"["),(RB,"]"),(LP,"("),(RP,")"),(LSB,"{"),(RSB,"}"),(QMARK,"?"),
             (TO,"to"),(PLUS,"+"),(MINUS,"-"),(TIMES,"*"),(DIV,"div"),(IDIV,"idiv"),(MOD,"mod"),
             (TEQ,"="),(TNE,"!="),(TLT,"<"),(TLE,"<="),(TGT,">"),(TGE,">="),(PRE,"<<"),(POST,">>"),
             (IS,"is"),(SEQ,"eq"),(SNE,"ne"),(SLT,"lt"),(SLE,"le"),(SGT,"gt"),(SGE,"ge"),(AND,"and"),
             (OR,"or"),(NOT,"not"),(UNION,"|"),(INTERSECT,"intersect"),(EXCEPT,"except"),
             (FOR,"for"),(LET,"let"),(IN,"in"),(AS,"as"),(COMMA,"','"),(ASSIGN,":="),(WHERE,"where"),(ORDER,"order"),
             (BY,"by"),(ASCENDING,"ascending"),(DESCENDING,"descending"),(ELEMENT,"element"),
             (ATTRIBUTE,"attribute"),(STAG,"</"),(ETAG,"/>"),(SATISFIES,"satisfies"),(ATSIGN,"@"),
             (SLASH,"/"),(DECLARE,"declare"),(FUNCTION,"function"),(VARIABLE,"variable"),
  	     (INSERT,"insert"),(INTO,"into"),(DELETE,"delete"),(FROM,"from"),(REPLACE,"replace"),(WITH,"with"),
             (AT,"at"),(DOTS,".."),(DOT,"."),(SEMI,";"),(COLON,":"),(INSTANCE,"instance"),(OF,"of"),
             (CAST,"cast"),(CASTABLE,"castable"),(CASE,"case"),(DEFAULT,"default"),(TYPESWITCH,"typeswitch")]


parseError tk = error (case tk of
                         ((TError s):_) -> "Parse error: "++s
                         (TokenEOF:_) -> "Parse error: Unexpected end of file"
		         _ -> "Parse error: "++(foldr (\a r -> (printToken a)++" "++r) "" (init (take 11 tk))))


scan :: String -> [Token]
scan cs = lexer cs ""


xmlText :: String -> [Token]
xmlText "" = []
xmlText text = [XMLtext text]


-- scans XML syntax and returns an XMLtext token with the text
xml :: String -> String -> String -> [Token]
xml ('{':cs) text n = (xmlText text)++(LSB : lexer cs ('{':n))
xml ('<':'/':cs) text n = (xmlText text)++(STAG : lexer cs ('<':'/':n))
xml ('<':'!':'-':cs) text n = xmlComment cs (text++"<!-") n
xml ('<':cs) text n = (xmlText text)++(TLT : lexer cs ('<':n))
xml ('(':':':cs) text n = xqComment cs text n
xml (c:cs) text n = xml cs (text++[c]) n
xml [] text _ = xmlText text


xqComment :: String -> String -> String -> [Token]
xqComment (':':')':cs) text n = xml cs text n
xqComment (_:cs) text n = xqComment cs text n
xqComment [] text _ = xmlText text


xmlComment :: String -> String -> String -> [Token]
xmlComment ('-':'>':cs) text n = xml cs (text++"->") n
xmlComment (c:cs) text n = xmlComment cs (text++[c]) n
xmlComment [] text _ = xmlText text


isQN :: Char -> Bool
isQN c = elem c "_-." || isDigit c || isAlpha c


isVar :: Char -> Bool
isVar c = elem c "_-." || isDigit c || isAlpha c


inXML :: String -> Bool
inXML ('>':'<':_) = True
inXML _ = False


-- the XQuery scanner
lexer :: String -> String -> [Token]
lexer [] "" = [ TokenEOF ]
lexer [] _ = [ TError "Unexpected end of input" ]
lexer (' ':'>':' ':cs) n = TGT : lexer cs n
lexer (c:cs) n
      | isSpace c = lexer cs n
      | isAlpha c || c=='_' = lexVar (c:cs) n
      | isDigit c = lexNum (c:cs) n
lexer ('$':c:cs) n | isAlpha c
      = let (var,rest) = span isVar (c:cs)
        in (Variable var) : lexer rest n
lexer (':':'=':cs) n = ASSIGN : lexer cs n
lexer ('<':'/':cs) n = STAG : lexer cs ('<':'/':n)
lexer ('<':'=':cs) n = TLE : lexer cs n
lexer ('>':'=':cs) n = TGE : lexer cs n
lexer ('<':'<':cs) n = PRE : lexer cs n
lexer ('>':'>':cs) n = POST : lexer cs n
lexer ('/':'>':cs) m = case m of
                         '<':n -> ETAG : (if inXML n then xml cs "" n else lexer cs n)
                         _ -> [ TError "Unexpected token: '/>'" ]
lexer ('(':':':cs) n = lexComment cs n
lexer ('<':'!':'-':cs) n = lexXmlComment cs "<!-" n
lexer ('.':'.':cs) n = DOTS : lexer cs n
lexer ('.':cs) n = DOT : lexer cs n
lexer ('!':'=':cs) n = TNE : lexer cs n
lexer ('\'':cs) n = lexString cs "" ('\'':n)
lexer ('\"':cs) n = lexString cs "" ('\"': n)
lexer ('[':cs) n = LB : lexer cs n
lexer (']':cs) n = RB : lexer cs n
lexer ('(':cs) n = LP : lexer cs n
lexer (')':cs) n = RP : lexer cs n
lexer ('}':cs) m = case m of
                     '{':'\"':n -> RSB : lexString cs "" ('\"':n)
                     '{':'\'':n -> RSB : lexString cs "" ('\'':n)
                     '{':n -> RSB : (if inXML n then xml cs "" n else lexer cs n)
                     _ -> [ TError "Unexpected token: '}'" ]
lexer ('+':cs) n = PLUS : lexer cs n
lexer ('-':cs) n = MINUS : lexer cs n
lexer ('*':cs) n = TIMES : lexer cs n
lexer ('=':cs) n = TEQ : lexer cs n
lexer ('<':c:cs) n = TLT : (lexer (c:cs) (if isAlpha c then ('<':n) else n))
lexer ('>':cs) m = case m of
                     '<':'/':'>':'<':n -> TGT : (if inXML n then xml cs "" n else lexer cs n)
                     '<':n -> TGT : xml cs "" ('>':m) 
                     _ -> TGT : lexer cs m
lexer (',':cs) n = COMMA : lexer cs n
lexer ('@':cs) n = ATSIGN : lexer cs n
lexer ('?':cs) n = QMARK : lexer cs n
lexer ('/':cs) n = SLASH : lexer cs n
lexer ('{':cs) n = LSB : lexer cs ('{':n)
lexer ('|':cs) n = UNION : lexer cs n
lexer (';':cs) n = SEMI : lexer cs n
lexer (':':cs) n = COLON : lexer cs n
lexer (c:cs) n = TError ("Illegal character: '"++[c,'\'']) : lexer cs n


lexExp :: String -> (String,String)
lexExp (e:cs)
    | e == 'e' || e == 'E'
    = case cs of
        '+':rest -> span isDigit rest
        '-':rest -> let (s,rest1) = span isDigit rest
                    in ('-':s,rest1)
        rest -> span isDigit rest
lexExp cs = ("",cs)


lexNum :: String -> String -> [Token]
lexNum cs n
    = let (si,rest) = span isDigit cs
      in case rest of
           '.':rest1
               -> let (sd,rest2) = span isDigit rest1
                  in case lexExp rest2 of
                       ("",_) -> (TFloat (read $ si ++ "." ++ sd)) : lexer rest2 n
                       (exp,rest3) -> (TFloat (read $ si ++ "." ++ sd ++ "e" ++ exp)) : lexer rest3 n
           _ -> case lexExp rest of
                  ("",_) -> (TInteger (read si)) : lexer rest n
                  (exp,rest3) -> (TFloat (read $ si ++ "e" ++ exp)) : lexer rest3 n


lexString :: String -> String -> String -> [Token]
lexString ('\"':cs) s m = case m of
                            '\"':n -> (TString s) : (lexer cs n)
                            _ -> lexString cs (s++"\"") m
lexString ('\'':cs) s m = case m of
                            '\'':n -> (TString s) : (lexer cs n)
                            _ -> lexString cs (s++"\'") m
-- a string in an attribute value must evaluate between {}
lexString ('{':cs) s (c:'<':n) = (TString s) : LSB : (lexer cs ('{':c:'<':n))
lexString ('\\':'n':cs) s n = lexString cs (s++['\n']) n
lexString ('\\':'r':cs) s n = lexString cs (s++['\r']) n
lexString (c:cs) s n = lexString cs (s++[c]) n
lexString [] s n = [ TError "End of input while in string" ]


lexComment :: String -> String -> [Token]
lexComment (':':')':cs) n = lexer cs n
lexComment (_:cs) n = lexComment cs n
lexComment [] n = [ TError "End of input while in comment" ]


lexXmlComment :: String -> String -> String -> [Token]
lexXmlComment ('-':'>':cs) text n = (xmlText (text++"->"))++(lexer cs n)
lexXmlComment (c:cs) text n = lexXmlComment cs (text++[c]) n
lexXmlComment [] text _ = xmlText text


lexVar :: String -> String -> [Token]
lexVar cs n =
    let (nm,rest) = span isQN cs
        token = case nm of
          "return" -> RETURN
          "some" -> SOME
          "every" -> EVERY
          "if" -> IF
          "then" -> THEN
          "else" -> ELSE
          "to" -> TO
          "div" -> DIV
          "idiv" -> IDIV
          "mod" -> MOD
          "and" -> AND
          "or" -> OR
          "not" -> NOT
          "union" -> UNION
          "intersect" -> INTERSECT
          "except" -> EXCEPT
          "for" -> FOR
          "let" -> LET
          "in" -> IN
          "as" -> AS
          "where" -> WHERE
          "order" -> ORDER
          "by" -> BY
          "ascending" -> ASCENDING
          "descending" -> DESCENDING
          "element" -> ELEMENT
          "attribute" -> ATTRIBUTE
          "satisfies" -> SATISFIES
          "declare" -> DECLARE
          "function" -> FUNCTION
          "variable" -> VARIABLE
          "at" -> AT
          "eq" -> SEQ
          "ne" -> SNE
          "lt" -> SLT
          "le" -> SLE
          "gt" -> SGT
          "ge" -> SGE
          "is" -> IS
	  "insert" -> INSERT
	  "into" -> INTO
	  "delete" -> DELETE
	  "from" -> FROM
	  "replace" -> REPLACE
	  "with" -> WITH
          "instance" -> INSTANCE
          "of" -> OF
          "cast" -> CAST
          "castable" -> CASTABLE
          "case" -> CASE
          "default" -> DEFAULT
          "typeswitch" -> TYPESWITCH
          var -> QName var
    in case token of
         QName v1 -> case rest of
                       ':':rest2 -> let (v2,rest3) = span isQN rest2
                                    in [QName v1,COLON,QName v2] ++ lexer rest3 n
                       _ -> QName v1 : lexer rest n
         x -> x : lexer rest n
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
