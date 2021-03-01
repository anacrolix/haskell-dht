{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BEncode
import Data.Either
import Test.Hspec

-- This is the implementation for Data.BEncode from the "bencoding" package.
checkDecode s = (decode s :: Result BValue) `shouldSatisfy` isRight
-- -- This is for Data.BEncode from the "bencode" package.
-- checkDecode = (`shouldNotBe` Nothing) . bRead

main = hspec $ do
  describe "bencode" $ do
    it "decodes good ping response" $
      checkDecode "d2:ip6:1\181\&4`]\US1:rd2:id20:2\245NisQ\255J\236)\205\186\171\242\251\227F|\194ge1:t4:r\146\223\236\&1:y1:re"
    it "decodes troublesome ping response from bootstrap node escaped by golang" $
      checkDecode "d2:ip6:1\xb54`[\x8b1:rd2:id20:\x1c\x11\xe0\x1b\xe8\xe7\x8dvZ.c3\x9fɚf2\r\xb7Te1:t1:\x001:y1:r1:v4:LT\x01\x02e"
    it "decodes troublesome ping response from bootstrap node from haskell show bytestring" $
      checkDecode "d2:ip6:1\181\&4`]\US1:rd2:id20:\FS\DC1\224\ESC\232\231\141vZ.c3\159\201\154f2\r\183Te1:t4:N\135!\220\&1:y1:r1:v4:LT\SOH\STXe"
