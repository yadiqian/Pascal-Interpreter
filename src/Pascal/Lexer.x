{

{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Pascal.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters


-- TODO: Map symbols into token types (with or without parameters)
tokens :-
  $white+                               ; -- remove multiple white-spaces
  "//".*                                ; -- skip one line comments
  "(*".+"*)"                             ; -- skip comments
  $digit+                               { tok_read     TokenInt }
  $digit+\.$digit*                      { tok_read     TokenFloat }
  [\+]|[\-]|[\*]|[\/]|":="              { tok_string     TokenOp }
  and|or|xor|not                        { tok_string     TokenOp }
  [\>]|[\<]|[\=]|">="|"<="|"<>"         { tok_string     TokenOp }
  sqrt|sin|cos|ln|exp                   { tok_string     TokenOp }
  program|begin|end|writeln|var         { tok_string     TokenK }
  boolean|true|false|real|string        { tok_string     TokenK }
  [\;]|[\:]|[\,]|[\(]|[\)]              { tok_string     TokenK }
  if|then|else|case|of                  { tok_string     TokenK }
  $alpha [$alpha $digit \_ \']*         { tok_string     TokenID }
  [\.]                                  { tok     TokenEOF }
{

-- Some action helpers:
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p


-- TODO: Add your own token types here
data TokenClass
 = TokenOp     String
 | TokenK      String
 | TokenInt    Int
 | TokenFloat  Float
 | TokenID    String
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
