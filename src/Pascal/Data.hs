-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        RealExp(..),
        BoolExp(..),
        GenExp(..),
        Statement(..),
        VType(..),
        Definition(..),
        Body,
        Program
    ) where

-- Data-structure for  numeric expressions
data RealExp = 
    -- unary operator: Op name expression
    Op1 String RealExp
    -- binary operator: Op name leftExpression rightExpression
    | Op2 String RealExp RealExp
    -- function call: FunctionCall name ListArguments
    | FunCall String [RealExp]
    -- real value: e.g. Real 1.0
    | Real Float
    -- int value
    | Integer Int
    -- variable: e.g. Var "x"
    | Var String

-- Data-structure for boolean expressions
data BoolExp = 
    -- binary operator on boolean expressions
    OpB String BoolExp BoolExp
    -- negation, the only unary operator
    | Not BoolExp
    -- comparison operator: Comp name expression expression
    | Comp String RealExp RealExp
    -- true and false constants
    | True_C
    | False_C

data GenExp = 
  FloatExp RealExp 
  | BExp BoolExp

-- Data-structure for statements
data Statement = 
    -- TODO: add other statements
    -- Variable assignment
    Assign String GenExp
    -- If statement
    | If BoolExp Statement Statement
    -- Block
    | Block [Statement]
    -- writeln
    | Print GenExp
    -- definition
    | Definition

data VType = REAL | BOOL

data Definition = 
    -- Variable definition, list of var, type
    VarDef [String] VType
    -- Procedures
    | Proc String [(String, VType)] Statement

-- Data-structure for hole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
type Program = [Statement]
type Body = [Statement]