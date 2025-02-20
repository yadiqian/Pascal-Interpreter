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
        ValueT(..),
        Body,
        Program(..),
        Defs,
        Table,
        Block(..),
        CaseStmt(..),
        VarDef(..),
        Param(..),
        Scope,
        FuncTable,
        toFloat,
        toBool,
        toString
    ) where

import qualified Data.Map as Map

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
    | VarReal String
    -- square root
    | Sqrt RealExp
    -- sin
    | Sin RealExp
    -- cos
    | Cos RealExp
    -- natural log
    | Ln RealExp
    -- exp
    | Exp RealExp
    -- function
    | FuncReal String [Param]
    deriving (Show, Eq)

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
    -- variable
    | VarBool String
    -- function
    | FuncBool String [Param]
    deriving (Show, Eq)

data GenExp = 
  FloatExp RealExp 
  | BExp BoolExp
  deriving (Show, Eq)

-- Data-structure for statements
data Statement = 
    -- TODO: add other statements
    -- Variable assignment
    Assign String GenExp
    -- If else statement
    | IfElse BoolExp Statement Statement
    -- If statement
    | If BoolExp Statement
    -- Block
    | Block [Statement]
    -- writeln
    | Print GenExp
    -- writeln with more than 1 argument
    | PrintList [Param]
    -- print new line
    | PrintNewLine
    -- Case statement
    | Case String [CaseStmt] [Statement]
    -- While loop
    | While BoolExp Statement
    -- For loop to
    | ForUp String RealExp RealExp Statement
    -- For loop downto
    | ForDown String RealExp RealExp Statement
    -- Function call
    | FuncCall String [Param]
    deriving (Show, Eq)

data CaseStmt = Check GenExp Statement deriving (Show, Eq)

data VType = REAL | BOOL deriving (Show, Eq)

data ValueT =
    Float Float
    | Bool Bool 
    deriving (Show, Eq)

data Definition = 
    -- Variable definition, list of var, type
    Def VarDef
    -- Functions
    | Func String [VarDef] [Definition] [Statement]
    -- Procedures
    | Proc String [VarDef] [VarDef] [Definition] [Statement]
    deriving (Show, Eq)

data VarDef =
    VarDef [String] VType
    deriving (Show, Eq)

data Block = 
    Body Body 
    | Stmt Statement
    deriving (Show, Eq)

data Program = Process Defs Body

data Param = 
    RealP RealExp
    | BoolP BoolExp
    | StrP String 
    deriving (Show, Eq)

-- Data-structure for hole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
-- type Program = [Statement]
type Defs = [Definition]
type Body = [Statement]
type Table = Map.Map String ValueT
type FuncTable = Map.Map String ([VarDef], [VarDef], [Definition], [Statement])
type Scope = [Table]

-- Type conversion
toFloat :: ValueT -> Float
toFloat (Float f) = f
toFloat _ = error("Value cannot be converted to float")

toBool :: ValueT -> Bool
toBool (Bool b) = b
toBool _ = error("Value cannot be converted to bool")

toString :: ValueT -> String
toString (Float f) = show f
toString (Bool b) = show b