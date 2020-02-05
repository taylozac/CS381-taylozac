module HW3 where

type Var   = String
type Macro = String

-- Pen Status Data Type
data Mode
   = Up
   | Down
  deriving (Eq, Show)


-- Expression Data Type
data Expr
   = Var String
   | Num_ Int
   | Add Expr Expr
  deriving (Eq, Show)

-- Program Data Type
data Prog
   = End
   | Next Cmd Prog
  deriving (Eq, Show)

-- Command Data Type
data Cmd
   = WriteMode Mode
   | Move Expr Expr
   | Define Macro [Var] Prog
   | Call Macro
  deriving (Eq, Show)

-- Line Macro Definition
--
-- substitute params and prog later
line = Define "line" ["x1", "y1", "x2", "y2"] End

-- Checking this works
expr_var :: Expr
expr_var = Var "test"

-- Checking this works
expr_num :: Expr
expr_num = Num_ 100

-- Checking this works
expr_add :: Expr
expr_add = Add expr_var expr_num

-- Checking this works
cmd_write :: Cmd
cmd_write = WriteMode Down

-- Checking this works
cmd_move :: Cmd
cmd_move = Move (Num_ 1) (Num_ 2)

-- Checking this works
cmd_define :: Cmd
cmd_define = Define "macro" ["these", "are", "all", "variables"] End

-- Checking this works
cmd_call :: Cmd
cmd_call = Call "macro"

-- Checking this works
test_prog :: Prog
test_prog = Next (WriteMode Down) (
             Next (cmd_move) (
              Next (cmd_define) (
               Next (cmd_call) (
                End    
               )
              )
             )
            )
