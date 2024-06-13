module Machine.Ast where

import Utils.Ident (Ident)

data Prog = Prog Block [Block]

data Block = Block Ident [Inst] LastInst

data Reg
  = AReg Int
  | TReg Int
  | L0
  | L1
  | S0
  | SP
  | Zero

data Inst
  = IAdd Reg Reg Reg -- rd = rs1 + rs2
  | ISub Reg Reg Reg -- rd = rs1 - rs2
  | IMul Reg Reg Reg -- rd = rs1 * rs2
  | Fetch Reg Reg Int -- rd = M[rs + imm]
  | Init Reg Int Reg -- M[rd + imm] = rs
  | LoadLabel Reg Ident -- rd = LABEL
  | AddImm Reg Reg Int -- rd = rs + imm
  | Move Reg Reg -- rd = rs
  | CPrint Reg

data LastInst
  = Jump Reg
  | If0 Reg Ident Ident
  | Halt