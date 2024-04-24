module SPL.Codegen.WASM where

-- https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-instr-numeric

data NN = Thrity2 
data MM = Sixty4 
data UNN = NN | MM
data SX = U | S

data FNN = IDK 

data IUNop = CLZ | CTZ | POPCNT
data IBINop = AddI | SubI | MulI | DivI SX | RemI SX | AndI | OrI | XorI | ShlI | ShrI SX | RotlI | RotrI
data Funop = Abs | Neg | Sqrt | Ceil | Floor | TruncF | Nearest
data FBinop = AddFB | SubFB |  MulFB | DivFB | FloorFB | TruncFB | NearestFB
data Itestop = Eqz
data Irelop = EqI | NeI | LtI SX | GtI SX | LeI SX | GeI SX
data Frelop  = EqF | NeF | LtF | GtF | LeF | GeF

data Instr =  ConstI UNN | ConstF UNN 
             | IUnop IUNop| Funop Funop
             | Itestop
             | Irelop Irelop  | Frelop Frelop
             | Extend8s | Extend16s | Extend32s
             | I32WrapI64 | I64ExtendI32 UNN | TruncFmm SX
             | F32Demotef64 | F64PromoteF32 | Convert MM SX 
             | Reinterpret NN FNN 


type Code = [Instr]