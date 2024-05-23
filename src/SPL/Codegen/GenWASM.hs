{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.Codegen.GenWASM where

import SPL.Codegen.WASM
import SPL.AST

class GenWASM a where
    gen :: a -> Code


instance GenWASM (Program TypecheckedP) where 
  gen _ = []


instance GenWASM (Decl TypecheckedP) where 
  gen _ = []


instance GenWASM (Stmt TypecheckedP) where 
  gen _ = []


instance GenWASM (Literal TypecheckedP) where 
  gen _ = []


