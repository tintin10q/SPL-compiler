{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module SPL.Codegen.Codegen where 

import SPL.Codegen.GenWASM
import SPL.Codegen.GenSSM
import SPL.AST 


class (GenWASM a, GenSSM a) => CodeGeneratable a

instance CodeGeneratable (Program 'TypecheckedP)
