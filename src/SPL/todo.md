


- Nested multi line comments?
- Return type analysis
- SSM code gen
- Hoist global var decls to the top by doing a sort.
- Make one check function that checks both vars and decls. Woah
- Return type check.
    - How would that work?
        - Sjaak said a partially applied unify function with the return type. Or Return just changes the func enviroment??
        - We need a special case here for regid type vars. If they are of the same name I think it is actually allowed.
            - The way codegen will solve that is by using the cells of one large! Hopefully we can get away with that in wasm as well.

- Fields...
- Trim all function body to remove everything after the first top level return statement, when doing so emit a warning that there are x statements left of dead code with a meta.


I think we check the return types 2 times now, once with the name but also using the statement lists.

We can merge the checkFunctions into checkDecls! I am sure of it.

Also find out where actually the return errors are being triggered.
Disallowing anything on rigid type vars seems like a good idea!

At the end of a function check we need to replace the prints, but lets for now just do a trap 0

If a return type is not specified it should be type var! But I think it actually is. So make sure that in Return.hs we don't say that you have to put a type in Return because if its not there we have to infer it.   
    The current behavoir how I think it is now should only be if you specified void type

Finish brief printing, but we have to get GOING!


-  If both if return we can remove the rest

- Error for True should be true

-- For some reason putting -- ends the parser and no further part of the file is parsed


--- plain pretty printer without colors and no Rigid Type var

-- Optimizer, remove unused args from functions. Kind of difficult. First see if an arg is used at all and then if not remove it from the fun decl but also ALL the fun call expr
-- Optimizer, remove unused variables from functions. First see if a var decl is used further along. If not remove the decl. 
-- Optimizer, remove unused functions decls. First see if the function decl is called further along. If not remove the decl. Give a warning.

-- Remove try from parser

-- ask asks

-- the apply function I think does the decl update 
