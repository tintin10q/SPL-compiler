


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
