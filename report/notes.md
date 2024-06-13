

Big yourney the typeccking where I kept finding ways to make it simpler and simpler until its now quite simple.
I had a hard time understanding the type schemes. But my biggest mistake was trying to typecheck an ast with Maybe types in there.
This means that if you need to generate a typevar in one place you won't generate the same one somewhere else. 

While very exiting this wrong directin ultimalty took many hours. Once I did an instanciate phase everything became so much easier. 


Also while a cool idea converting between nodes of the tree was not as easy as I had hoped. Sometimes it would work easly but other times it was very difficult.
The type families definitly did not make the error messages nicer.

I changed the ssm.jar to accept labels that start with a '.
This is great to generate function names that the programmer can not make

Integer bound checking