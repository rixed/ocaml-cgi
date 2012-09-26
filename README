ocamlcgi - Objective Caml library for writing CGIs
Copyright (C) 1997 Daniel de Rauglaudre, INRIA
Copyright (C) 1998 Jean-Christophe FILLIATRE

Installation:

 1. "make" will compile both bytecode and native-code versions of the library.
    Use "make byte" if your platform only suports bytecode compilation.

 2. You can install the library with "make install" ("make install-byte" on 
    bytecode only platforms). Default place is the ocaml standard library. 
    You can change it by editing the Makefile and setting the variable 
    TARGETDIR to another directory. 


Programs using it must be compiled this way:

     ocamlc -o <prog> -custom <options> cgi.cmo <files>

     ocamlopt -o <prog> <options> cgi.cmx <files>


Mainly, it provides a function to parse the CGI arguments, parse_args,
the result being an association list 

	[ (FIELD1, VALUE1); (FIELD2, VALUE2); ... ]

where the form was of the kind

	<INPUT NAME="FILED1" ...>
	<INPUT NAME="FIELD2" ...>
	...


Please send comments and bug reports to
	
	Jean-Christophe.Filliatre@lri.fr

