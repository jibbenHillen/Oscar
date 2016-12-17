TARGET=oscar.native
OSCAR=oscar
STDLIB=stdlib.oscar
NATIVE=native
ACTOR=NATIVE/actor
IMMUT=NATIVE/immut

CXX= clang
CXXFLAGS = -std=c++14 -Wall -g -c -O3

LIBS=-I,/usr/lib/ocaml/
OCAMLLIBS=-I/usr/lib/ocaml
OCAMLFLAGS= ocamlopt -linkpkg -package llvm -package llvm.analysis

OCAMLC=ocamlfind

BUILDDIR=include

OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx oscar.cmx

oscar :
	@echo 'Buiding Oscar Compiler'
	@cd ./src && \
	eval `opam config env` && \
	make && \
	cd ../ && \
	mkdir -p $(BUILDDIR) && \
	mv ./src/$(TARGET) ./$(OSCAR) && \
	cp ./src/$(STDLIB) ./$(BUILDDIR) && \
	cp ./src/$(IMMUT)/* ./$(BUILDDIR) && \
	echo 'Oscar Compiler Succesfully Built'

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

.PHONY : all clean

all: clean oscar

clean :
	@echo 'Cleaning Oscar build'
	@cd ./src/ && \
	rm -f oscar parser.ml parser.mli scanner.ml *.cmo *.cmi *.cmx *.o && \
	rm -rf _build && \
	cd ../ && \
	rm -rf $(BUILDDIR) ./oscar && \
	echo Oscar build cleaned

# Generated by ocamldep *.ml *.mli
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
oscar.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
oscar.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo
