.PHONY: all clean

all: prob12 sort sortML typeclass

prob12: prob12.hs
	ghc prob12.hs -o prob12

sort: sort.hs 
	ghc sort.hs -o sort

sortML: sortML.ml
	ocamlopt sortML.ml -o sortML 

typeclass: typeclass.hs
	ghc typeclass.hs -o typeclass

clean:
	rm -f sortML typeclass prob12 sort *.cm* *.o *.hi

