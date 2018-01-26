.PHONY: all clean celan pdf

.DEFAULT_GOAL=pdf

all:
	ocamlbuild -use-ocamlfind src/demo01adt.native src/demo02polyvars.native \
		src/demo03inhmatter.native src/demo05mutal.native \
		src/demo08argcount.native src/demo04option.native \
		src/demo06.native src/demo07lists.native \
		src/demo09nonrec.native

pdf:
	pdflatex main.tex

celan: clean
clean:
	@$(RM) -f 
	@$(RM) -f *.out *.aux *.log *.bak *.pdf *~ *.native

