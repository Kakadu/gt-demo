.PHONY: all clean celan pdf

.DEFAULT_GOAL=all

all:
	ocamlbuild -use-ocamlfind -classic-display \
		src/demo01adt.native src/demo02polyvars.native \
		src/demo03inhmatter.native src/demo05mutal.native \
		src/demo08argcount.native src/demo04option.native \
		src/demo06.native src/demo07lists.native \
		src/demo09nonrec.native \
		src/demo10ulc.native 

pdf:
	pdflatex main.tex

celan: clean
clean:
	@$(RM) -r _build
	@$(RM) *.out *.aux *.log *.bak *.pdf *~ *.native

