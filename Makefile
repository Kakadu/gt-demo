.PHONY: all clean pdf
all:
	ocamlbuild -use-ocamlfind src/demo01adt.native src/demo02polyvars.native \
		src/demo08argcount.native src/demo04option.native \
		src/demo06.native src/demo07lists.native

pdf:
	pdflatex main.tex

clean:
	@$(RM) -f 
	@$(RM) -f *.out *.bak *.pdf *~

