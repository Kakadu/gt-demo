.PHONY: all clean pdf
all:
	ocamlbuild -use-ocamlfind src/demo01adt.native src/demo02polyvars.native \
		src/demo03argcount.native src/demo04option.native \
		src/demo06.native
pdf:
	pdflatex main.tex

clean:
	@$(RM) -f 
	@$(RM) -f *.out *.bak *.pdf *~

