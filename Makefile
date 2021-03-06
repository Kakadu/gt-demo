.PHONY: all clean celan pdf hk

.DEFAULT_GOAL=all

all:
	ocamlbuild -use-ocamlfind -classic-display \
		src/demo01adt.native \
		src/demo03inhmatter.native src/demo05mutal.native \
		src/demo08argcount.native src/demo04option.native \
		src/demo06.native src/demo07lists.native \
		src/demo09nonrec.native \
		src/demo10ulc.native  \
		src/demo12mutal.native  \
		src/demo16modules.native \
		src/demo80onj.native \
 		src/demo20.native \
 		src/hashcons.native \
		src/demo808gadt.native \
 		#src/demo21.native \
		#src/demo13mutal.native  \
		#src/demo02polyvars.native \
 		#src/demo22.native \
 #		src/demo18irr.native \
 #		src/demo19.native \
 #		src/demo17.native \
 #		src/demo14mutal.native  \

hk:
	ocamlbuild -use-ocamlfind -classic-display \
		src/hk6.native

pdf:
	pdflatex -shell-escape main.tex

celan: clean
clean:
	@$(RM) -r _build
	@$(RM) *.out *.aux *.log *.bak *.pdf *~ *.native
