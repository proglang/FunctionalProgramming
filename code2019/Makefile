M09functionaldata.pdf: M09functionaldata.tex
	pdflatex M09functionaldata

M09functionaldata.tex: M09functionaldata.lhs
	pandoc -f markdown+lhs --markdown-headings=atx -s -o M09functionaldata.tex M09functionaldata.lhs
