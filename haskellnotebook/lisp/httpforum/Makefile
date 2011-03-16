#
# References:
# [1] http://www.graphviz.org/doc/info/output.html

all: run

dot:
	dot -Tpng diagram.dot -o diagram.png 

run:
	clisp httpforum.lisp

clean:
	rm -vf *.png *.log

# End of Makefile
