all:
	ghc -o scene -main-is RubiksAFrame rubiksAframe.hs

run:
	make
	./scene output.html

view:
	make run
	open output.html

clean:
	rm scene *.hi *.o output.html
