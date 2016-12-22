all:
	ghc -o scene aframe.hs

run:
	make
	./scene output.html

view:
	make run
	open output.html

clean:
	rm scene *.hi *.o
