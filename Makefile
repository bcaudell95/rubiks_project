scene:
	ghc -o scene -main-is RubiksAFrame rubiksAframe.hs

run-scene:
	make scene
	./scene output.html

view-scene:
	make run-scene
	open output.html



interactive:
	ghc -o interactive -main-is RubiksInteractive interactive.hs

run-interactive:
	make interactive
	./interactive



clean:
	-rm scene *.hi *.o output.html
	-rm interactive
