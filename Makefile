all: roller.html

roller.html: roller.elm RollerStyle.elm RollerModel.elm RollerView.elm
	elm-make roller.elm --output roller.html

clean:
	rm roller.html

realclean:
	rm -r roller.html elm-stuff
