all: index.html

index.html: roller.elm RollerStyle.elm RollerModel.elm RollerView.elm
	elm-make roller.elm --output index.html

clean:
	rm index.html

realclean:
	rm -r index.html elm-stuff
