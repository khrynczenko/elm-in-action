format:
	elm-format --yes src
build:
	elm make --debug src/PhotoGroove.elm --output app.js
