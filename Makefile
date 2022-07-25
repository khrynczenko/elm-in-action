format:
	elm-format --yes src tests
build:
	elm make --debug src/PhotoGroove.elm --output app.js
