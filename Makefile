format:
	elm-format --yes src
build:
	elm make --debug src/PhotoGroove.elm --output app.js
build-folders:
	elm make --debug src/PhotoFolders.elm --output app.js
