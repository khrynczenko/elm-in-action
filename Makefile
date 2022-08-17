format:
	elm-format --yes src
build:
	elm make --debug --output=app.js src/Main.elm
build-folders:
	elm make --debug src/PhotoFolders.elm --output app.js
