.PHONY: client

client:
	cd client && elm make src/Main.elm --output app.js
