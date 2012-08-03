FILES = \
	src/log.bend \
	src/token.bend \
	src/node.bend \
	src/symbol.bend \
	src/scope.bend \
	src/type.bend \
	src/parser.bend \
	src/pass.bend \
	src/native.bend \
	src/js-node.bend \
	src/js.bend \
	src/compiler.bend

all: build browser node

build:
	mkdir build

browser:
	# Compile the compiler for browsers
	bin/bend $(FILES) -o build/compiler.browser.js

node:
	# Compile the compiler for node.js
	bin/bend $(FILES) src/main.bend -o build/compiler.node.1.js

	# Use the compiled results to compile the compiler again (sanity check)
	node build/compiler.node.1.js $(FILES) src/main.bend -o build/compiler.node.2.js
	node build/compiler.node.2.js $(FILES) src/main.bend -o build/compiler.node.js

	# The compiled results should be the same for the same source code
	diff build/compiler.node.1.js build/compiler.node.2.js
	diff build/compiler.node.2.js build/compiler.node.js

	# Discard the intermediate builds
	rm -f build/compiler.node.1.js build/compiler.node.2.js

replace: node
	# Destructively update the current compiler
	cp build/compiler.node.js lib/bend.js
