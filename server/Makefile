all: test

build: Main.hs
	# stack docker pull
	stack setup
	stack build --only-dependencies
	stack build

test: Main.hs
	stack test

image: Main.hs
	stack image container
