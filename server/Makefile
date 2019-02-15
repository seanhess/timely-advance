all: test

build: Main.hs
	stack docker pull
	stack setup --docker
	stack build --docker --only-dependencies
	stack build --docker

test: Main.hs
	stack test --docker

image: Main.hs
	stack image container --docker
