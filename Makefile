all: build

build:
	stack build

test: test-unit

test-unit:
	stack test :unit

lint:
	stack exec hlint src/ app/ test/
