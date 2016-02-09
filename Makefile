
.PHONY = build run

build:
	@stack build

run:
	@./.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/dummy-api-exe/dummy-api-exe

