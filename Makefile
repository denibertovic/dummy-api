.PHONY = build run doc

build:
	@stack build

run:
	@./.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/dummy-api-exe/dummy-api-exe

doc:
	@stack runghc src/Dummy/Api/Docs.hs > docs/README.md

