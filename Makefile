.PHONY = build run doc

VERSION ?= $(shell grep "^version:" dummy-api.cabal | cut -d " " -f14)

build:
	@stack build

run:
	@./.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/dummy-api-exe/dummy-api-exe

doc:
	@stack runghc src/Dummy/Api/Docs.hs > docs/README.md

image: build
	@docker build -t denibertovic/dummy-api:${VERSION} .
	@docker tag denibertovic/dummy-api:${VERSION} denibertovic/dummy-api:latest

