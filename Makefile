.PHONY = build run doc

VERSION ?= $(shell grep "^version:" dummy-api.cabal | cut -d " " -f14)

IMAGE_NAME=denibertovic/dummy-api

build:
	@stack build

run:
	@./.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/dummy-api-exe/dummy-api-exe

doc:
	@stack runghc src/Dummy/Api/Docs.hs > docs/README.md

image: build
	@docker build -t ${IMAGE_NAME}:${VERSION} .
	@docker tag ${IMAGE_NAME}:${VERSION} ${IMAGE_NAME}:latest

push: image
	@docker push ${IMAGE_NAME}:${VERSION}
	@docker push ${IMAGE_NAME}:latest

