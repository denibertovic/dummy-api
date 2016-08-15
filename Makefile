.PHONY = build run doc

VERSION ?= $(shell grep "^version:" dummy-api.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)

IMAGE_NAME=denibertovic/dummy-api

build:
	@stack build

run:
	@./.stack-work/install/x86_64-linux/${RESOLVER}/${GHC_VERSION}/bin/dummy-api-exe

doc:
	@stack runghc src/Dummy/Api/Docs.hs > docs/README.md

image: build
	@docker build -t ${IMAGE_NAME}:${VERSION} .
	@docker tag ${IMAGE_NAME}:${VERSION} ${IMAGE_NAME}:latest

push: image
	@docker push ${IMAGE_NAME}:${VERSION}
	@docker push ${IMAGE_NAME}:latest

