develop:
	HEALTHY_STORAGE=events.edn HEALTHY_PORT=8080 lein repl
.PHONY: develop

build:
	lein uberjar
.PHONY: build

image:
	docker build -t healthy .
.PHONY: image
