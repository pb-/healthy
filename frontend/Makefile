run:
	clojure -A:fig:build
.PHONY: run

build: out/main.js

out/main.js: src/healthy/fe/*.cljs
	clojure -m cljs.main --compile-opts '{:closure-defines {"goog.DEBUG" false}}' --optimizations advanced -c healthy.fe.core

image: out/main.js resources/nginx.conf resources/public/*
	docker build -t healthy-fe .
