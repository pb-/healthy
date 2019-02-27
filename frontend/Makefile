run:
	clojure -A:fig:build
.PHONY: run

build: out/main.js

out/main.js: src/healthy/fe/*.cljs
	clojure -m cljs.main --optimizations advanced -c healthy.fe.core
