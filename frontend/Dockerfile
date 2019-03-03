FROM clojure:tools-deps AS builder

# cache deps
RUN mkdir /build
WORKDIR /build
COPY deps.edn /build
RUN clojure -Stree

COPY src src/
RUN clojure -m cljs.main --compile-opts "{:closure-defines {\"goog.DEBUG\" false}}" --optimizations advanced -c healthy.fe.core


FROM nginx:alpine

COPY resources/nginx.conf /etc/nginx/conf.d/default.conf
COPY resources/public/* /usr/share/nginx/html/
COPY --from=builder /build/out/main.js /usr/share/nginx/html/
