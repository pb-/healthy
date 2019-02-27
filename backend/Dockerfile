FROM clojure AS builder

RUN mkdir /app
WORKDIR /app
COPY project.clj /app
RUN lein deps

COPY . /app
RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" healthy.jar


FROM openjdk:8-alpine
RUN apk --no-cache add ca-certificates
RUN mkdir /data
WORKDIR /root

EXPOSE 8080/tcp
VOLUME ["/data"]

ENV HEALTHY_STORAGE=/data/events.edn HEALTHY_PORT=8080

COPY --from=builder /app/healthy.jar .
CMD ["java", "-jar", "healthy.jar"]
