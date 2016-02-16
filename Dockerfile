FROM debian:jessie

RUN apt-get update && apt-get install -y --no-install-recommends \
   libpq-dev

RUN mkdir /opt/dummy-api

COPY .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/dummy-api-exe/dummy-api-exe /opt/dummy-api/dummy-api-exe

CMD ["/opt/dummy-api/dummy-api-exe"]

