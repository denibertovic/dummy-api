FROM debian:jessie


RUN apt-get update && apt-get install -y --no-install-recommends \
   libpq-dev

RUN mkdir /opt/dummy-api

ARG BINARY_PATH
COPY "$BINARY_PATH" /opt/dummy-api/dummy-api-exe

CMD ["/opt/dummy-api/dummy-api-exe"]

