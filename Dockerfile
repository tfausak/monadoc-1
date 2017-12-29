FROM debian:stable-slim AS build

  ARG STACK_VERSION=1.6.3

  RUN apt-get update && apt-get upgrade --assume-yes && apt-get install --assume-yes gcc libgmp-dev make wget xz-utils zlib1g-dev
  
  WORKDIR /tmp/stack
  RUN wget https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64-static.tar.gz
  RUN tar --extract --file stack-$STACK_VERSION-linux-x86_64-static.tar.gz
  RUN cp stack-$STACK_VERSION-linux-x86_64-static/stack /usr/local/bin/
  
  WORKDIR /root/monadoc
  COPY stack.yaml .
  RUN stack setup
  
  COPY package.yaml .
  RUN stack build --only-dependencies
  
  COPY . .
  RUN stack build --copy-bins

FROM debian:stable-slim

  RUN apt-get update && apt-get upgrade --assume-yes && apt-get install --assume-yes ca-certificates libgmp-dev netbase
  COPY --from=build /root/.local/bin/monadoc /usr/local/bin/
  EXPOSE 8080
  CMD monadoc
