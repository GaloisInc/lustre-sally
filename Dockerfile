FROM haskell:8.6 AS build

RUN apt-get update && apt-get install -y libgmp10 libffi6
RUN cabal v2-update
RUN mkdir -p /install/bin
COPY .  /build
WORKDIR /build
RUN git submodule update --init
RUN cabal v2-run exe:generate-ui
RUN cabal v2-install exe:lustre-sally --symlink-bindir="/install/bin"

FROM debian:stretch-slim AS demo
RUN apt-get update && apt-get install -y libgmp10 libffi6
RUN useradd -m demo && chown -R demo:demo /home/demo
USER demo
WORKDIR /home/demo
RUN mkdir -p bin
RUN mkdir -p inputs
RUN mkdir -p outputs
COPY --from=build /install/bin/lustre-sally bin
ENTRYPOINT bin/lustre-sally --in-dir=inputs --out-dir=outputs

