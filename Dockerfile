FROM haskell:8.6 AS build

# Setup system
RUN apt-get update
RUN cabal v2-update
RUN apt-get install -y --no-install-recommends \
      wget unzip curl \
      cmake autoconf gperf patch file \
      default-jre \
      python2.7-dev python-sympy \
      libgmp-dev libffi6 \
      libboost-program-options-dev libboost-iostreams-dev \
      libboost-test-dev libboost-thread-dev libboost-system-dev \
      libreadline-dev flex bison automake libtool

# Setup sources for external tools

RUN wget --quiet https://github.com/Z3Prover/z3/releases/download/Z3-4.8.5/z3-4.8.5-x64-debian-8.11.zip
RUN unzip z3*.zip
RUN cp z3-*/bin/z3 /usr/local/bin/
RUN cp z3-*/bin/libz3.* /usr/local/lib/
RUN cp z3-*/include/* /usr/local/include/

RUN git clone -b master https://github.com/usi-verification-and-security/opensmt.git
RUN cd opensmt && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=Release -DPRODUCE_PROOF=ON .. && \
    make && make install

RUN mkdir -p /build
WORKDIR /build
RUN wget --quiet https://github.com/SRI-CSL/libpoly/archive/master.tar.gz -O libpoly.tar.gz
RUN wget --quiet https://github.com/SRI-CSL/yices2/archive/master.tar.gz -O yices2.tar.gz
RUN wget --quiet https://github.com/SRI-CSL/sally/archive/master.tar.gz -O sally.tar.gz

RUN tar -xzf libpoly.tar.gz
RUN tar -xzf yices2.tar.gz
RUN tar -xzf sally.tar.gz

# Build Sally

WORKDIR /build/libpoly-master/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release
RUN make -j install

WORKDIR /build/yices2-master
RUN autoconf
RUN ./configure --enable-mcsat
RUN make -j
RUN make install

WORKDIR /build/sally-master/build
RUN cmake ..
RUN make
RUN make install


# Build lustre-sally
# We do this last because it is likely to change more oftern

COPY docker-src/* ./
RUN tar -xzf language-lustre.tar.gz
RUN tar -xzf lustre-sally.tar.gz
RUN cabal v2-install exe:lustre-sally --symlink-bindir="/usr/local/bin"


# Now build the deplyment image

FROM debian:stretch-slim AS demo
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
      libgmp10 libffi6 libreadline7 libgmpxx4ldbl \
      libboost-program-options1.62 libboost-iostreams1.62 \
      libboost-test1.62 libboost-thread1.62 libboost-system1.62

RUN useradd -m demo && chown -R demo:demo /home/demo
COPY --from=build /usr/local/bin/sally        /usr/local/bin/sally
COPY --from=build /usr/local/bin/lustre-sally /usr/local/bin
COPY --from=build /usr/local/lib/* /usr/local/lib/
USER demo
WORKDIR /home/demo
RUN mkdir -p inputs
RUN mkdir -p outputs
ENV LD_LIBRARY_PATH=/usr/local/lib
ENTRYPOINT lustre-sally --in-dir=inputs --out-dir=outputs


