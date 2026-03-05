FROM gcr.io/oss-fuzz-base/base-builder-rust
RUN apt-get update && apt-get install -y make autoconf automake libtool
COPY . $SRC/vext-email-gateway
WORKDIR $SRC/vext-email-gateway
COPY .clusterfuzzlite/build.sh $SRC/
