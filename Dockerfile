FROM haskell:9.2 AS builder

RUN mkdir /tmp/build
WORKDIR /tmp/build

COPY . .

RUN stack install --resolver lts-20.10


FROM debian:10-slim

WORKDIR /root/

COPY --from=builder /root/.local/bin/org-buttler-exe ./

EXPOSE 3000
ENTRYPOINT [ "./org-buttler-exe" ]
