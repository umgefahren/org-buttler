FROM haskell:9.2

RUN mkdir /tmp/build
WORKDIR /tmp/build

COPY . .

RUN stack install --resolver lts-20.10

ENV ORGDIR=/tmp/org/
EXPOSE 3030
ENTRYPOINT [ "/root/.local/bin/org-buttler-exe" ]
