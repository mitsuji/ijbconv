FROM alpine:latest

RUN apk add gmp-dev
RUN apk add zlib-dev
RUN wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.31-r0/glibc-2.31-r0.apk
RUN apk add glibc-2.31-r0.apk

RUN mkdir /var/ijbconv
COPY ijbconv-http-exe /var/ijbconv
COPY static /var/ijbconv/static
COPY init.sh /usr/local/bin
RUN chmod u+x /usr/local/bin/init.sh

EXPOSE 8080
ENTRYPOINT ["init.sh"]
