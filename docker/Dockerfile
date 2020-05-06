FROM alpine:latest


RUN set -x && apk update && apk upgrade


RUN apk add --no-cache openssh openrc

RUN rc-update add sshd && rc-status
RUN mkdir -p /run/openrc/ && touch /run/openrc/softlevel

# Enable Port Forwarding
RUN sed -i 's/^#Port 22/Port 2222/g' /etc/ssh/sshd_config
RUN sed -i 's/^#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
RUN sed -i 's/^AllowTcpForwarding no/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
RUN sed -i 's/^GatewayPorts no/GatewayPorts yes/g' /etc/ssh/sshd_config
RUN sed -i 's/^X11Forwarding no/X11Forwarding yes/g' /etc/ssh/sshd_config

RUN adduser -D ssh-user
# It will cause an ssh login error if user has no password
RUN echo "ssh-user:ssh-pass" | chpasswd
# If need sudo
# RUN apk add --no-cache sudo
# RUN echo "ssh-user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER ssh-user
RUN mkdir $HOME/.ssh
RUN ssh-keygen -m PEM -t rsa -b 1024 -C "ssh-user@example.com" -N "" -f $HOME/.ssh/id_rsa
RUN cat $HOME/.ssh/id_rsa.pub >> $HOME/.ssh/authorized_keys

USER root
RUN cp /home/ssh-user/.ssh/id_rsa $HOME/ssh-user.pem
RUN echo "root:Docker!" | chpasswd



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


EXPOSE 8080 2222


ENTRYPOINT ["init.sh"]
