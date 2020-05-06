#!/bin/sh

/etc/init.d/sshd start

cd /var/ijbconv
LD_LIBRARY_PATH=/lib:/usr/lib ./ijbconv-http-exe 0.0.0.0 8080
