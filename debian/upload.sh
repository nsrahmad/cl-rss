#!/bin/bash -e

dup cl-rss -Ufiles.kpe.io -D/home/ftp/cl-rss -su \
    -C"(umask 022; cd /srv/www/html/cl-rss; make install; find . -type d |xargs chmod go+rx; find . -type f | xargs chmod go+r)" $*
