#!/bin/bash -e

dup cl-rss -Ufiles.b9.com -D/home/ftp/cl-rss -C"(umask 022; /home/kevin/bin/remove-old-versions cl-rss latest)" -su $*
