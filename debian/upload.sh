#!/bin/bash -e

dup cl-rss -Uftp.med-info.com -D/home/ftp/xlunit -C"/home/kevin/bin/remove-old-versions cl-rss latest" -su $*
