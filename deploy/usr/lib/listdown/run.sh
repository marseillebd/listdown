#!/bin/sh
set -e

exe=/usr/lib/listdown/listdown-server
port=7668
usersDir=/var/lib/listdown/users
staticDir=/usr/lib/listdown/static

cd "/"
$exe \
  --port $port \
  --users-dir $usersDir \
  --static-dir $staticDir

