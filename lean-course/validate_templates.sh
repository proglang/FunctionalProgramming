#!/bin/sh
set -eu
cd "$(dirname "$0")"
for source in Templates/Week*.lean; do
  lake env lean "$source" >/dev/null
done
printf '%s\n' 'All templates parse and elaborate (intentional sorry warnings omitted).'
