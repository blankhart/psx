#!/usr/bin/env bash
# Downloads every package in the workspace's pinned package set into DIR,
# one directory per package, for psx-cst-compare.
#
#   packages/psx-cst/compare/fetch-package-set.sh DIR
set -euo pipefail

dir=${1:?usage: fetch-package-set.sh DIR}
root=$(cd "$(dirname "$0")/../../.." && pwd)
version=$(sed -n 's/^ *registry: *\([0-9.]*\) *$/\1/p' "$root/spago.yaml" | head -1)
[ -n "$version" ] || { echo "no package set version in spago.yaml" >&2; exit 1; }

set_file="$HOME/.cache/spago-nodejs/registry/package-sets/$version.json"
if [ ! -f "$set_file" ]; then
  set_file=$(mktemp)
  curl -sSfL "https://raw.githubusercontent.com/purescript/registry/main/package-sets/$version.json" -o "$set_file"
fi

mkdir -p "$dir"
jq -r '.packages | to_entries[] | "\(.key) \(.value)"' "$set_file" |
  xargs -P 16 -n 2 sh -c '
    [ -d "$0/$1-$2" ] && exit 0
    curl -sSfL --retry 3 "https://packages.registry.purescript.org/$1/$2.tar.gz" | tar xz -C "$0"
  ' "$dir"
echo "package set $version: $(ls "$dir" | wc -l) packages in $dir"
