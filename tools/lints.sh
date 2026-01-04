#!/bin/sh
set -eux

version=`sed -n 's/^version:[[:space:]]*//p' directory.cabal`
grep -q "## $version (" changelog.md || {
    echo >&2 "Error: Please describe version $version in changelog.md."
    exit 1
}
