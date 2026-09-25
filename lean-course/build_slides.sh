#!/bin/sh
set -eu
cd "$(dirname "$0")/slides"
for source in Week*.md; do
  pandoc "$source" -t beamer --slide-level=2 --pdf-engine=xelatex -H slide-header.tex -o "${source%.md}.pdf"
done
