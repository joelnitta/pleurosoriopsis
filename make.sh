#!usr/bin/bash
set -e

git clone https://github.com/joelnitta/pleurosoriopsis
cd pleurosoriopsis
git checkout targets
Rscript -e 'targets::tar_make()'