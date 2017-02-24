#!/bin/sh
# Stop on error:
set -e
# Be really verbose
set -x

RSCRIPT="Rscript --no-init-file"
SRC=$PWD
TARGET_BASE=~/net/home/cluster_testing
DATE=$(date +%Y%m%d)
TARGET=$TARGET_BASE/$DATE/vignette

# Building the vignettes is a bit tricky because I need to be in
# different location where the vignettes will be built
if [ ! -f ~/.smbcredentials ]; then
    echo "Can't automatically run vignettes for you -- no credentials"
    exit 1
fi

if [ -d $TARGET ]; then
    echo "Target directory already exists -- please delete first"
    exit 1
fi

if [ ! -d $(dirname $TARGET_BASE) ]; then
    echo "Can't automatically run vignettes for you -- missing directory"
    exit 1
fi

mkdir -p $TARGET
cp -p $SRC/vignettes/src/*.R $SRC/vignettes/src/Makefile $TARGET
echo $SRC > $TARGET/upstream

make -C $TARGET

cp $TARGET/*.md vignettes
chmod 644 vignettes/*.md
for f in vignettes/*.md; do
    mv "$f" "vignettes/$(basename "$f" .md).Rmd"
done
