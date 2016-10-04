#!/bin/bash
set -ex
VERSION=$1

if [ "$#" -ne 1 ]; then
    echo "illegal number of parameters"
    exit 1
fi

echo "Installing R version $VERSION"

MAJOR=$(echo "$VERSION" | awk -F \. {'print $1'})
MINOR=$(echo "$VERSION" | awk -F \. {'print $2'})
PATCH=$(echo "$VERSION" | awk -F \. {'print $3'})

if [ -z $MAJOR ] || [ -z $MINOR ] || [ -z $PATCH ]; then
    echo "Invalid version number"
    exit 1
fi

DEST="/opt/local/R/${VERSION}"
TARFILE="R-${VERSION}.tar.gz"
SRCDIR="src/R-${VERSION}"

if [ -d ${DEST} ]; then
    echo "*** R version ${VERSION} already installed at ${DEST}"
    exit 0
fi

if [ ! -f ${TARFILE} ]; then
    wget "https://cran.rstudio.com/src/base/R-${MAJOR}/${TARFILE}"
fi

# This might be useful?
# if [ -f ${SRCDIR} ]; then
#     rm -rf ${SRCDIR}
# fi

echo "*** Unpacking sources"
mkdir -p ${SRCDIR}
tar -C ${SRCDIR} -zxf ${TARFILE} --strip-components=1
cd ${SRCDIR}

echo "*** Configuring"
./configure --prefix="$DEST" --enable-R-shlib

echo "*** Building"
make

echo "*** Installing (enter password for sudo)"
sudo make install
