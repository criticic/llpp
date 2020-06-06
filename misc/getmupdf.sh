#!/bin/sh
set -eu

MUPDF_OUTPUT_DIR="$1"
MUPDF_URL="https://github.com/ArtifexSoftware/mupdf"
MUPDF_DESIRED_VERSION="43790a089627def8f40f298e2c16e10f9035fcc3"

if [ ! -d ${MUPDF_OUTPUT_DIR} ]; then
    echo "mupdf does not exist, fetching it from ${MUPDF_URL}"
    git clone ${cloneargs-} ${MUPDF_URL} --recursive ${MUPDF_OUTPUT_DIR}
fi

cd ${MUPDF_OUTPUT_DIR}
git remote update
MUPDF_VERSION=$(git rev-parse HEAD)

test "${MUPDF_VERSION}" = "${MUPDF_DESIRED_VERSION}" || {
    printf "mupdf current version is ${MUPDF_VERSION} "
    echo "switching to ${MUPDF_DESIRED_VERSION}"
    git reset --hard ${MUPDF_DESIRED_VERSION}
    git submodule update --init --recursive
}
