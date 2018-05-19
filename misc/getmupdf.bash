#!/bin/bash
set -eu

MUPDF_OUTPUT_DIR="$1"
MUPDF_URL="https://github.com/ArtifexSoftware/mupdf"
MUPDF_DESIRED_VERSION="bd88b96f7b7b21d64d4fb32a2674653a08f8cb38"

if [ ! -d ${MUPDF_OUTPUT_DIR} ]; then
    echo "mupdf does not exist, fetching it from ${MUPDF_URL}"
    git clone ${cloneargs} ${MUPDF_URL} --recursive ${MUPDF_OUTPUT_DIR}
fi

cd ${MUPDF_OUTPUT_DIR}
git fetch
MUPDF_VERSION=$(git rev-parse HEAD)

if [ "${MUPDF_VERSION}" = "${MUPDF_DESIRED_VERSION}" ]; then
    exit 0
fi

printf "mupdf current version is ${MUPDF_VERSION} "
echo "switching to ${MUPDF_DESIRED_VERSION}"
git checkout ${MUPDF_DESIRED_VERSION}
git submodule update --init --recursive
