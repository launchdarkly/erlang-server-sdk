#!/bin/bash

set -e

PROJECT_DIR=$(pwd)

make doc

# Put the docs where releaser expects them.
cp -R $PROJECT_DIR/doc ${LD_RELEASE_DOCS_DIR}
