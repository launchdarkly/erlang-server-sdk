#!/bin/bash

set -e

TARGET_FILE=src/eld.app.src

TEMP_FILE=${TARGET_FILE}.tmp
sed "s/  {vsn, \".*\"},/  {vsn, \"${LD_RELEASE_VERSION}\"},/" "${TARGET_FILE}" > "${TEMP_FILE}"
mv "${TEMP_FILE}" "${TARGET_FILE}"

TARGET_FILE=src/eld_settings.erl

TEMP_FILE=${TARGET_FILE}.tmp
sed "s/-define(VERSION, \".*\")./-define(VERSION, \"${LD_RELEASE_VERSION}\")./" "${TARGET_FILE}" > "${TEMP_FILE}"
mv "${TEMP_FILE}" "${TARGET_FILE}"
