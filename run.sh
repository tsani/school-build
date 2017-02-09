#!/bin/bash

# Loads the necessary environment variables.
source .secrets

exec dist/build/school-build/school-build
