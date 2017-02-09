#!/bin/bash

# Causes the repo specified on the command line to be built.

if test -z "$SCHOOL_BASE_URL" ; then
    echo "SCHOOL_BASE_URL not set"
    exit 1
fi

if test -z "$SCHOOL_BUILD_KEY" ; then
    echo "SCHOOL_BUILD_KEY not set"
    exit 1
fi

REPO="$1"

if test -z "$REPO" ; then
    echo "Missing required command line argument to specify the repo to build."
    exit 1
fi

curl -v -X POST \
    -H "Authorization: $SCHOOL_BUILD_KEY" \
    "$SCHOOL_BASE_URL/manual/$1"
