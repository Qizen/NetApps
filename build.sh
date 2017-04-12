#!/bin/bash

loc="$PWD"
cd "$loc"

cd githubauth
stack build

cd ../Crawler
stack build
