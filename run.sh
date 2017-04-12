#!/bin/bash

loc="$PWD"
cd "$loc"

gnome-terminal -e 'bash -c "cd '$loc'/githubauth;echo ---- Yesod ----;stack exec githubauth; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/Crawler;echo ---- Crawler ----;stack exec Crawler-exe; exec bash"'
