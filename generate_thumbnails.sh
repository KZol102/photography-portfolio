#!/bin/bash

for file in content/media/pictures/*.jpg; do
    mkdir -p content/media/pictures/thumbnails;
    ffmpeg -i "$file" -vframes 1 -update true -vf scale=800:800 -y "content/media/pictures/thumbnails/$(basename "$file")";
done