#!/usr/bin/env sh

docker load -i $(nix-build docker.nix --no-out-link) && docker run -p 8000:8000 -it blackbird-demo:latest
