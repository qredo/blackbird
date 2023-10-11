# Generating a Docker image for the blackbird demo UI

> This assumes you are running an `aarch64-darwin` system.

We mostly need to follow steps 1-3 from [this page](https://www.notion.so/NixOS-tests-on-macOS-1631ebb136154b7cb952dca5440e3b19) in order to get this repository's `docker.nix` going.

First, add this to your `/etc/nix/nix.conf`.

```
# Needed for the darwin linux builder (aarch64-linux)
build-users-group = nixbld
extra-trusted-users = <REPLACE_WITH_YOUR_MACOS_USERNAME>
builders = ssh-ng://builder@localhost aarch64-linux /etc/nix/builder_ed25519 10 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=
builders-use-substitutes = true

# Needed for NixOS tests
extra-system-features = hvf
```

Then, restart the nix daemon:

``` sh
$ sudo launchctl kickstart -k system/org.nixos.nix-daemon
```

Finally, spin up the `aarch64-linux` builder that we added in `/etc/nix/nix.conf`:

``` sh
QEMU_OPTS="-m 8192" nix run github:NixOS/nixpkgs/af89d3a2be6f70edb187dd817377d6c4360134fa#darwin.builder --extra-experimental-features nix-command --extra-experimental-features flakes
```

Wait for this to bring you into a VM, and then in another terminal, run:

``` sh
$ nix-build docker.nix --no-out-link
```

This will produce a `tar.gz` file containing the docker image, which can be `docker load`ed:

``` sh
$ docker load -i $(nix-build docker.nix --no-out-link)
```

This registers the image under `blackbird-demo:latest`. You can then start a container from that image, and get the blackbird demo running on port 8000 with:

``` sh
$ docker run -p 8000:8000 -it blackbird-demo:latest
```

You should now be able to play around with blackbird policies in your browser at http://localhost:8000/.
