# Cranberry

Cranberry is a simple URL-shortener written in Haskell and Elm. It uses PostgreSQL as a backend and supports user synchronization via LDAP and OpenID Connect.

### Install Cranberry

The simplest way to install Cranberry is via docker: An image is provided at `ghcr.io/noeppi-noeppi/cranberry`.

Alternatively, you can build Cranberry from source. You'll need

- A working GHC (version `9.14.1`) and cabal installation. Those can easily be installed via [GHCup](https://www.haskell.org/ghcup/).
- An [Elm compiler](https://github.com/elm/compiler).

To build Cranberry, you'll need to install some dependencies. On Debian, they can be installed like this:
```shell
apt-get install -y libffi-dev libgmp-dev postgresql-client libpq-dev zlib1g-dev
```
Then you'll need to build the Elm project by running the following command:
```shell
cd web && elm make --optimize --output=index.js src/Main.elm
```
It is recommended to minify the generated javascript, for example by using [uglify-js](https://www.npmjs.com/package/uglify-js).
Once that is done, you can build and install the cabal project using
```shell
cabal build && cp "$(cabal -v0 list-bin exe:cranberry)" /usr/local/bin/cranberry
```

### Run Cranberry

Cranberry uses a file named `config.yaml` inside the current working directory as configuration. You can find the default values and the explanations for the various configuration keys [here](./config.yaml). When using the Docker container, you can mount it to `/data/config.yaml`.

To run Cranberry, just invoke the `cranberry` binary (or run the docker container). You'll then be able to access the web-ui in your browser.

### Wildcard shortlinks

By default a shortlink with name `hello` will only redirect the resource `/hello` but not `/hello/world`. However users with the `manage` permission can also create wildcard shortlinks. Using `hello*` as a shortlink name will **not** redirect `/hello` but redirect everything below `/hello` while keeping the extra part of the URL intact.
