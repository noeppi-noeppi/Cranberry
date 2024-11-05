# Cranberry

Cranberry is a simple URL-shortener written in Haskell and Elm. It uses PostgreSQL as a backend and supports user synchronization via LDAP.

### Install Cranberry

The simplest way to install Cranberry is via docker: An image is provided at `ghcr.io/noeppi-noeppi/cranberry`.

Alternatively, you can build Cranberry from source. You'll need

- A working GHC (version `9.4.8`) and cabal installation. Those can easily be installed via [GHCup](https://www.haskell.org/ghcup/).
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

### API

Cranberry provides an API to programmatically create and manage short links. The API endpoints can be found under `/_/api`.

Most API endpoints can need some kind of authentication. This can be done via HTTP Basic-Auth using a username and password or via Bearer-Auth using a token that was previously created with the `/_/api/grant` endpoint. Sending an unauthenticated request to an endpoint that needs authentication will result in a `401 Unauthorized` response with a `WWW-Authenticate` Header set that prompts for Basic-Auth. Sometimes however, it may be desirable to supress the `WWW-Authenticate` header. This can be achieved by setting the `X-No-Authenticate` header to `1` in your request.

The supported endpoints are
- `GET /_/api/me`: Provides information about the currently logged in user.
- `POST /_/api/grant`: Creates an access token that can be used instead of username and password. This endpoint is only available using Basic-Auth. Tokens are valid for 10 minutes.
- `POST /_/api/revoke`: Revokes the access token that was used to make the request. This endpoint is only available using Token-Auth.
- `POST /_/api/create`: Creates a new short link. The request body must contains the short link target as a URL. The server will reply with the newly created short link URL.
- `POST /_/api/create/:name`: Same as above but assign a custom name for the short link. If the name already exists, the result is `409 Conflict`.
- `GET /_/api/list`: Lists all known short links as a JSON-object where the keys are the link ids and the values are the link targets.
- `POST /_/api/revise/:name`: Same as named create but replaces the link, if it already exists.
- `POST /_/api/delete/:name`: Deletes the short link with the given id. The server will reply with the deleted short link URL.

The result of the `/_/api/me` endpoint is a json object with the two keys `user` and `role` where `user` contains the username of the logged in user or `null` for anonymous access and `role` is one of `none`, `create_anonymous`, `create_named` or `manage` and describes the current permissions.
