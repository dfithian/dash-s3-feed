# dash-s3-feed

A proxy server over S3 that serves [Dash](http://kapeli.com/) docsets.

## Setup

* Postgres: `brew install postgres`
* AWS Credentials in `~/.aws/credentials`
* Migrations: `psql -d postgres -f sql/create-tables.sql`
* Build: `stack install`
* Run: `cd dash-s3-feed && ~/.local/bin/dash-s3-feed`

## Authentication

This uses basic auth. Use an SSL certificate (not implemented yet).

## Getting files

For both manifests and docsets, `GET` to `/file/<bucket>/<path>`.

### Example

```bash
curl http://user:pass@localhost:8080/file/my-bucket/my%2furlescaped%2fpath%2fmanifest.xml
```

## Pushing docsets

Multipart upload with `version` (text) and `file` (.tar.gz file) inputs. `POST` to
`/file/<bucket>/<manifest-path>`.

### Example

```bash
curl http://user:pass@localhost:8080/file/my-bucket/my%2furlescaped%2fpath%2fmanifest.xml \
  -F "version=1.0" -F "file=@docset.tar.gz"
```

## Setting up a Dash feed

Go to Preferences > Downloads > `+`. Enter the manifest url
(`https://user:pass@server:port/file/bucket/path%2fmanifest.xml`). Click "Download".
