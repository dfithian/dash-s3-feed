# dash-s3-feed

A proxy server over S3 that serves [Dash](http://kapeli.com/) docsets.

## Setup

* Postgres: `brew install postgres`
* AWS Credentials in `~/.aws/credentials`
* Migrations: `psql -d postgres -f dash-s3-feed/sql/create-tables.sql`
* Build: `stack install`
* Run: `cd dash-s3-feed && ~/.local/bin/dash-s3-feed`

## Creating Users

There's a `create-user` binary. For help, run `create-user --help`. An example invocation:

```bash
create-user --username username --password password \
  --conn-string "host=localhost port=5432 dbname=postgres"
```
## Authentication

This uses basic authentication. Generate an SSL certificate. Some sample steps:

```bash
# Create the key
openssl genrsa -out dash-s3-feed-key.pem 2048
# Create a CSR and fill out the appropriate values
openssl req -new -sha256 -key dash-s3-feed-key.pem -out dash-s3-feed-csr.csr
# Generate the certificate
openssl req -x509 -sha256 -days 365 -key dash-s3-feed-key.pem -in dash-s3-feed-csr.csr -out dash-s3-feed-certificate.pem
# Move key and certificate to the appropriate directory
mv dash-s3-feed-key.pem dash-s3-feed/config/dash-s3-feed-key.pem
mv dash-s3-feed-certificate.pem dash-s3-feed/config/dash-s3-feed-certificate.pem
```

## Getting files

For both manifests and docsets, `GET` to `/file/<bucket>/<path>`.

### Example

```bash
curl https://user:pass@localhost:10443/file/my-bucket/my%2furlescaped%2fpath%2fmanifest.xml
```

## Pushing docsets

Multipart upload with `version` (text) and `file` (.tar.gz file) inputs. `POST` to
`/file/<bucket>/<manifest-path>`.

### Example

```bash
curl https://user:pass@localhost:10443/file/my-bucket/my%2furlescaped%2fpath%2fmanifest.xml \
  -F "version=1.0" -F "file=@docset.tar.gz"
```

## Setting up a Dash feed

Go to Preferences > Downloads > `+`. Enter the manifest url
(`https://user:pass@server:port/file/bucket/path%2fmanifest.xml`). Click "Download".
