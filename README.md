school-build
============

A build system for school work.

Usage
-----

Once `school-build` is compiled and the binary is in your path, follow these
instructions to configure it.

  * Create a random secret, e.g. `openssl rand -base64 32`
  * Store the random secret in `$XDG_CONFIG_DIR/school-build/key.bin`
  * Configure a GitHub webhook to point to the URL
    `http://your.domain:8080/github`. (See security note.)
  * Ensure that your school repository is available at `$HOME/school`.
  * Place a Makefile in your school repository.
  * (Optional) Create a startup job appropriate for your system to launch
    `school-build` on boot.

Every time a push is made to the GitHub remote, an HTTP request will be sent to
the configured URL. The webservice will then queue a background job to execute
the build. The build is executed by performing a git fetch to the remote
`origin`, which should be the GitHub remote, followed by a
`git reset --hard origin/master`, in order to set the work tree to the state
indicated by the tip of the master branch on the remote. Finally, the Makefile
in the repository root is executed. The logic for building the repository
should go in the Makefile.

Security
--------

Ideally, you should set up a reverse proxy such as Nginx to perform TLS
termination and forward the request to `school-build`. However, if the
repository is public, then it doesn't really matter if someone
man-in-the-middles the connection, since the information available in the
webhook HTTP request would be available on the world-facing repository page.
Furthermore, due to the signature verifications performed by
[servant-github-webhook](https://github.com/tsani/servant-github-webhook)
a request body that has been tampered with would be rejected. The worst a
malicious man-in-the-middle could do is alter the `X-Github-Event` header,
causing the build to never run, since builds are executed only for `push`
events.
