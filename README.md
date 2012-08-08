# A simple Emacs web server

This used to be `httpd.el` but there are already several of these out
there already of varying usefulness. Since the name change, it's been
stripped down to simply serve files and directory listings. Client
requests are sanitized so this *should* be safe, but I make no
guarantees.

## Usage

Once loaded, there are only two interactive functions to worry about:
`httpd-start` and `httpd-stop`. Files are served from `httpd-root`
(can be changed at any time) on port `httpd-port`. Directory listings
are enabled by default but can be disabled by setting `httpd-listings`
to `nil`.

```cl
(require 'simple-httpd)
(setq httpd-root "/var/www")
(httpd-start)
```

## Fork

There's a fork out there but it's been GPLed. Like the rest of my
software I want to keep this public domain.

## Unit tests

The unit tests can (and should usually) be run like so,

    emacs -batch -L . -l simple-httpd-test.el -f ert-run-tests-batch

It does some mocking to avoid using network code during testing.
