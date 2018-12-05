# Simple Proxy

Little Common Lisp throwaway server to be able to test edge cases on a proxy server. Built to help with some testing at work in which the vendor wanted us to test the edge case of a request to their service taking too long to respond. After trying to mess with network settings, we realized we just needed a little proxy server in between us and them for testing. 

## Usage

The only endpoint right now is `proxy-request`:

```
POST /proxy-request?url=some_url&timeout=1
Body-of-the-request
```

Where:

* `url` is a _full_ url that you want the proxy service to call on your behalf.
* `timeout` is the timeout in *minutes* you want it to sleep before responding.
* The body of the request will be passed as-is to the receiving server. For this particular iteration, we're dealing with a service that doesn't give a crap about headers, so we're not sending them over, but it's an easy addition.

### Example interaction

Notice that the `url` parameter is urlencoded. You can quickly put a string like this together using a free service like https://www.url-encode-decode.com/ (or your favorite language's native capabilities).

```sh
curl -vH "Content-Type: text/xml" -d "<TransferredValueTxn><TransferredValueTxnReq> <ReqCat>TransferredValue</ReqCat> <ReqAction>Echo</ReqAction> <Date>20181017</Date> <Time>180000</Time> <PartnerName>PARTNER</PartnerName> <EchoData>test</EchoData></TransferredValueTxnReq> </TransferredValueTxn>" http://localhost:8666/proxy-request\?url\=https%3A%2F%2Fxml-mockery.herokuapp.com%2Fcard_service
*   Trying ::1...
* TCP_NODELAY set
* Connection failed
* connect to ::1 port 8666 failed: Connection refused
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8666 (#0)
> POST /proxy-request?url=https%3A%2F%2Fxml-mockery.herokuapp.com%2Fcard_service HTTP/1.1
> Host: localhost:8666
> User-Agent: curl/7.54.0
> Accept: */*
> Content-Type: text/xml
> Content-Length: 258
> 
* upload completely sent off: 258 out of 258 bytes
< HTTP/1.1 200 OK
< Content-Length: 610
< Date: Mon, 26 Nov 2018 23:40:20 GMT
< Server: Hunchentoot 1.2.38
< Content-Type: text/xml; charset=utf-8
< 
<?xml version="1.0" encoding="UTF-8"?>
<TransferredValueTxn>
	<TransferredValueTxnResp>
		<RespCat>TransferredValue</RespCat>
		<RespAction>Echo</RespAction>
		<RespCode>0</RespCode>
		<RespMsg>Echoing</RespMsg>
		<RespRefNum>246686</RespRefNum>
		<Date>"20181126"</Date>
		<Time>"234015"</Time>
		<EchoData>test</EchoData>
		<TransferredValueTxnReq>
			<ReqCat>TransferredValue</ReqCat>
			<ReqAction>Echo</ReqAction>
			<Date>20181017</Date>
			<Time>180000</Time>
			<PartnerName>MOCK</PartnerName>
			<EchoData>test</EchoData>
		</TransferredValueTxnReq>
	</TransferredValueTxnResp>
</TransferredValueTxn>
* Connection #0 to host localhost left intact

```
## Running

Since this is literally supposed to be alive for under a week while we finish some documentation/integration testing, I'm just running it on a REPL from tmux session on the desired server:

```sh
[staging] luis@web9.nyc2: ~/common_lisp/simple-proxy (master u+1) $ sbcl
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (load "server.lisp")
To load "hunchentoot":
  Load 1 ASDF system:
    hunchentoot
; Loading "hunchentoot"
....
To load "drakma":
  Load 1 ASDF system:
    drakma
; Loading "drakma"

Call (hunchentoot:stop *server*) anytime to stop the server I just started!
T
* 10.160.20.20 - [2018-11-27 00:05:00] "GET / HTTP/1.1" 200 393 "-" "curl/7.21.0 (x86_64-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.15 libssh2/1.2.6"
10.160.20.20 - [2018-11-27 00:07:23] "POST /proxy-request?url=http%3A%2F%2Fsomewhere.com%3A80%2Fsome_path%2F HTTP/1.1" 200 566 "-" "curl/7.21.0 (x86_64-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.15 libssh2/1.2.6"
*
```

One cool thing: notice how the REPL is still responsive, this means we can stop the server from there, redefine methods on the fly, and even do debugging. The hunchentoot server is trucking along in a separate thread and logging to STDOUT; for example, on that same REPL:

```sh
* (hunchentoot:started-p *server*)
T
* (hunchentoot:stop *server*)
#<HUNCHENTOOT:EASY-ACCEPTOR (host *, port 8666)>
* (hunchentoot:started-p *server*)
NIL
```


## Setup

### Remote linux server

#### Installing Common Lisp

SBCL has binaries for different architectures. With `uname -m` I was able to tell that the remote system in which I want to deploy this had a 64bit architecture, downloaded the Linux version of the SBCL installer: http://www.sbcl.org/platform-table.html

Installation instructions reside at: http://www.sbcl.org/getting.html

Once that was done, I installed it in a nonstandard directory to not conflict with any system-wide things, as it is technically a "production" server:

```sh
511  wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.13-x86-64-linux-binary.tar.bz2
512  bzip2 -cd sbcl-1.4.13-x86-64-linux-binary.tar.bz2 | tar xvf -
513  cd sbcl-1.4.13-x86-64-linux/
521  INSTALL_ROOT=/home/luis/sbcl sh install.sh 
522  vim /home/luis/.bash_profile 
523  source /home/luis/.bash_profile 
524  sbcl
```

The `.bash_profile` tinkering I'm doing there is to set up a couple of env vars necessary due to the non-default installation folder, added this to that file:

```sh
export SBCL_HOME=/home/luis/sbcl/lib/sbcl
export PATH="$PATH:/home/luis/sbcl/bin"
```

#### Installing Quicklisp

Official instructions at: https://www.quicklisp.org/beta/ (the instructions at https://lisp-lang.org/learn/getting-started/ don't quite do the trick, since they skip the "add-to-init-file" bit).

Downloaded quicklisp to a local directory and then did:

```sh
$ sbcl --load quicklisp.lisp
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

  ==== quicklisp quickstart 2015-01-28 loaded ====

    To continue with installation, evaluate: (quicklisp-quickstart:install)

    For installation options, evaluate: (quicklisp-quickstart:help)

* (quicklisp-quickstart:install :path "/home/luis/quicklisp")
WARNING: Making quicklisp part of the install pathname directory
; Fetching #<URL "http://beta.quicklisp.org/client/quicklisp.sexp">
; 0.82KB
==================================================
838 bytes in 0.00 seconds (0.00KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/client/2017-03-06/quicklisp.tar">
; 250.00KB
==================================================
256,000 bytes in 0.01 seconds (31250.00KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/client/2015-09-24/setup.lisp">
; 4.94KB
==================================================
5,054 bytes in 0.00 seconds (0.00KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/asdf/2.26/asdf.lisp">
; 194.07KB
==================================================
198,729 bytes in 0.01 seconds (13862.24KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp.txt">
; 0.40KB
==================================================
408 bytes in 0.00 seconds (398.44KB/sec)
Installing dist "quicklisp" version "2018-10-18".
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/releases.txt">
; 406.11KB
==================================================
415,858 bytes in 0.06 seconds (7124.76KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/systems.txt">
; 302.96KB
==================================================
310,231 bytes in 0.03 seconds (12118.40KB/sec)

  ==== quicklisp installed ====

    To load a system, use: (ql:quickload "system-name")

    To find systems, use: (ql:system-apropos "term")

    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)

    For more information, see http://www.quicklisp.org/beta/

NIL
* (ql:add-to-init-file)
I will append the following lines to #P"/home/luis/.sbclrc":

  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

Press Enter to continue.

#P"/home/luis/.sbclrc"
```

To test that it works, a fresh REPL:

```sh
 ~/common_lisp $ sbcl
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:system-apropos "vecto")
#<SYSTEM 3d-vectors / 3d-vectors-20180831-git / quicklisp 2018-10-18>
#<SYSTEM 3d-vectors-test / 3d-vectors-20180831-git / quicklisp 2018-10-18>
#<SYSTEM adw-charting-vecto / adw-charting-20120909-http / quicklisp 2018-10-18>
#<SYSTEM cl-aa / cl-vectors-20180228-git / quicklisp 2018-10-18>
#<SYSTEM cl-aa-misc / cl-vectors-20180228-git / quicklisp 2018-10-18>
#<SYSTEM cl-glfw-opengl-apple_specular_vector / cl-glfw-20150302-git / quicklisp 2018-10-18>
#<SYSTEM cl-paths / cl-vectors-20180228-git / quicklisp 2018-10-18>
#<SYSTEM cl-paths-ttf / cl-vectors-20180228-git / quicklisp 2018-10-18>
#<SYSTEM cl-vectors / cl-vectors-20180228-git / quicklisp 2018-10-18>
#<SYSTEM com.elbeno.vector / vector-20130128-git / quicklisp 2018-10-18>
#<SYSTEM lispbuilder-sdl-cl-vectors / lispbuilder-20180831-git / quicklisp 2018-10-18>
#<SYSTEM lispbuilder-sdl-cl-vectors-examples / lispbuilder-20180831-git / quicklisp 2018-10-18>
#<SYSTEM lispbuilder-sdl-vecto / lispbuilder-20180831-git / quicklisp 2018-10-18>
#<SYSTEM lispbuilder-sdl-vecto-examples / lispbuilder-20180831-git / quicklisp 2018-10-18>
#<SYSTEM org.middleangle.foreign-numeric-vector / fnv-20140713-git / quicklisp 2018-10-18>
#<SYSTEM sb-vector-io / sb-vector-io-20110829-git / quicklisp 2018-10-18>
#<SYSTEM static-vectors / static-vectors-v1.8.3 / quicklisp 2018-10-18>
#<SYSTEM static-vectors/test / static-vectors-v1.8.3 / quicklisp 2018-10-18>
#<SYSTEM vecto / vecto-1.5 / quicklisp 2018-10-18>
#<SYSTEM vectometry / vecto-1.5 / quicklisp 2018-10-18>
#<SYSTEM vectors / vectors-20171227-git / quicklisp 2018-10-18>
```

#### Installing a web server

Going with [Hunchentoot](https://www.quicklisp.org/beta/UNOFFICIAL/docs/hunchentoot/doc/index.html#start), and installing it with quicklisp:

```sh
$ sbcl
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:system-apropos "hunchentoot")
#<SYSTEM clack-handler-hunchentoot / clack-20181018-git / quicklisp 2018-10-18>
#<SYSTEM froute/hunchentoot / froute-20180711-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-auth / hunchentoot-auth-20140113-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-cgi / hunchentoot-cgi-20140211-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-dev / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-single-signon / hunchentoot-single-signon-20131111-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-test / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM prometheus.exposers.hunchentoot / prometheus.cl-20160825-git / quicklisp 2018-10-18>
#<SYSTEM prometheus.exposers.hunchentoot.test / prometheus.cl-20160825-git / quicklisp 2018-10-18>
#<SYSTEM quux-hunchentoot / quux-hunchentoot-20180430-git / quicklisp 2018-10-18>
#<SYSTEM t-clack-handler-hunchentoot / clack-20181018-git / quicklisp 2018-10-18>
* (ql:quickload :hunchentoot)
To load "hunchentoot":
  Load 2 ASDF systems:
    asdf uiop
  Install 19 Quicklisp releases:
    alexandria babel bordeaux-threads cffi chunga cl+ssl
    cl-base64 cl-fad cl-ppcre flexi-streams hunchentoot md5
    rfc2388 split-sequence trivial-backtrace
    trivial-features trivial-garbage trivial-gray-streams
    usocket
;; plus some more sucessful installation messages...
```

Testing that it works:

On my host:

```sh
* (defparameter *server* (make-instance 'hunchentoot:acceptor :port 8666))
* (hunchentoot:start *server*)
#<HUNCHENTOOT:ACCEPTOR (host *, port 8666)>
```

On another host in the same network:

```sh
$ curl web9.nyc2.b.com:8666
<html>
  <head>
    <title>Welcome to Hunchentoot!</title>
  </head>
  <body>
    <h1>Welcome</h1>
    <p>
      When you're reading this message, Hunchentoot has been properly installed.
    </p>
    <p>
      Please read the <a href="hunchentoot-doc.html">documentation</a>.
    </p>
    <p>
      <img src="img/made-with-lisp-logo.jpg" width="300" height="100"/>
    </p>
  </body>
</html>
```

More development will be done following: https://www.quicklisp.org/beta/UNOFFICIAL/docs/hunchentoot/doc/index.html#start

(Notice that once one leaves the REPL session, the server is killed, an actual script would use something like: https://stackoverflow.com/questions/19739527/how-do-i-start-hunchentoot)


### Local machine, for development

#### Installing Common Lisp

For better Emacs development, there's a homebrew formula so I was able to just do

```sh
brew install sbcl
```

Since I don't need to install in any nonstandard location.

Confirming it works and is the same version as the server:

```sh
λ ~ sbcl
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (+ 1 1)
2
```

#### Installing Quicklisp

Same as above: download from the official site, jump into a REPL; following the official guide: https://www.quicklisp.org/beta/#installation

```sh
λ ~/quicklisp_install/ curl -O https://beta.quicklisp.org/quicklisp.lisp

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 57144  100 57144    0     0   129k      0 --:--:-- --:--:-- --:--:--  129k
λ ~/quicklisp_install/ curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   882  100   882    0     0   6923      0 --:--:-- --:--:-- --:--:--  6944
λ ~/quicklisp_install/ gpg --verify quicklisp.lisp.asc quicklisp.lisp
zsh: command not found: gpg
λ ~/quicklisp_install/ sbcl --load quicklisp.lisp
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

  ==== quicklisp quickstart 2015-01-28 loaded ====

    To continue with installation, evaluate: (quicklisp-quickstart:install)

    For installation options, evaluate: (quicklisp-quickstart:help)

* (quicklisp-quickstart:install)
; Fetching #<URL "http://beta.quicklisp.org/client/quicklisp.sexp">
; 0.82KB
==================================================
838 bytes in 0.00 seconds (818.36KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/client/2017-03-06/quicklisp.tar">
; 250.00KB
==================================================
256,000 bytes in 0.05 seconds (5208.33KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/client/2015-09-24/setup.lisp">
; 4.94KB
==================================================
5,054 bytes in 0.00 seconds (4935.55KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/asdf/2.26/asdf.lisp">
; 194.07KB
==================================================
198,729 bytes in 0.02 seconds (8437.88KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp.txt">
; 0.40KB
==================================================
408 bytes in 0.00 seconds (398.44KB/sec)
Installing dist "quicklisp" version "2018-10-18".
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/releases.txt">
; 406.11KB
==================================================
415,858 bytes in 0.04 seconds (11603.18KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/systems.txt">
; 302.96KB
==================================================
310,231 bytes in 0.03 seconds (10820.00KB/sec)

  ==== quicklisp installed ====

    To load a system, use: (ql:quickload "system-name")

    To find systems, use: (ql:system-apropos "term")

    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)

    For more information, see http://www.quicklisp.org/beta/

NIL
* (ql:add-to-init-file)
I will append the following lines to #P"/Users/luis.borjas/.sbclrc":

  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

Press Enter to continue.

#P"/Users/luis.borjas/.sbclrc"
* %                                                                                                               λ ~/quicklisp_install/ sbcl
This is SBCL 1.4.13, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:system-apropos "hunchentoot")
#<SYSTEM clack-handler-hunchentoot / clack-20181018-git / quicklisp 2018-10-18>
#<SYSTEM froute/hunchentoot / froute-20180711-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-auth / hunchentoot-auth-20140113-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-cgi / hunchentoot-cgi-20140211-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-dev / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-single-signon / hunchentoot-single-signon-20131111-git / quicklisp 2018-10-18>
#<SYSTEM hunchentoot-test / hunchentoot-v1.2.38 / quicklisp 2018-10-18>
#<SYSTEM prometheus.exposers.hunchentoot / prometheus.cl-20160825-git / quicklisp 2018-10-18>
#<SYSTEM prometheus.exposers.hunchentoot.test / prometheus.cl-20160825-git / quicklisp 2018-10-18>
#<SYSTEM quux-hunchentoot / quux-hunchentoot-20180430-git / quicklisp 2018-10-18>
#<SYSTEM t-clack-handler-hunchentoot / clack-20181018-git / quicklisp 2018-10-18>

```

## References

* https://www.quicklisp.org/beta/UNOFFICIAL/docs/hunchentoot/doc/index.html#example
* http://slack.net/~evenson/abcl/hunchentoot/ (interesting comments on quicklisp)
* https://www.quicklisp.org/beta/faq.html
* https://github.com/lfborjas/mockery (another project to pick up this vendor's slack)
* https://edicl.github.io/drakma/#dict-request
* https://edicl.github.io/hunchentoot/#teen-age
* https://github.com/fukamachi/clack (didn't really look into this, but looks neat except for the Roswell bit)
* https://edicl.github.io/hunchentoot/#requests
* https://stackoverflow.com/questions/45601566/endpoint-defined-with-define-easy-handler-returns-404
* https://www.url-encode-decode.com/
* To run as a shell script (didn't use this as I just call it from a REPL: https://stackoverflow.com/questions/9229526/how-to-use-quicklisp-when-cl-program-is-invoked-as-a-shell-script + http://www.sbcl.org/manual/#Shebang-Scripts)
