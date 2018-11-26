# Who proxies the proxy

Little Common Lisp throwaway thingy to be able to test edge cases on a proxy server

## Usage

Should be able to just call this service with the url you want to proxy to and the timeout (if any), and the rest of the querystring/request body/headers will be sent on over to the original service.

If you choose a timeout, it'll sleep for that amount of _minutes_ and then respond. Your code should be prepared for this.

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
* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8666))
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
