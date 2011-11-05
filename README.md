                          Virtual Haskell Environment

What is it?
-----------
virthualenv is a tool (inspired by Python's virtualenv)
to create isolated Haskell environments.


What does it do?
----------------
It creates a sandboxed environment in a .virthualenv/ sub-directory
of your project, which, when activated, allows you to use regular Haskell tools
(ghc, ghci, ghc-pkg, cabal) to manage your Haskell code and environment.
It's possible to create an environment, that uses different GHC version
than your currently installed. Very simple emacs integration mode is included.

Basic usage
-----------
First, choose a directory where you want to keep your
sandboxed Haskell environment, usually a good choice is a directory containing
your cabalized project (if you want to work on a few projects
(perhaps an app and its dependent library), just choose any of them,
it doesn't really matter). Enter that directory:

> cd ~/projects/foo

Next, create your new isolated Haskell environment
(this is a one time only (per environment) step):

> virthualenv

Now, every time you want to use this enviroment, you have to activate it:

> source .virthualenv/bin/activate

That's it! Now it's possible to use all regular Haskell tools like usual,
but it won't affect your global/system's Haskell environment, and also
your per-user environment (from ~/.cabal and ~/.ghc) will stay the same.
All cabal-installed packages will be private to this environment,
and also the external environments (global and user) will not affect it
(this environment will only inherit very basic packages,
mostly ghc and Cabal and their deps).

When you're done working with this environment, enter command 'deactivate',
or just close the current shell (with exit).

> deactivate

Advanced usage
--------------
The only advanced usage is using different GHC version.
This can be useful to test your code against different GHC version
(even against nightly builds).

First, download binary distribution of GHC for your platform
(e.g. ghc-7.0.4-i386-unknown-linux.tar.bz2),
then create a new environment using that GHC:

> virthualenv --ghc=/path/to/ghc_something.tar.bz2

Then, proceed (with [de]activation) as in the basic usage case.

Misc
----
virthualenv has been tested on i386 Linux and FreeBSD systems,
but it should work on any Posix platform. External (from tarball) GHC feature
requires binary GHC distribution compiled for your platform,
that can be extracted with tar and installed with
"./configure --prefix=PATH; make install".

FAQ
---
Q: Can I use it together with tools like cabal-dev or capri?  
A: No. All these tools work more or less the same (wrapping cabal command,
   setting GHC_PACKAGE_PATH env variable), so something will probably break.

Q: Using GHC from tarball fails, when using FreeBSD with a bunch of make tool  
   gibberish. What do I do?  
A: Try '--make-cmd=gmake' switch.

Q: Can I use virthualenv inside virthualenv?  
A: No. It may be supported in future versions.

Q: Does it work on x64 systems?  
A: It hasn't been tested, but there's no reason why it shouldn't.

Q: Will it work on Mac?  
A: I doubt it. It should be easy to make it work there with system's GHC,
   Using GHC from tarball will be probably harder. I don't have any mac
   machines, so you're on your own, but patches/ideas/questions are welcome.

Q: Will it work on Windows?  
A: I really doubt it would even compile. I don't have access to any windows
   machines, so you're on your own, but patches/ideas/questions are welcome.
   Maybe it would work on cygwin.

Q: Does it require bash?  
A: No, it should work with any POSIX-compliant shell. It's been tested with
   bash, bash --posix, dash, zsh and ksh.

Q: Can I use it with a different haskell package repository than hackage?  
A: Yes, just adjust the url in .virthualenv/cabal/config file.
