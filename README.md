Virtual Haskell Environment
===========================

What is it?
-----------
hsenv is a tool (inspired by Python's virtualenv)
to create isolated Haskell environments.


What does it do?
----------------
It creates a sandboxed environment in a .hsenv_<ENVIRONMENT_NAME>/ sub-directory
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

> hsenv

Now, every time you want to use this environment, you have to activate it:

> source .hsenv_foo/bin/activate

That's it! Now it's possible to use all regular Haskell tools like usual,
but it won't affect your global/system's Haskell environment, and also
your per-user environment (from ~/.cabal and ~/.ghc) will stay the same.
All cabal-installed packages will be private to this environment,
and also the external environments (global and user) will not affect it
(this environment will only inherit very basic packages,
mostly ghc and Cabal and their deps).

When you're done working with this environment, enter command 'deactivate_hsenv',
or just close the current shell (with exit).

> deactivate_hsenv

Advanced usage
--------------
Here's the most advanced usage of hsenv. Let's say you want to:

* hack on json library
* do so comfortably
* use your own version of parsec library
* and do all this using nightly version of GHC

First, download binary distribution of GHC for your platform
(e.g. ghc-7.3.20111105-i386-unknown-linux.tar.bz2).

Create a directory for you environment:

> mkdir /tmp/test; cd /tmp/test

Then, create a new environment using that GHC:

> hsenv --ghc=/path/to/ghc-7.3.20111105-i386-unknown-linux.tar.bz2

Activate it:

> source .hsenv_test/bin/activate

Download a copy of json library and your private version of parsec:

> darcs get http://patch-tag.com/r/Paczesiowa/parsec; cabal unpack json

Install parsec:

> cd parsec2; cabal install

Install the rest of json deps:

> cd ../json-0.5; cabal install --only-dependencies

Now, let's say you want to hack on Parsec module of json library.
Open it in emacs:

> emacsclient Text/JSON/Parsec.hs

Activate the virtual environment (hsenv must be required earlier):

> M-x hsenv-activate <RET> /tmp/test/ <RET>

Edit some code and load it in ghci using 'C-c C-l'. If it type checks,
you can play around with the code using nightly version of ghci running
in your virtual environment. When you're happy with the code, exit emacs
and install your edited json library:

> cabal install

And that's it.

Misc
----
hsenv has been tested on i386 Linux and FreeBSD systems,
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

Q: Can I use hsenv inside hsenv?  
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
A: Yes, just adjust the url in .hsenv_<ENVIRONMENT_NAME>/cabal/config file.

Q: How do I remove the whole virtual environment?  
A: If it's activated - 'deactivate_hsenv' it. Then, delete
   the .hsenv_<ENVIRONMENT_NAME>/ directory.

Q: Is every environment completely separate from other environments and
   the system environment?  
A: Yes. The only (minor) exception is ghci history - there's only one
   per user history file. Also, if you alter your system's GHC, then
   virtual environments using system's GHC copy will probably break.
   Virtual environments using GHC from a tarball should continue to work.

Q: Can I use multiple environments in the same directory (e.g. to test
   my project against different ghc versions)?  
A: Yes, just use different names for all the environments.

Q: Can I share one cabalized project directory among multiple environments
   (e.g. build cabalized project in the same dir using different environments)?  
A: Yes. hsenv also overrides cabal with a wrapper, that will force using different
   builddirs, so there shouldn't be even any recompilation between different environments.

Q: Is it possible to activate an environment upon entering its directory?  
A: Yes, if you really know what you're doing. Here's a snippet for bash:

    function precmd() {
        if [[ -z $HSENV ]]; then
            NUMBER_OF_ENVS=$(find . -maxdepth 1 -type d -name ".hsenv_*" | wc -l)
            case ${NUMBER_OF_ENVS} in
                "0") ;;
                "1") source .hsenv_*/bin/activate;;
                *) echo multiple environments, manual activaton required;;
            esac
        fi
    }
    export PROMPT_COMMAND=precmd
