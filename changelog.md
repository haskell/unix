# Changelog for [`unix` package](http://hackage.haskell.org/package/unix)

## 2.8.5.1 *Apr 2023*

  * fix building with newer filepath/os-string when `#ifndef HAVE_OPENPTY`

## 2.8.5.0 *Dec 2023*

  * allow building with newer filepath/os-string

## 2.8.4.0 *Dec 2023*

  * add `haveStatx`
  * fix `statx.stx_mnt_id` detection on buggy glibc, see [GHC #24072](https://gitlab.haskell.org/ghc/ghc/-/issues/24072)

## 2.8.3.0 *Oct 2023*

  * add `getExtendedFileStatus` (based on `statx`) style functions
  * drop support for GHC < 8.6
  * Don't `foreign import` `environ`, see [GHC #24011](https://gitlab.haskell.org/ghc/ghc/-/issues/24011)

## 2.8.2.1 *Sep 2023*

  * Fix UB bug in `withFilePath` that causes it to error out (introduced in 2.8.2.0) wrt [#295](https://github.com/haskell/unix/issues/295)

## 2.8.2.0 *Sep 2023*

  * Bump bounds to accomodate `base-4.19` and `bytestring-0.12`.

  * Ensure that `FilePath`s don't contain interior `NUL`s.

  * JavaScript backend: add support for `utimes` / `lutimes` / `futimes`.

## 2.8.1.1 *Mar 2023*
  * Fix `System.Posix.Env.ByteString.getEnvironment` segfaulting on empty environment

## 2.8.1.0 *Feb 2023*
  * Fix build if HAVE_ALARM is undefined

  * Add missing autoconf checks for chown/fchdir/fchmod

  * Make TABX constructors and code conditional on underlying #defines

  * Bump bounds to accomodate base-4.18

  * Add semWaitInterruptible

  * semaphore: Teach semThreadWait to use semWait with threaded RTS

  * make the foreign imports of dlopen & dlclose safe

  * do not use capi for dlfcn.h stuff under wasm-wasi

  * Use capi for syscalls that break under musl's handling of 64-bit `time_t`

  * Replace `last` with `unsnoc`

  * Avoid Data.List.{head,tail}

  * Consistently use `throwErrnoPathIf*`

  * Fix WASI build

## 2.8.0.0 *August 2022*
  * Use ByteString for GroupEntry/UserEntry

  * Fix `semTrywait` on darwin

  * Fix vanishing environment variables in `System.Posix.Env.ByteString.putEnv`/`System.Posix.Env.ByteString.getEnv`

  * Add missing `setEnvironment` and `clearEnv` to `System.Posix.Env.ByteString`

  * Add support for filepath-1.4.100.0 with `PosixString` variants

  * Expose `DirStream`, `CDir`, `CDirent` and `DirStreamOffset` (internal API)

  * Add `fdRead`/`fdWrite` with ByteString payload and deprecate String based `fdRead`

  * Add `clocksPerSec`

  * Support wasm32-wasi

  * Implement BaudRate as an `Int` newtype and allow matching via extensible PatternSynonyms

  * Added `readDirStreamMaybe`

  * Warn when functions are used on unsupported platforms

  * Fix the error handling of `posix_fallocate`

  * Export `FileStatus` constructor

  * Various documentation and error message improvements

  * Add openDirStreamFd, openFileAt and createFileAt

  * Add accessors for st_blocks and st_blksize

  * Deal with FreeBSD getpwnam_r(3), ... thread safety.  On FreeBSD these
    are not in fact safe for overlapped execution with a sequence of
    getpwent(3) or getgrent(3) calls when multiple "green" threads share
    the same underlying OS thread.  The *ent(3) calls now run in bound
    threads or else locks are used to avoid overlapped execution.

  * Make passwd/group FFI functions "safe", these are not low-latency APIs.

  * Drop support for non-thread-safe getpwnam(3) and getpwuid(3).  All
    supported platforms have getpwnam_r(3) and getpwuid_r(3).  This was
    already the case for the getgr(nam|gid) calls.

  * Added terminal output flags to `System.Posix.Terminal.Common.TerminalMode`

        IXANY, ONLCR, OCRNL, ONOCR, ONLRET, OFDEL, OFILL, NLDLY(NL0,NL1),
        CRDLY(CR0,CR1,CR2,CR2), TABDLY(TAB0,TAB1,TAB2,TAB3) BSDLY(BS0,BS1),
        VTDLY(VT0,VT1), FFDLY(FF0,FF1)

  * Add support for `O_NOFOLLOW`, `O_CLOEXEC`, `O_DIRECTORY` and `O_SYNC`
    (#6, #57)

  * Refactor API of `openFd` removing `Maybe FileMode` argument,
    which now must be passed as part of `OpenFileFlags`
    (e.g. `defaultFileFlags { creat = Just mode }`)  (#58)

  * Remove deprecated `execvpe(3)` legacy-emulation CPP macro

  * Generalise return type of `exitImmediately` from `ExitCode -> IO ()` to
    `∀a. ExitCode -> IO a` (#130)

  * Add `Read`, `Show`, `Eq`, and `Ord` typeclass instances to `OpenFileFlags` and `OpenMode`. (#75, #141)

## 2.7.2.2  *May 2017*

  * Bundled with GHC 8.2.1

  * Improve Autoconf detection of `telldir`/`seekdir` and define
    `_POSIX_VDISABLE` if missing for Android (#91,#90)

  * Fix error message of `createSymbolicLink` (#84)

## 2.7.2.1  *Nov 2016*

  * Bundled with GHC 8.0.2

  * Don't use `readdir_r` if its deprecated.

  * Add argument documentation for Env modules

## 2.7.2.0  *Apr 2016*

  * Bundled with GHC 8.0.1

  * Don't assume non-POSIX `WCOREDUMP(x)` macro exists

  * Don't assume existence of `termios(3)` constants beyond `B38400`

  * Don't assume existence of `ctermid(3)`/`tcdrain(3)`

  * Change `drainOutput`'s `tcdrain(3)` into a `safe` FFI call

  * Turn build error into compile warnings for exotic `struct stat`
    configurations (GHC #8859)

  * Improve detection of `fdatasync(2)` (GHC #11137)

  * Drop support for Hugs

  * Drop support for Cygwin (and Windows in general)

## 2.7.1.0  *Dec 2014*

  * Bundled with GHC 7.10.1

  * Add support for `base-4.8.0.0`

  * Tighten `SafeHaskell` bounds for GHC 7.10+

  * Add haddock comments on `RTLD_NEXT` and `RTLD_DEFAULT`

  * Deprecate function `haveRtldLocal`

  * Fix `getGroupEntryForID/getGroupEntryForName` on Solaris. Solaris uses
    CPP macros for required `getgrgid_r` and `getgrnam_r` functions definition
    so the fix is to change from C ABI calling convention to C API calling
    convention

  * Fix potential type-mismatch in `telldir`/`seekdir` FFI imports

  * Use CAPI FFI import for `truncate` to make sure the LFS-version is used.

  * `executeFile`: Fix `ENOTDIR` error for entries with non-directory
    components in `PATH` (and instead skip over non-directory `PATH`-elements)

  * New functions in `System.Posix.Unistd`:
     - `fileSynchronise` (aka `fsync(2)`), and
     - `fileSynchroniseDataOnly` (aka `fdatasync(2)`)

  * New module `System.Posix.Fcntl` providing
     - `fileAdvise` (aka `posix_fadvise(2)`), and
     - `fileAllocate` (aka `posix_fallocate(2)`)

  * Fix SIGINFO and SIGWINCH definitions

## 2.7.0.1  *Mar 2014*

  * Bundled with GHC 7.8.1

  * Handle `EROFS` and `ETXTBSY` as (non-exceptional) permission denied in
    `fileAccess`

  * Fix `getFileStatus` to retry `stat(2)` when it returns `EAGAIN`
    (this can happen on Solaris)

## 2.7.0.0  *Nov 2013*

  * New `forkProcessWithUnmask` function in the style of `forkIOWithUnmask`

  * Change `forkProcess` to inherit the exception masking state of its caller

  * Add new `Bool` flag to `ProcessStatus(Terminated)` constructor
    indicating whether a core dump occured

  * New functions in `System.Posix.Files{,.ByteString}` for operating
    on high resolution file timestamps:

        setFdTimesHiRes :: Fd -> POSIXTime -> POSIXTime -> IO ()
        setFileTimesHiRes :: FilePath -> POSIXTime -> POSIXTime -> IO ()
        setSymbolicLinkTimesHiRes :: FilePath -> POSIXTime -> POSIXTime -> IO ()
        touchFd :: Fd -> IO ()
        touchSymbolicLink :: FilePath -> IO ()

  * Export `SignalInfo(..)` and `SignalSpecificInfo(..)` as well as
    the two `Handler` constructors `CatchInfo` and `CatchInfoOnce`
    from `System.Posix.Signals`

  * Don't export `seekDirStream` and `tellDirStream` if the underlying
    `seekdir(3)`/`telldir(3)` system calls are not available (as on Android)

  * Fix library detection of `shm*` on openSUSE (#8350)

  * Minor documentation fixes/updates

  * Update package to `cabal-version >= 1.10` format

## 2.6.0.1  *Jan 2013*

  * Bundled with GHC 7.6.2
  * Fix memory corruption issue in `putEnv`
  * Use `pthread_kill(3)` instead of `raise(2)` on OS X too

## 2.6.0.0  *Sep 2012*

  * Bundled with GHC 7.6.1
  * New functions `mkdtemp` and `mkstemps` in `System.Posix.Temp`
  * New functions `setEnvironment` and `cleanEnv`
  * New functions `accessTimeHiRes`, `modificationTimeHiRes`, and
    `statusChangeTimeHiRes` for accessing high resolution timestamps
