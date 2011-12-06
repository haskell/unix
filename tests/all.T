if config.platform == 'i386-unknown-mingw32':
   conf = skip
else:
   conf = normal

test('signals001',  conf, compile_and_run, ['-package unix -cpp'])
test('signals002',  compose(only_compiler_types(['ghc']), conf),
	compile_and_run, ['-package unix'])
test('fileexist01', conf, compile_and_run, ['-package unix'])
test('forkprocess01', [ only_compiler_types(['ghc']),
                        extra_ways(['threaded1_ls']), # test #4512
                        conf ],
        compile_and_run, ['-package unix'])

#
# user001 may fail due to this bug in glibc:
#   http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=466647
#
test('user001', conf, compile_and_run, ['-package unix'])
test('resourceLimit', conf, compile_and_run, ['-package unix'])

if config.platform == 'i386-unknown-freebsd':
        conf = expect_fail

test('queryfdoption01', compose(omit_ways(['ghci']), compose(only_compiler_types(['ghc']), conf)),
	compile_and_run, ['-package unix'])
test('getEnvironment01', conf, compile_and_run, ['-package unix'])
test('getEnvironment02', conf, compile_and_run, ['-package unix'])
test('getGroupEntryForName', compose(conf, exit_code(1)), compile_and_run,
     ['-package unix'])
test('getUserEntryForName', compose(conf, exit_code(1)), compile_and_run,
     ['-package unix'])


test('signals004', normal, compile_and_run, ['-package unix'])

if ('threaded1' in config.run_ways):
   only_threaded_ways = only_ways(['ghci','threaded1','threaded2'])
else:
   only_threaded_ways = skip

test('fdReadBuf001', only_threaded_ways, compile_and_run, ['-package unix'])

test('fileStatus',
     extra_clean(['dir', 'regular', 'link-dir', 'link-regular']),
     compile_and_run,
     ['-package unix'])

test('fileStatusByteString',
     extra_clean(['dir', 'regular', 'link-dir', 'link-regular']),
     compile_and_run,
     ['-package unix'])


test('1185', normal, compile_and_run, ['-package unix'])

# This test fails for me on x86/Linux with a "does not exist" error.
# Running with strace shows it is trying to talk to winbindd (part of
# Samba), so I think the failure has nothing to do with GHC.  Also it
# works on a different machine that doesn't have Samba installed.
#  --SDM 18/05/2010
test('3816', normal, compile_and_run, ['-package unix'])

test('processGroup001', normal, compile_and_run, ['-package unix'])
test('processGroup002', normal, compile_and_run, ['-package unix'])
test('executeFile001', omit_ways(prof_ways), compile_and_run, ['-package unix'])
