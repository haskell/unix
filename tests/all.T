if config.platform == 'i386-unknown-mingw32':
   conf = skip
else:
   conf = normal

test('signals001',  conf, compile_and_run, ['-package unix -cpp'])
test('signals002',  compose(only_compiler_types(['ghc']), conf),
	compile_and_run, ['-package unix'])
test('fileexist01', conf, compile_and_run, ['-package unix'])
test('forkprocess01', compose(only_compiler_types(['ghc']), 
		      compose(expect_fail_for(['threaded2']), conf)),
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
test('getGroupEntryForName', compose(conf, expect_fail), compile_and_run,
     ['-package unix'])
test('getUserEntryForName', compose(conf, expect_fail), compile_and_run,
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

