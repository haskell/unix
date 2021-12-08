setTestOpts(when(opsys('mingw32'), skip))

test('posix002', [ omit_ways(prof_ways), fragile_for(16550, concurrent_ways) ],
                 compile_and_run, [''])

# Skip on mingw32: assumes existence of 'pwd' and /tmp
test('posix003', [extra_clean(['po003.out'])],
                 compile_and_run, [''])

test('posix004', [ ], compile_and_run, [''])

test('posix005', [ ], compile_and_run, [''])

test('posix006', normal , compile_and_run, [''])
test('posix009', [ omit_ways(threaded_ways) ], compile_and_run, [''])
test('posix010', normal, compile_and_run, [''])

test('posix014', [ omit_ways(prof_ways) ],
                 compile_and_run, [''])
