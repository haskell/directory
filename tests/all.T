test('renameFile001', extra_clean(['renameFile001.tmp1','renameFile001.tmp2']),
      compile_and_run, [''])

test('createDirectory001',  normal, compile_and_run, [''])

test('createDirectoryIfMissing001',  normal, compile_and_run, [''])

# No sane way to tell whether the output is reasonable here...
test('getHomeDirectory001',  ignore_output, compile_and_run, [''])

test('removeDirectoryRecursive001', normal, compile_and_run, [''])
