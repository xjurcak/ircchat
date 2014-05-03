erl -detached -eval 'make:all()' -eval 'init:stop()'
start cmd.exe @cmd /k "erl -boot start_sasl -sname c1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname l1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname l2"
start cmd.exe @cmd /k "erl -boot start_sasl -sname il1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname il2"
start cmd.exe @cmd /k "erl -boot start_sasl -sname ls1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname ls2"
start cmd.exe @cmd /k "erl -boot start_sasl -sname adm1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname adm2"
start cmd.exe @cmd /k "erl -boot start_sasl -sname chrm1"
start cmd.exe @cmd /k "erl -boot start_sasl -sname chrm2"
erl -sname initiator < init.txt