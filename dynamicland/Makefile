dynamicland: dynamicland.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o dynamicland.wasm dynamicland.scm

datalog: datalog.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o datalog.wasm datalog.scm

serve: dynamicland datalog
	../../hoot/pre-inst-env guile -c '((@ (hoot web-server) serve))'
