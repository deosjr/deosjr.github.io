dynamicland: dynamicland.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o dynamicland.wasm dynamicland.scm

datalog: datalog.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o datalog.wasm datalog.scm

whiskers: whiskers.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o whiskers.wasm whiskers.scm

serve: dynamicland datalog whiskers
	../../hoot/pre-inst-env guile -c '((@ (hoot web-server) serve))'
