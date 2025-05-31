dynamicland: dynamicland.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o dynamicland.wasm dynamicland.scm

serve: dynamicland
	../../hoot/pre-inst-env guile -c '((@ (hoot web-server) serve))'
