minikanren: minikanren.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L . -o minikanren.wasm minikanren.scm

serve: minikanren
	../../hoot/pre-inst-env guile -c '((@ (hoot web-server) serve))'
