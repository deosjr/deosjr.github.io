dynamicland: dynamicland.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o dynamicland.wasm dynamicland.scm

datalog: datalog.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o datalog.wasm datalog.scm

whiskers: whiskers.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o whiskers.wasm whiskers.scm

variables: variables.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o variables.wasm variables.scm

wiki: wiki.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o wiki.wasm wiki.scm

blue: blue-prince/blue.scm
	# deosjr.github.io is checked out next to a bootstrapped Hoot folder
	../../hoot/pre-inst-env guild compile-wasm -L modules -o blue-prince/blue.wasm blue-prince/blue.scm

serve:
	@python3 -m http.server

run: dynamicland datalog whiskers variables wiki blue serve
