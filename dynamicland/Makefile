COMPILE = guild compile-wasm -L modules

dynamicland: dynamicland.scm
	$(COMPILE) -o dynamicland.wasm dynamicland.scm

datalog: datalog.scm
	$(COMPILE) -o datalog.wasm datalog.scm

whiskers: whiskers.scm
	$(COMPILE) -o whiskers.wasm whiskers.scm

variables: variables.scm
	$(COMPILE) -o variables.wasm variables.scm

wiki: wiki.scm
	$(COMPILE) -o wiki.wasm wiki.scm

keyboard: keyboard.scm
	$(COMPILE) -o keyboard.wasm keyboard.scm

time: time.scm
	$(COMPILE) -o time.wasm time.scm

live: live.scm
	# -fruntime-modules residualizes the run-time module system, which
	# (interaction-environment) needs to look up identifiers. Without it,
	# eval throws "No run-time module registry; recompile with -g" — the
	# error's hint is misleading; the actual fix is the feature flag.
	# -fmutable-defs lets the REPL redefine bindings (e.g., a user
	# redefining a function on a second Run).
	$(COMPILE) -fruntime-modules -o live.wasm live.scm

blue: blue-prince/blue.scm
	$(COMPILE) -o blue-prince/blue.wasm blue-prince/blue.scm

energy: grid/grid.scm
	$(COMPILE) -o grid/grid.wasm grid/grid.scm

serve:
	@python3 -m http.server

run: dynamicland datalog whiskers variables wiki keyboard blue energy live serve
