.PHONY: default test deps clean report-missing-deps
BUILD=dune build

native:
	$(BUILD) _build/install/default/bin/tigerc

bytecode:
	$(BUILD) _build/install/default/bin/tigerc.bc

all:
	$(BUILD)

compile-test-tool:
	$(BUILD) _build/install/default/bin/runtests 

llvm-util:
	_build/install/default/bin/llvm_util

test:
	_build/install/default/bin/runtests 

deps:
	opam install --deps-only .

clean:
	dune clean

report-missing-deps:
	dune external-lib-deps @install --missing

utop:
	dune utop . -- -init=./.ocamlinit
	
zip-compiler:
	(cd src; zip -FSr ../compiler.zip compiler)

ll-clean:
	rm -f _build/ll/*.ll 
	rm -f _build/ll/*.out	

normal-config:
	./configure-normal.sh

rescue-config:
	./configure-rescue.sh