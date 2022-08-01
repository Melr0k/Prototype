PROTOTYPE=src/prototype.exe
PROTOJS=html/prototype_js.js
JSBC=src/prototype_js.bc.js

all: build run

build:
	dune build $(PROTOTYPE)

run:
	dune exec ./$(PROTOTYPE)

clean:
	dune clean
	rm -f $(PROTOJS)

js:
	dune build $(JSBC)
	cp _build/default/$(JSBC) $(PROTOJS)
	chmod +w $(PROTOJS)
