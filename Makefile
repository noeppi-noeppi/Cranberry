HASKELL_FILES = $(shell find app -type f -name '*.hs')
ELM_FILES = $(shell find web -type f -name '*.elm')

.PHONY: all clean
all: bin/cranberry

bin/cranberry: cranberry.cabal cabal.project.local $(HASKELL_FILES) web/src/Main.js web/index.js
	cabal build -j
	mkdir -p bin
	cp "$$(cabal -v0 list-bin exe:cranberry)" bin/cranberry

web/index.full.js: web/elm.json $(ELM_FILES)
	cd web && elm make --optimize --output=index.full.js src/Main.elm

web/index.z.js: web/index.full.js
	uglifyjs "$<" --compress 'pure_funcs=["F2","F3","F4","F5","F6","F7","F8","F9","A2","A3","A4","A5","A6","A7","A8","A9"],pure_getters,keep_fargs=false,unsafe_comps,unsafe' --output "$@"

web/index.js: web/index.z.js
	uglifyjs "$<" --mangle --output "$@"

clean:
	rm -f web/index.js web/index.full.js
	rm -rf dist-newstyle web/elm-stuff
