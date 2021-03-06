#
# A Yesod web app has secrets in certain files; but we don't want to
# push those secrets to a public repository by accident.  Hence this
# makefile.
#
# This makefile invokes GNU m4 to replace placeholders in ${foo}.in
# files with actual secrets, thus creating the appropriate ${foo}
# files.  The file M4MACROS contains actual secrets, and since it is
# in .gitignore, our secrets should be safe.
#
# This file also happens to contain some convenience rules (such as
# 'make devel' and 'make test').  That is just a happy accident.
#
# TODO: we could perhaps try using the 'configurator' library.
#

M4	 = m4
M4FLAGS	 =
M4MACROS = secrets.m4

all: build

files:	config/settings.yml \
	config/test-settings.yml \
	config/keter.yml \
	Betty/SESCreds.hs

build: files
	stack setup
	stack install --only-dependencies
	stack build

devel: build
	APPROOT=http://localhost:3000 yesod devel -p 3000 # was `stack exec yesod devel -- -p 3000`

test: build
	stack test

repl: build
	APPROOT=http://localhost:3000 stack ghci --ghci-options="-O0 -fobject-code app/DevelMain.hs"

test-repl: build
	stack ghci --test --main-is betty:test:test --ghci-options="-O0 -fobject-code"

ghcid: build
	ghcid -c "stack ghci"

ghcid-test: build
	ghcid -c "stack ghci --test --main-is betty:test:test"

secrets.m4: secrets.m4.example
	cp $< $@

config/settings.yml: config/settings.yml.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

config/test-settings.yml: config/test-settings.yml.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

config/postgresql.yml: config/postgresql.yml.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

config/keter.yml: config/keter.yml.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

Betty/SESCreds.hs: Betty/SESCreds.hs.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

.PHONY: clean
clean:
	rm -f config/settings.yml
	rm -f config/test-settings.yml
	rm -f config/postgresql.yml
	rm -f config/keter.yml
	rm -f Betty/SESCreds.hs
	stack clean

# Provides `make deploy` etc., when Makefile.deploy exists.
# Because deployment details are a... separate concern.
-include Makefile.deploy

