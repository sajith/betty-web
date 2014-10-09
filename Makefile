#
# A Yesod web app has secrets in certain files; but we don't want to
# push those secrets to a public repository by accident.
#
# This makefile invokes GNU m4 to replace placeholders in ${foo}.in
# files with actual secrets, thus creating the appropriate ${foo}
# files.  The file M4MACROS contains actual secrets, and since it is
# in .gitignore, our secrets should be safe.
#

M4	 = m4
M4FLAGS	 = 
M4MACROS = secrets.m4

all: 	config/settings.yml \
	config/postgresql.yml \
	config/keter.yml \
	Betty/SESCreds.hs

config/settings.yml: config/settings.yml.in secrets.m4 
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

config/postgresql.yml: config/postgresql.yml.in secrets.m4 
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

config/keter.yml: config/keter.yml.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

Betty/SESCreds.hs: Betty/SESCreds.hs.in secrets.m4
	${M4} ${M4FLAGS} ${M4MACROS} $< > $@

