############################ -*- Mode: Makefile -*- ###########################
## Makefile --- Construction de la bibliotheque JMFFT
##
## Auteur          : jean-michel Dupays (CNRS/IDRIS) <jean-michel.dupays@idris.fr>
## Créé le         : 26/06/2007
###############################################################################

SHELL = /bin/ksh

all: jmfft
jmfft:
	@echo
	@echo "*********** Construction de libjmfft.a ***********"
	@echo
	(cd src ; make && mv libjmfft.a ../lib/.)
