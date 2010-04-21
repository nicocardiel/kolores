# $Id$
#

noinst_LIBRARIES = libbutton.a
libbutton_a_SOURCES = \
button.f \
buttqbr.f \
buttqcf.f \
buttqch.f \
buttqex.f \
buttqit.f \
buttqpr.f \
buttqxb.f \
buttqyb.f \
buttqytext.f \
buttsbr.f \
buttscf.f \
buttsch.f \
buttsex.f \
buttsit.f \
buttspr.f \
buttsxb.f \
buttsyb.f \
buttsytext.f \
ifbutton.f \
rpgband.f \
rpgbegin.f \
rpgbegok.f \
rpgenv.f \
rpgerasb.f \
rpgeras.f \
rpgerasw.f 


bin_PROGRAMS = kolores
kolores_SOURCES = \
button.inc \
photometry.inc \
version.inc \
avoidrep.f \
expand.f \
filterpar.f \
findmml.f \
fit_sed.f \
fmean0.f \
guessef.f \
iofunctions.f \
kolores.f \
leeatm.f \
leefilter.f \
leevega.f \
listfilter.f \
ordena1f.f \
plotatm.f \
plotfilter.f \
plotspectrum2.f \
plotspectrum.f \
plotvega.f \
ranred.f \
rvrebin.f \
scompext.f \
sescrfits.f \
sleeascii.f \
sleefits.f \
systemfunction.f

kolores_LDADD = $(CFITSIO_LIBS) libbutton.a
AM_CPPFLAGS = -I$(top_srcdir) $(CFITSIO_CFLAGS) 
