SYSTYPE="IUCAA_PERSEUS"
#SYSTYPE="IUCAA_CRAY"
#SYSTYPE="IUCAA_SURYA"

F90C=ifort
FFLAG = -openmp -O2 -ip -W0 -WB -fpp2 -vec_report0 -I. -debug

ifeq ($(SYSTYPE),"IUCAA_PERSEUS")
GCC=/usr/lib/gcc/x86_64-redhat-linux/4.4.6/
CFITSIO=/data1/sysadmin/cjayanti/Software/cfitsio
LPACK=/data1/sysadmin/cjayanti/Software/lapack-3.4.2
CAMB=../camb
WMAP9=./wmap_likelihood_v5
endif

ifeq ($(SYSTYPE),"IUCAA_CRAY")
GCC=/usr/lib/gcc/x86_64-redhat-linux/4.1.2
CFITSIO=/data1/software/cfitsio
LPACK=/data1/software/lapack/lapack-3.3.1
CAMB=../camb
WMAP9=../wmap_likelihood_v5
endif

ifeq ($(SYSTYPE),"IUCAA_SURYA")
GCC=/usr/lib/gcc/x86_64-linux-gnu/4.6.3
CFITSIO=/opt/software/cfitsio
LPACK=/opt/software/lapack-3.5.0
CAMB=../camb
WMAP9=../wmap_likelihood_v5
endif

FFLAG+=-I$(CAMB) -I$(WMAP9)   

CAMBOBJS=$(CAMB)/constants.o $(CAMB)/utils.o  $(CAMB)/subroutines.o $(CAMB)/inifile.o $(CAMB)/power_tilt.o $(CAMB)/recfast.o $(CAMB)/reionization.o $(CAMB)/modules.o $(CAMB)/bessels.o $(CAMB)/equations.o $(CAMB)/halofit.o $(CAMB)/lensing.o $(CAMB)/SeparableBispectrum.o $(CAMB)/cmbmain.o $(CAMB)/camb.o 

WMAP9OBJS=$(WMAP9)/WMAP_9yr_likelihood.o  $(WMAP9)/read_archive_map.o $(WMAP9)/read_fits.o\
	 $(WMAP9)/healpix_types.o $(WMAP9)/br_mod_dist.o $(WMAP9)/WMAP_9yr_options.o \
	 $(WMAP9)/WMAP_9yr_util.o $(WMAP9)/WMAP_9yr_gibbs.o $(WMAP9)/WMAP_9yr_tt_pixlike.o\
	 $(WMAP9)/WMAP_9yr_tt_beam_ptsrc_chisq.o $(WMAP9)/WMAP_9yr_teeebb_pixlike.o\
	 $(WMAP9)/WMAP_9yr_tetbeebbeb_pixlike.o $(WMAP9)/WMAP_9yr_likelihood.o 

NROBJS  =    nrtype.o  nrutil.o  nr.o  

PSOOBJS =    cmb_likelihood.o $(CAMBOBJS)  

all : camb wmap  global.o libwmap9_pso.a $(NROBJS) cmb_likelihood.o cosmobeba 

camb:
	cd $(CAMB)     && $(MAKE) 
	
wmap:
	cd $(WMAP9)    && $(MAKE)

nrutil.o: nrutil.f90 
	$(F90C) -c nrutil.f90 
nrtype.o: nrtype.f90
	$(F90C) -c nrtype.f90
nr.o:   nr.f90
	$(F90C) -c nr.f90

libwmap9_pso.a: $(WMAP9OBJS)
	ar r libwmap9_pso.a  $(WMAP9OBJS)
global.o : global.f90 
	$(F90C) -c global.f90 -I$(CAMB) 
cmb_likelihood.o:  cmb_likelihood.f90 
	$(F90C) $(FFLAG) -c cmb_likelihood.f90   -I$(CAMB) -I$(WMAP9) 
cosmobeba: $(NROBJS) $(PSOOBJS) global.f90 func.f90 read_cambpar.f90 amoeba.f90 main.f90 
	$(F90C) $(FFLAG) $(PSOOBJS) $(NROBJS) global.f90 func.f90 read_cambpar.f90 amoeba.f90 main.f90  -o cosmobeba  libwmap9_pso.a -L$(CFITSIO)/lib -lcfitsio  -L$(LPACK) -llapack -lblas -L$(GCC) -lgfortran     
clean_camb:
	cd $(CAMB)     && $(MAKE) clean  
clean_wmap:
	cd $(WMAP9)    && $(MAKE) clean
clean:
	rm -rf *~ *.o *.mod  *.err *.out lib*  cosmobeba *.out fort.*  
		
