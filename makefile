#FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -fPIC  -align array64byte
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX -ftz  -fast-transcendentals

FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -fPIC -r8
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full

#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I185274/ifort $(FRTFLAGS) $(OPT_FRTFLAGS)
FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I185274/ifort $(FRTFLAGS) -g -O0

#FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays


all: wrap_shallow_mf.x

%.o: %.F90
	$(FC) -o $@ -c $< 

parkind1.o: parkind1.F90 
	$(FC) -c parkind1.F90

xrd_getoptions.o: xrd_getoptions.F90 xrd_unix_env.o parkind1.o
	$(FC) -c xrd_getoptions.F90

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90

load_mod.o: load_mod.F90 parkind1.o
	$(FC) -c load_mod.F90

wrap_shallow_mf.o: wrap_shallow_mf.F90 xrd_getoptions.o modi_shallow_mf.o load_mod.o
	$(FC) -c wrap_shallow_mf.F90

shallow_mf_load_all.o: shallow_mf_load_all.F90 modd_cst.o modd_cmfshall.o modd_cturb.o load_mod.o modd_neb.o
	$(FC) -c shallow_mf_load_all.F90

shallow_mf.o: shallow_mf.F90 modd_cst.o modd_cmfshall.o modd_parameters.o modi_thl_rt_from_th_r_mf.o modi_compute_updraft.o modi_mf_turb.o modi_compute_mf_cloud.o modi_compute_frac_ice2d.o
	$(FC) -c shallow_mf.F90

compute_frac_ice2d.o: compute_frac_ice2d.F90 modi_compute_frac_ice1d.o
	$(FC) -o $@ -c $< 

thl_rt_from_th_r_mf.o: thl_rt_from_th_r_mf.F90
	$(FC) -o $@ -c $< 

compute_updraft.o: compute_updraft.F90 modd_cst.o modd_cmfshall.o modi_compute_entr_detr.o modi_th_r_from_thl_rt_1d.o modi_compute_bl89_ml.o modi_shuman_mf.o
	$(FC) -o $@ -c $< 

compute_mf_cloud.o: compute_mf_cloud.F90 modi_compute_mf_cloud_direct.o 
	$(FC) -o $@ -c $< 

mf_turb.o: mf_turb.F90 modd_cmfshall.o modi_shuman_mf.o modi_tridiag_massflux.o
	$(FC) -o $@ -c $< 

shuman_mf.o: shuman_mf.F90
	$(FC) -o $@ -c $< 

compute_frac_ice1d.o: compute_frac_ice1d.F90 modd_neb.o modd_cst.o
	$(FC) -o $@ -c $< 

th_r_from_thl_rt_1d.o: th_r_from_thl_rt_1d.F90 modi_compute_frac_ice1d.o modd_cst.o mode_thermo_mono.o
	$(FC) -o $@ -c $< 

mode_thermo_mono.o: mode_thermo_mono.F90 mode_fm.o
	$(FC) -o $@ -c $< 

tridiag_massflux.o: tridiag_massflux.F90 modd_parameters.o modi_shuman_mf.o modd_cturb.o modd_parameters.o 
	$(FC) -o $@ -c $< 

compute_bl89_ml.o: compute_bl89_ml.F90
	$(FC) -o $@ -c $< 

compute_entr_detr.o: compute_entr_detr.F90 modd_cst.o modd_cmfshall.o mode_thermo_mono.o modi_th_r_from_thl_rt_1d.o
	$(FC) -o $@ -c $< 

compute_mf_cloud_direct.o: compute_mf_cloud_direct.F90 modd_cmfshall.o
	$(FC) -o $@ -c $< 

compute_function_thermo_mf.o: compute_function_thermo_mf.F90 modd_cst.o
	$(FC) -o $@ -c $< 

wrap_shallow_mf.x: wrap_shallow_mf.o shallow_mf_load_all.o shallow_mf.o compute_frac_ice2d.o thl_rt_from_th_r_mf.o compute_updraft.o compute_mf_cloud.o mf_turb.o compute_frac_ice1d.o th_r_from_thl_rt_1d.o tridiag_massflux.o compute_bl89_ml.o compute_entr_detr.o compute_mf_cloud_direct.o compute_function_thermo_mf.o shuman_mf.o
	$(FC) -o wrap_shallow_mf.x wrap_shallow_mf.o xrd_getoptions.o xrd_unix_env.o load_mod.o shallow_mf_load_all.o modd_cturb.o modd_cmfshall.o modd_cst.o shallow_mf.o compute_frac_ice2d.o thl_rt_from_th_r_mf.o compute_updraft.o compute_mf_cloud.o mf_turb.o compute_frac_ice1d.o shuman_mf.o modd_neb.o th_r_from_thl_rt_1d.o tridiag_massflux.o compute_bl89_ml.o compute_entr_detr.o compute_mf_cloud_direct.o compute_function_thermo_mf.o

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

