# Inputs <br />
FHD inputs various models and catalogs, described below.

## Beam Models <br />

## Sky catalogs <br />
All sky catalogs are located in `catalog_data`.

* **GLEAMIDR4_181_consistent.sav**:	GLEAM Internal Data Release 4 for 181 MHz, with no sources consistent…	4 years ago
* **GLEAM_EGC_catalog.sav**:	Adding full published GLEAM extragalactic catalog as GLEAM_EGC_catalo…	3 years ago
* **GLEAM_EGC_catalog_KGSscale_ssextended.sav**:	KGS-scaled GLEAM with extended model for southern sidelobe source	3 years ago
* **GLEAM_EGC_v2_181MHz.sav**:	GLEAM release version 2 at 181MHz for the full-sky except A-team sources and the galactic plane. Only has point-source information; does not include Gaussian fitting or spectral slopes.
* **GLEAM_plus_rlb2017.sav**:	GLEAM release version 1 at 181MHz for the full-sky with added bright (A-team) sources. It includes a Fornax A component model produced by Patti Carroll with FHD. It does not include Gaussian fitting or spectral slopes.
* **GLEAM_v2_plus_gaussian_sources_rlb2019.sav**:	GLEAM release version 2 at 181MHz for the full-sky with added bright (A-team) sources and some Gaussian source models. This catalog is designed for implementation with FHD's gaussian_source_models branch, which has not yet been fully tested.
* **GLEAM_v2_plus_rlb2019.sav**: GLEAM release version 2 at 181MHz for the full-sky with added bright (A-team) sources. It includes a Fornax A component model produced by Patti Carroll with FHD. It does not include Gaussian fitting or spectral slopes.
* **MRC_calibration_catalog.sav**:	Convert MRC catalog to calibration catalog format	6 years ago
* **MRC_full_radio_catalog.fits**:	Bug fix:MRC catalog	7 years ago
* **component_maps_408locked.fits**:	Add catalog data to repo, and modify paths in wrappers	7 years ago
* **components.fits**:	Add catalog data to repo, and modify paths in wrappers	7 years ago
* **eor01_calibration_source_list.sav**:	New EOR0 and EOR1 source list	6 years ago
* **eor1_calibration_source_list.sav**:	New EoR1 high band deconvolved source list	5 years ago
* **gsm_150MHz.sav**:	Attempt to read coord_sys from diffuse model healpix file, defaulting…	3 years ago
* **lambda_haslam408_dsds.fits**:	Convert haslam binary table to useable format	7 years ago
* **master_sgal_cat.sav**:	Add new KGS+MWACS+MRC master catalog and NGC253 extended model.	4 years ago
* **master_sgal_fornax_cat.sav**:	Add master catalog version with extended fornax model.	4 years ago
* **mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav**:	add eor1 versions	5 years ago
* **mwa_calibration_source_list.sav**:	added fornax model and updated FHD sources	5 years ago
* **mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav**:	Fix merge conflict	5 years ago
* **mwa_calibration_source_list_gleam_kgs_no_fornax.sav**:	Update master catalog for testing.	5 years ago
* **mwa_calibration_source_list_nofornax.sav**:	Add new catalog which is MRC without Fornax A for calibration purposes.	6 years ago
* **mwa_commissioning_source_list.sav**:	Fix flux of extended sources	6 years ago
* **mwa_commissioning_source_list_add_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav**:	add ben fornax model with halfpixeloffset correction	5 years ago
* **mwa_commissioning_source_list_add_FHDaug23deconvolve_fornax_and_VLA_pic.sav**:	add FHD fornax model and setup firstpass eor1 run	5 years ago
* **mwa_galactic_center_catalog.sav**:	Update defaults, pass more keywords to fhd_init	6 years ago
* **planck_map_read.pro**:	Bug fix in wrapper	5 years ago
* **vlssr_and_mwacs_and_ben_fornax_and_pic_vla.sav**:	added catalog with mwacs, Fornax, and Pic along with VLSSR for source…	5 years ago
* **vlssr_dec_gt_m15.sav**:	added vlssr catalogue greater than minus 15 degrees dec to cover area…	5 years ago

* **simulation/eor_power_1d.idlsave**:	Added code for making EoR simulations	6 years ago
* **simulation/flat_power_1d.idlsave**:	Added flat power savefile to catalog data for simulation plots.	5 years ago
* **simulation/RFI_PLAW_1000s_Cat.sav**:	1000 point sources distributed uniformly over hemisphere centered on EoR0 with power law flux density distribution ranging from 0.1 - 100 mJy
* **simulation/RFI_PLAW_10x_Cat.sav**:	100 point sources distributed uniformly over hemisphere centered on EoR0 with power law flux density distribution ranging from 1 mJy - 1 Jy
* **simulation/RFI_PLAW_Cat.sav**:	100 point sources distributed uniformly over hemisphere centered on EoR0 with power law flux density distribution ranging from 0.1 - 100 mJy
* **simulation/test_RFI_source_1061315448_zenith.sav**: A 1 Jy source at zenith for MWA obsid 1061315448

## Calibration files <br />
