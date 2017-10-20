# An example script to generate calfits from pointing/cable calibration txt files,
# modified from the example script on the pyuvdata github. 

from pyuvdata import UVCal
import numpy as np

# Time array is in JD and calculated at the center of the time sample, and the corresponding pointing number.
# Best to reference pointing JDs from Aug 23, 2013 (though not required)
time_array = [2456528.21023,2456528.23208,2456528.25329,2456528.27444,2456528.29565]
poi_array = ['-2','-1','0','1','2']
#
# large reference time array = [2456528.18694,2456528.21023,2456528.23208,2456528.25329,2456528.27444,2456528.29565,2456528.31894,2456528.34194]
# for pointings ['-3','-2','-1','0','1','2','3','4']
#
Ntimes = len(time_array)

# Frequency array of the cal in Hz
freq_array = np.linspace(1.67075e8, 1.97715e8, 384)  # highband
# freq_array for lowband is np.linspace(1.38915e8, 1.69555e8, 384)

Nfreqs = len(freq_array)
jones_array = np.array([-5, -6])  #  only 2 jones parameters, jxx and jyy.
Njones = len(jones_array)
ant_array = np.arange(128)
Nants_data = len(ant_array)
antenna_names = np.array(['ant{0}.format(ant)' for ant in ant_array])
Nspws = 1  # only 1 spw is supported

# Generate data arrays
gains = np.zeros((Nants_data,Nfreqs,Ntimes,Njones)) + 0j
flags = np.ones_like(gains, dtype=np.bool)
chisq = np.ones_like(gains, dtype=np.float32)

### Get cable lengths to sort calibration solutions to the right tiles
datafile_name='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/instrument_config/mwa_cable_length.txt'
datafile = open(datafile_name, "r")
temp = next(datafile)
cols = (col.strip().split() for col in datafile)
rows = zip(*cols)

tile_index = np.asarray(rows[0], dtype=np.int16)
cable_length = np.asarray(rows[2], dtype=np.float32)

datafile.close()

index90 = np.where(cable_length == 90)
index150 = np.where(cable_length == 150)
index230 = np.where(cable_length == 230)
index320 = np.where(cable_length == 320)
index400 = np.where(cable_length == 400)
index524 = np.where(cable_length == 524)
###

for poi_i, poi in enumerate(poi_array)):

	### Get cable fits
	#
	# Change this datafile name to match the txt tile format
	#
	datafile_name='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/instrument_config/'+poi+'_bandpass.txt'
	datafile = open(datafile_name, "r")
	cols = (col.strip().split() for col in datafile)
	rows = zip(*cols)

	x90 = np.asarray(rows[1], dtype=np.float32) + 0j
	y90 = np.asarray(rows[2], dtype=np.float32) + 0j
	x150 = np.asarray(rows[3], dtype=np.float32) + 0j
	y150 = np.asarray(rows[4], dtype=np.float32) + 0j
	x230 = np.asarray(rows[5], dtype=np.float32) + 0j
	y230 = np.asarray(rows[6], dtype=np.float32) + 0j
	x320 = np.asarray(rows[7], dtype=np.float32) + 0j
	y320 = np.asarray(rows[8], dtype=np.float32) + 0j
	x400 = np.asarray(rows[9], dtype=np.float32) + 0j
	y400 = np.asarray(rows[10], dtype=np.float32) + 0j
	x524 = np.asarray(rows[11], dtype=np.float32) + 0j
	y524 = np.asarray(rows[12], dtype=np.float32) + 0j

	gains[index90, :, poi_i, 0] = x90 
	gains[index90, :, poi_i, 1] = y90 
	gains[index150, :, poi_i, 0] = x150 
	gains[index150, :, poi_i, 1] = y150 
	gains[index230, :, poi_i, 0] = x230 
	gains[index230, :, poi_i, 1] = y230 
	gains[index320, :, poi_i, 0] = x320 
	gains[index320, :, poi_i, 1] = y320 
	gains[index400, :, poi_i, 0] = x400 
	gains[index400, :, poi_i, 1] = y400
	gains[index524, :, poi_i, 0] = x524
	gains[index524, :, poi_i, 1] = y524

	datafile.close()
	###

cal = UVCal()
cal.cal_type = 'gain'
cal.set_gain()
cal.Nfreqs = Nfreqs
cal.Njones = Njones
cal.Ntimes = Ntimes
#
# Change the history comment to list field, freq range name, instrument, averaging sample set, pointing JD reference,
# calibration catalogs, and whatever else is important.
#
cal.history = 'EXAMPLE HISTORY, PLEASE CHANGE: EoR0 highband per frequency, per pointing, per polarization bandpass for MWA, averaged per cable over Season 1 using an early version of KGS. Pointing JD is referenced from Aug 23,2013.'
#
cal.Nspws = 1
cal.freq_array = freq_array.reshape(cal.Nspws, -1)
cal.freq_range = [freq_array[0], freq_array[-1]]  # valid frequencies for solutions.
cal.channel_width = np.diff(freq_array)[0]
cal.jones_array = jones_array
cal.time_array = time_array
#
# Pointing integration time
#
cal.integration_time = 1800.
#
cal.gain_convention = 'divide'  # Use this operation to apply gain solution.
cal.x_orientation = 'east'  # orientation of 1st jones parameter.
#
# JD's this can applied to. Below is Season 1
#
cal.time_range = [2456528., 2456626.]
#
cal.telescope_name = 'MWA'
cal.Nants_data = Nants_data
cal.Nants_telescope = Nants_data  # have solutions for all antennas in array.
cal.ant_array = ant_array
cal.antenna_names = antenna_names
cal.antenna_numbers = ant_array
cal.flag_array = flags
cal.gain_array = gains
cal.quality_array = chisq
#
# Put your name in as creator
#
cal.observer = '<YOUR NAME HERE>'
#
# Put in the git url of the code that generated the cals
#
cal.git_origin_cal = 'https://github.com/EoRImaging/FHD'
#
# And if you know the git hash, put that in as well
#
#cal.git_hash_cal = 

# Finally, generate an output!
cal.write_calfits('<put in a filepath>/<put in an instrument>_<put in a field>_<put in a frequency range name>_<put in a season/day>_cable_bandpass.fits')
