
This folder provides stimuli, raw data, preprocessed data as well as analytic scripts for the study
https://osf.io/8zsc9/

Scripts start analyses from raw data, and analyses are organized by data type.
Experiment 1a: 
	Stimuli in STIMULI/PLT/HUMAN
	Raw preferential looking data in DATA_RAW/plt_adult 
	Analysis of preferential looking times with PLT_HUMAN.R (see description of PLT_HUMAN.R at the end of this file
	Raw data of the memory task in DATA_RAW/behavior_adult/memory
	Analysis of the memory task with MEMORY_TASK.R

Experiment 1b: 
	Stimuli in STIMULI/SURVEY
	Ratings in DATA_RAW/behavior_adult/ratings
	Analysis with SURVEY_ADULT.R

Experiment 2:
	Stimuli in STIMULI/PLT/MONKEYS
	Raw preferential looking data in DATA_RAW/plt_monkeys
	Analysis with PLT_MONKEYS.R	

Experiment 3a:
	Stimuli in STIMULI/PLT/HUMAN
	Raw preferential looking data for each age group in DATA_RAW/plt_7mo, DATA_RAW/plt_10mo, DATA_RAW/plt_15mo, DATA_RAW/plt_18mo
	Analysis with PLT_HUMAN.R (see description of PLT_HUMAN.R at the end of this file

Experiment 3b:
	Stimuli in STIMULI/PLT/HUMAN
	Raw preferential looking data for each age group in DATA_RAW/plt_3yo and DATA_RAW/plt_5yo
	Analysis with PLT_HUMAN.R (see description of PLT_HUMAN.R at the end of this file)



FOLDER STRUCTURE

STIMULI stores the stimuli of all experiments
	- PLT stores the stimuli used in the preferential looking paradigm
		HUMAN gives stimuli used with human adults, children and infants
			FACING gives images of facing dyads, NONFACING gives images of nonfacing dyads
			find_distances.m is a matlab script measuring the distance between bodies on the images of either folder, values are reported in bodies_distance.csv
		MONKEYS provides stimuli for macaques
			FACING gives images of facing dyads, FAWAY gives images of nonfacing dyads
			find_distances.m is a matlab script measuring the distance between bodies on the images of either folder
	- SURVEY stores the stimuli used in the survey of Experiment 1b
			FACING gives images of facing dyads, Facing_Away gives images of nonfacing dyads, Left are bodies turned to the left, Right are bodies turned to the right

DATA_RAW stores the raw data of each experiment
	- Preferential looking data are stored in plt_7mo, plt_10mo, plt_15mo, plt_18mo, plt_3yo, plt_5yo, and plt_adult
		infos_bb.xlsx or infos_adults.xlsx summarise information about participants of the age group
	- behavior_adult stores data for
		the memory task of Experiment 1a are stored in /memory
		the survey of Experiment 1b in /ratings



PLT_HUMAN.R 
	- This script is used to analyze looking times of human adults, children and infants, in Experiment 1a, 3a & 3b.
	- Results of each experiment with the following
		Experiment 1a: 
			Decomment the lines 1356, 1367 in INFORMATIVE TIME WINDOW 
			Decomment the lines 1389-1391 in COMPUTE TIME COURSE OF DIFFERENTIAL LOOKING 
			Run ANALYSIS OF ADULT DIFFERENTIAL LOOKING

		Experiment 3a: 
			Decomment the lines 1357, 1369-1371 in INFORMATIVE TIME WINDOW
			Decomment the lines 1394-1396 in COMPUTE TIME COURSE OF DIFFERENTIAL LOOKING 
			Run ANALYSIS OF INFANT DIFFERENTIAL LOOKING

		Experiment 3b: 
			Decomment the lines 1358, 1375-1377 in INFORMATIVE TIME WINDOW 
			Decomment the lines 1399-1401 in COMPUTE TIME COURSE OF DIFFERENTIAL LOOKING
			Run ANALYSIS OF CHILDREN DIFFERENTIAL LOOKING



