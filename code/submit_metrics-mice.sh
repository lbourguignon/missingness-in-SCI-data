#!/bin/bash
#
# The purpose of this script is to submit baseline computation jobs to
# an LSF system in order to speed up job processing.

MEMORY=1024
TIME=02:00

#for SIM in 'balanced'; do
#for SIM in 'balanced' 'EMSCI' 'Murnau' 'SygenAll' 'SygenCompleteNAanalysis' 'SCIRehab'; do
for SIM in 'balanced' 'EMSCI' 'SygenAll' 'SygenCompleteNAanalysis'; do
    for SUB in {1..500}; do
        #for OUT in 'lower26' 'lower52'; do
        for OUT in 'lower52'; do
	    bsub -W $TIME -R "rusage[mem=${MEMORY}]" "python 4_metrics-mice.py --subset ${SUB} --scenario ${SIM} --outcome ${OUT}"
        done
    done
done

