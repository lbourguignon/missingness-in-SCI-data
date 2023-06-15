#!/bin/bash
#
# The purpose of this script is to submit baseline computation jobs to
# an LSF system in order to speed up job processing.

MEMORY=1024
TIME=02:00

for SIM in 'balanced' 'SygenCompleteNAanalysis'; do
    for OUT in 'lower26' 'lower52'; do
        for SUB in {1..500}; do
            bsub -W $TIME -R "rusage[mem=${MEMORY}]" "python 2_single-imputation.py --subset ${SUB} --outcome ${OUT} --scenario ${SIM}"
        done
    done
done

