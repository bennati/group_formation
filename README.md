# group_formation
Supporting material to the paper https://arxiv.org/abs/1602.06737

## Requirements:

- A C++ compiler with MPI support is required in order to execute the code. The code has been compiled with Make and the GCC compiler, but other development environments might be compatible as well.
- Data analysis and figures are produced by R, the code relies on the executable ``Rscript`` to run the analysis non-interactively.
- Compilation and startup scripts are written for bash on a *nix system, but other shells might be supported as well.
- The code has support for the LSF platform for parallel execution of clusters, but it can also be run on a single machine.
- The output of the simulation might take large amounts of space on disk, therefore files are zipped after the analysis is completed. The scripts rely on ``gzip`` for compression.

## Instructions:

In order to run the code execute the script ``build.sh``, which will build the appropriate code for each simulation scenario and start the simulation.
Parameters for each scenario are found in the ``params.sh`` file in the respective folder.
After the simulation has completed, the analysis scripts, ``analysis.R`` and ``time_series_3d.R``, are executed. Figures are produced for each simulation in the subfolder ``results``.
Experiments differ in the parameters, contained in the file ``params.sh``, and in the features of the simulation, encoded as compile flags in the Makefile.
