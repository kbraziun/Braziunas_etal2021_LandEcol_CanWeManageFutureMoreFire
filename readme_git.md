# readme for Braziunas et al. 2021 simulation runs and data prep

## see also

This git repository is a copy of a permanent data deposit with the Environmental Data
Initiative. It is also archived here for easier viewing of associated scripts, and this
repository does not include data, model inputs and executable, and other large files.
To download the full data deposit, go to:

**Data desposit citation:** Braziunas, K.H., R. Seidl, W. Rammer, and M.G. Turner. 2020. Can we manage a future with more fire? Effectiveness of defensible space treatment depends on housing amount and configuration ver 1. Environmental Data Initiative. [https://doi.org/10.6073/pasta/696e59acecd0bd289dae1afe3316c09c](https://doi.org/10.6073/pasta/696e59acecd0bd289dae1afe3316c09c).

## purpose

This readme gives an overview of directory structure, files, and steps for recreating
outputs and analyses associated with wildland urban interface fire risk simulation runs for
Braziunas et al. 2021.

**Manuscript citation:** Braziunas, K.H., Seidl, R., Rammer, W. et al. Can we manage a future with more fire? Effectiveness of defensible space treatment depends on housing amount and configuration. Landscape Ecol 36, 309–330 (2021). [https://doi.org/10.1007/s10980-020-01162-x](https://doi.org/10.1007/s10980-020-01162-x)

## organization

Directory structure and files (refers to EDI data deposit, not all folders and files will
appear in this github repository):

- `outputs/`: processed and summarized iland and fire risk analysis outputs.
  - `iland_outputs/runs_processed/`: processed iland outputs will be stored here after running
      `iland_run_local.sh`.
  - `analysis/fire_risk/`: compiled fire risk for all scenarios, all replicates will be stored
      here after running `fire_risk_prep.R`.
  - `analysis/output_summaries/`: output summary CSVs will be stored here after running
      `output_summaries.R`.
- `programs/the_model/`
  - `executable_qt512/`: compiled executable version of iland.
  - `NR/`: initialization data, project files, scripts, etc. associated with running iland
      for Braziunas et al. 2020. See model documentation in Seidl & Rammer 2020
      [http://iland.boku.ac.at/](http://iland.boku.ac.at/).
  - `src_20190503.zip`: iland source code.
- `scripts/`: scripts to run iland and recreate fire risk and summarized outputs, with the
  exception of initial output processing scripts (located in `programs/the_model/NR/scripts/`).
  - `run_iland/`: bash scripts to run simulation replicates and perform initial output processing.
      Also includes scripts for spinup and fire intensity test runs.
  - `analysis/`: R scripts to create summarized fire risk, area burned, forest, and fuels outputs
      across all replicates and over duration of simulation. Also includes R script for
      analyses associated with the manuscript.

## platforms

- Operating systems and software used for development and implementation
  - OS: Windows 10 Pro
  - iLand version: version 1.09, SVN-Revision 1399
  - QT library: 5.12.2
  - R version: 3.6.1

## implementation

To run simulation replicates, run from main directory:

```bash
# run 20 reps, all scenarios
bash scripts/run_iland/iland_run_local.sh 20
```

For each replicate, this runs the model and a series of two output processing R scripts
to first synthesize and condense outputs and second estimate fire intensity from fuels
and fire weather.

Next, to generate summary outputs across all replicates, run:

```bash
Rscript.exe scripts/analysis/fire_risk_prep.R
Rscript.exe scripts/analysis/output_summaries.R
```

Finally, rerun and view analyses in R using `scripts/analysis/fire_risk_analysis.R`.
