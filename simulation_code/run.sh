#!/bin/bash
date
whoami
echo "running test now on euler..."
amin=50
bin_in=0
direct_feedback=0
antisocial_ratio=0.0
fmax=200
sizes=(20)
ages=(1000)
foods=(20)
fovs=(1)
srs=(0.1)

num_runs=50000
samples=24

# executable=test_nointeractions
source $1
if [ $# -eq 2 ]; then
    executable=$2
else
    executable="./test"
fi
echo "running executable $executable"

for num_agents in "${sizes[@]}"; do
  for age in "${ages[@]}"; do
    for num_food in "${foods[@]}"; do
      for fov_radius in "${fovs[@]}"; do
        for social_ratio in "${srs[@]}"; do
          format=measurement_NF"$num_food"_A"$age"_FR"$fov_radius"_N"$num_agents"_SR"$social_ratio"
          mkdir -p $format
          cd $format
          mkdir -p results
          jname="$executable"_"$num_agents"_"$num_food"_"$fov_radius"
          ## check if bsub is present
          if command -v bsub >/dev/null 2>&1; then
              prefix=(bsub -W 4:00 -J $jname -n $samples) # -R "rusage[mem=2000]"
              prefix2=(bsub -R "rusage[mem=20000]" -W 24:00 -J "analysis_$jname" -n 1 -w "done($jname)")
              prefix3=(bsub -W 4:00 -R "rusage[mem=64000]" -J "analysis_ts_$jname" -n 1 -w "done($jname)")
              prefix4=(bsub -n 1 -J "cat_$jname" -w "done($jname)")
              prefix5=(bsub -W 4:00 -n 1 -w "done(cat_$jname) && done(analysis_$jname) && done(analysis_ts_$jname)" -J "zip_$jname")
          else
            prefix=()
            prefix2=()
            prefix3=()
            prefix4=()
            prefix5=()
            prefix6=()
          fi
          command=( "${prefix[@]}" # combine arrays
                    mpirun -np $samples ../$executable $num_agents $amin $num_food $fmax $num_runs $samples $age $fov_radius $bin_in $direct_feedback $social_ratio $antisocial_ratio)
          "${command[@]}"
          command2=( "${prefix2[@]}" Rscript ../../analysis_code/analysis.R)
          "${command2[@]}"
          command3=( "${prefix3[@]}" Rscript ../../analysis_code/time_series_3d.R )
          "${command3[@]}"
          command4=( "${prefix4[@]}" "cat results/report*.csv > report.csv") # unify reports in one file
          "${command4[@]}"
          command5=( "${prefix5[@]}" "gzip ./results/*; gzip ./time_series_data.csv")
          "${command5[@]}"
          cd ..
        done
      done
    done
  done
done
