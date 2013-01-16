import System.Random (mkStdGen)

import COLE

main :: IO()
main = do
    let qsubArgs = "-l walltime=1:00:00 -q short -l nodes=1:ppn=8"
        jobsDir = "COLEjobs"
        jobsScript = "COLE_eval_entity_job.sh"
        baseFlags = ["-O1","-O2","-O3"]
        optFlags = map (('-':) . return) ['a'..'z']
        cfg = COLEConfig
          10 -- number of experiments per generation
          5 -- max. number of Pareto-optimal solutions
          3 -- max. number of generations
          ("PBStest_label_seed" ++ (show seed)) -- label for COLE experiment
          (FuzzyDominance 0.001) -- type of fitness scoring
          (EvaluationViaPBS qsubArgs jobsDir jobsScript) -- evaluation type
          (Tuple (LowerIsBetter,LowerIsBetter)) -- type of evaluation result
          5 -- number of experiments per entity
          baseFlags -- available base flags
          optFlags -- available optimization flags

	seed = 0
        g = mkStdGen seed

    es <- cole g cfg

    putStrLn $ show $ head es
