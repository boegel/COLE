import Data.List (intersperse)
import System.Random (mkStdGen)

import COLE

main :: IO()
main = do
    let baseFlags = ["-O1","-O2","-O3"]
        optFlags = ["-fcase-merge",
                    "-fno-case-merge",
                    "-fdicts-strict",
                    "-fno-dicts-strict",
                    "-fmethod-sharing",
                    "-fno-method-sharing",
                    "-fdo-eta-reduction",
                    "-fno-do-eta-reduction",
                    "-fdo-lambda-eta-expansion",
                    "-fno-do-lambda-eta-expansion",
                    "-fexcess-precision",
                    "-fno-excess-precision",
                    "-fignore-asserts",
                    "-fno-ignore-asserts",
                    "-fignore-interface-pragmas",
                    "-fno-ignore-interface-pragmas",
                    "-fomit-interface-pragmas",
                    "-fno-omit-interface-pragmas",
                    --"-fmax-worker-args",
                    --"-fsimplifier-phases",
                    --"-fmax-simplifier-iterations",
                    "-fno-state-hack",
                    "-fcse",
                    "-ffull-laziness",
                    "-fenable-rewrite-rules", --"-frewrite-rules",
                    "-fno-enable-rewrite-rules", --"-fno-rewrite-rules",
                    "-fstrictness",
                    "-fspec-constr",
                    --"-fspec-constr-threshold=n",
                    "-fno-spec-constr-threshold",
                    --"-fspec-constr-count=n",
                    "-fno-spec-constr-count",
                    "-fliberate-case",
                    "-fstatic-argument-transformation",
                    --"-fliberate-case-threshold=n",
                    "-fno-liberate-case-threshold",
                    "-funbox-strict-fields",
                    "-fno-unbox-strict-fields",
                    --"-funfolding-creation-threshold",
                    "-fno-unfolding-creation-threshold",
                    --"-funfolding-fun-discount",
                    "-fno-unfolding-fun-discount",
                    --"-funfolding-keeness-factor",
                    "-fno-unfolding-keeness-factor",
                    --"-funfolding-use-threshold",
                    "-fno-unfolding-use-threshold",
                    "-fno-pre-inlining",
                    "-feager-blackholing"]
        cfg = COLEConfig
          10 -- number of experiments per generation
          5 -- max. number of Pareto-optimal solutions
          25 -- max. number of generations
          ("test_label_seed" ++ (show seed)) -- label for COLE experiment
          ByValue -- type of fitness scoring
          (LocalEvaluation "./COLE_eval_entity_execTime.sh") -- evaluation type
          (Value LowerIsBetter) -- type of evaluation result
          5 -- number of experiments per entity
          baseFlags -- ^ available base flags
          optFlags -- ^ available optimization flags

        seed = 0
        g = mkStdGen seed

    es <- cole g cfg

    putStrLn "\nlist of Pareto-optimal entities: \n"
    putStrLn $ concat $ intersperse "\n" $ map show es
