pisaedTable1REF <- c("", "Formula: math ~ st04q01 + st20q01 ", "", "Plausible values: 5", 
                     "jrrIMax: 1", "Weight variable: 'w_fstuwt'", "Variance method: jackknife", 
                     "JK replicates: 80", "full data n: 4978", "n used: 4873", "", 
                     "", "Summary Table:",
                     " st04q01         st20q01    N     WTD_N       PCT   SE(PCT)     MEAN  SE(MEAN)", 
                     "  Female Country of test 2234 1575416.5 92.425753 0.8038984 481.1794  4.019205", 
                     "  Female   Other country  180  129104.6  7.574247 0.8038984 463.6272 10.355889", 
                     "    Male Country of test 2272 1611412.6 91.796017 0.9558501 486.8819  3.853613", 
                     "    Male   Other country  187  144015.0  8.203983 0.9558501 474.1468  9.474347"
)

plm1REF <- c("             (Intercept)             st29q06Agree          st29q06Disagree st29q06Strongly disagree           sc01q01Private ", 
             "              506.993125               -21.828757               -32.381549               -52.944871                 2.408131 "
)

pgap1REF <- c("Call: gap(variable = \"math\", data = usaINT2012, groupA = st04q01 == ", 
              "    \"Male\", groupB = st04q01 == \"Female\", weightVar = \"w_fstuwt\")", 
              "", "labels:", " group          definition nFullData nUsed", 
              "     A   st04q01 == \"Male\"      4978  2525", "     B st04q01 == \"Female\"      4978  2453", 
              "", "percentage:", "     pctA    pctAse     pctB    pctBse   diffAB      covAB diffABse diffABpValue    dofAB", 
              " 50.98087 0.7182871 49.01913 0.7182871 1.961734 -0.5159363 1.436574     0.174861 110.0328", 
              "", "results:", " estimateA estimateAse estimateB estimateBse   diffAB    covAB diffABse diffABpValue    dofAB", 
              "   483.647    3.800262  478.9953     3.92109 4.651662 11.01904 2.789059   0.09911296 83.14098"
)

al1REF <- c("", "AchievementVars: cpro", "aggregateBy: st04q01", "", "Achievement Level Cutpoints:", 
            "358.49 423.42 488.35 553.28 618.21 683.14 ", "", "Plausible values: 5", 
            "jrrIMax: 1", "Weight variable: 'w_fstuwt'", "Variance method: jackknife", 
            "JK replicates: 80", "full data n: 5177", "n used: 5177", "", 
            "", "Discrete", "                     Level st04q01     N      wtdN   Percent StandardError", 
            " Below Proficiency Level 1  Female  95.4  1541.697  3.539350     0.7086689", 
            "    At Proficiency Level 1  Female 234.2  3815.988  8.760555     0.8224039", 
            "    At Proficiency Level 2  Female 524.6  8669.991 19.904132     0.9013387", 
            "    At Proficiency Level 3  Female 771.0 12722.017 29.206570     1.3662494", 
            "    At Proficiency Level 4  Female 644.6 10711.838 24.591702     1.3001839", 
            "    At Proficiency Level 5  Female 300.8  4985.134 11.444621     1.1955623", 
            "    At Proficiency Level 6  Female  66.4  1112.086  2.553071     0.5394964", 
            " Below Proficiency Level 1    Male  68.8  1083.360  2.616423     0.5358135", 
            "    At Proficiency Level 1    Male 167.6  2578.450  6.227218     0.6980039", 
            "    At Proficiency Level 2    Male 383.8  6222.924 15.028992     1.1775214", 
            "    At Proficiency Level 3    Male 648.8 10591.764 25.580183     1.3363213", 
            "    At Proficiency Level 4    Male 696.6 11515.983 27.812266     1.7109083", 
            "    At Proficiency Level 5    Male 430.6  7036.873 16.994760     1.1910320", 
            "    At Proficiency Level 6    Male 143.8  2376.777  5.740157     0.6795269"
)

pgap2REF <- c("gapList", "Call: gap(variable = \"math\", data = usaINT2012, groupA = st04q01 == ", 
              "    \"Male\", groupB = st04q01 == \"Female\", percentiles = c(50, ", 
              "    90), weightVar = \"w_fstuwt\")", "", "labels:", " group          definition", 
              "     A   st04q01 == \"Male\"", "     B st04q01 == \"Female\"", 
              "", "percentage:", "     pctA    pctAse     pctB    pctBse   diffAB      covAB diffABse diffABpValue    dofAB", 
              " 50.98087 0.7182871 49.01913 0.7182871 1.961734 -0.5159363 1.436574     0.174861 110.0328", 
              "", "results:", " percentiles estimateA estimateAse estimateB estimateBse   diffAB     covAB diffABse diffABpValue    dofAB", 
              "          50  480.7158    4.067997  474.6257    4.497721 6.090035 11.419570 3.733492    0.1071387 73.31812", 
              "          90  605.0812    3.919613  595.3616    8.595678 9.719676  7.819979 8.579574    0.2602487 90.68492"
)

