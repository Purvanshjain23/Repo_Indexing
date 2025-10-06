Task: Analyze the AnkitBansal_Sources directory using the specifications outlined in FULL_REPO_ANALYSIS_PROMPT.md.
Repository Path:
C:\Users\PurvanshJain\OneDrive - Programmers.IO\Desktop\Full_Repo_Analysis\AnkitBansal_Sources
Instructions:

Refer to FULL_REPO_ANALYSIS_PROMPT.md for the complete analysis specification.
Use full_repo_analysis.py to perform the analysis. If the script does not exist or requires updates, create or modify it accordingly.
Ensure the script executes a single-pass analysis, scanning each file once and generating both control-flow and data-flow outputs simultaneously.

Expected Outputs:

repository_call_analysis_YYYYMMDD_HHMMSS.csv — Contains all Level 1 program calls with function-level granularity. Includes Record Type column (CALL or LEAF) to distinguish between functions that make calls vs terminal/leaf functions.
repository_errors_YYYYMMDD_HHMMSS.csv — Includes INFO-level entries and error logs.
repository_dds_dependencies_YYYYMMDD_HHMMSS.csv — Lists all DDS dependencies extracted from RPG F-specs and CL DCLF statements.
A comprehensive terminal summary detailing:

Positive findings
Breakdown of INFO entries
Function-level call statistics (CALL vs LEAF records)



Goal:
Execute a complete and efficient dependency analysis in one run. The outputs should collectively form a synchronized control-flow and data-flow dependency map of the IBM i repository, supporting modernization, impact analysis, and compliance tracking.



  Analyze the AnkitBansal_Sources directory following FULL_REPO_ANALYSIS_PROMPT.md:       

  Repository: C:\Users\PurvanshJain\OneDrive -
  Programmers.IO\Desktop\Full_Repo_Analysis\AnkitBansal_Sources

  Follow the specification in FULL_REPO_ANALYSIS_PROMPT.md to analyze all
  files.

  Use full_repo_analysis.py (create/update if needed) to execute the analysis
  efficiently. Use or update full_repo_analysis.py to perform the complete analysis in a single pass — scanning each file once and generating both control-flow and data-flow outputs simultaneously.

  Expected outputs:
  - repository_call_analysis_YYYYMMDD_HHMMSS.csv (all Level 1 calls with function-level granularity, includes Record Type column: CALL/LEAF)
  - repository_errors_YYYYMMDD_HHMMSS.csv (INFO entries + errors)
  - repository_dds_dependencies_YYYYMMDD_HHMMSS.csv → All program-to-file (DDS) dependencies extracted from RPG F-specs and CL DCLF statements.
  - Comprehensive terminal summary with positive results, INFO breakdown, and function-level call statistics (CALL vs LEAF)

Goal:
Execute the entire dependency analysis in one efficient run, producing synchronized outputs that together form a complete control-flow and data-flow dependency map of the IBM i repository for modernization, impact analysis, and compliance tracking.