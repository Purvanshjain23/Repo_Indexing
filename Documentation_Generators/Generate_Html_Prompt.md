 Use the IBM_i_Universal_Documentation_Generator_Prompt.md to analyze this AS/400 code.

  File Name: C:\Users\PurvanshJain\OneDrive - Programmers.IO\Desktop\Full_Repo_Analysis\shipping_modules/PBMHDFR_RPG.txt

  Output Location : HTML Outputs Folder

  Instructions:
  - Follow the universal prompt template exactly
  - Target audience: Non-technical business users
  - Purpose: Modernization/rewrite business requirements
  - Structure:
    - 8 main sections for overall program documentation (Sections 1-7: program-level, Section 8: function-level)
    - Section 8 only: 4-part analysis for EACH subroutine/function
  - File structure: Single HTML file (≤15 functions) or Multi-file subfolder (>15 functions)
  - Diagrams: Mermaid flowcharts for all sections (1-8) with syntax validation
  - Coverage in terminal : How much time you spent on this analysis, tokens consumed, cost incurred and model used.

  Expected Output:
  - Professional HTML business documentation suitable for executive review and modernization planning
  - Mermaid diagrams with rendering verification
  - Complete function catalog
  - Business-focused language throughout


Generate complete IBM i documentation for OMEIPVR from shipping_modules/OMEIPVR_RPG.txt
python generate_complete_documentation.py "shipping_modules/PBBIXFR_RPG.txt" "PBBIXFR"
python stage1_generate_structure.py "shipping_modules/PBMHDFR_RPG.txt" "PBMHDFR"
python lightweight_doc_generator.py "shipping_modules/PBMHDFR_RPG.txt" "PBMHDFR"
python universal_ibm_i_doc_generator.py "shipping_modules\CAALXFR_RPG.txt" "CAALXFR" "HTML_Outputs"
python ai_powered_doc_generator.py "shipping_modules\CAALXFR_RPG.txt" "CAALXFR"

 
ICQ9DFR

C:\Users\PurvanshJain\OneDrive - Programmers.IO\Desktop\Full_Repo_Analysis\







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