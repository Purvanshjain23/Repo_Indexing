# AI Enhancement Instructions for OMEIPVR

## Overview
This file contains instructions for Claude to enhance the generic documentation
with accurate, function-specific analysis.

## Source Information
- **Program:** OMEIPVR
- **Source File:** shipping_modules\OMEIPVR_RPG.txt
- **Lines of Code:** 1,492
- **Total Functions:** 24
- **Estimated Tokens:** 47,000
- **Estimated Cost:** $0.32

## Instructions for Claude

Please perform the following analysis to enhance the documentation:

### TASK: Generate Complete Documentation with AI Analysis

**Source Code:** `shipping_modules\OMEIPVR_RPG.txt`

**Output Location:** `HTML_Outputs/OMEIPVR/`

**Requirements:**
1. Follow the `IBM_i_Universal_Documentation_Generator_Prompt.md` template exactly
2. Generate multi-file structure (>15 functions)
3. Analyze ACTUAL source code for each function (not generic templates)
4. Create function-specific Mermaid diagrams showing real logic flow
5. Write accurate business narratives based on code analysis

### Functions to Analyze (24 total):

 1. **CAEXFM** (Line 291) - User Interface - Critical Priority
 2. **DAPRSC** (Line 340) - Database Operations - Critical Priority
 3. **DBVLSC** (Line 422) - Validation - Medium Priority
 4. **GADSA1** (Line 596) - Validation - Medium Priority
 5. **MALDSC** (Line 607) - Validation - Medium Priority
 6. **SARVGN** (Line 673) - Database Operations - Critical Priority
 7. **SBRVGN** (Line 702) - Database Operations - Critical Priority
 8. **SCRVGN** (Line 731) - Database Operations - Critical Priority
 9. **SDRVGN** (Line 763) - Database Operations - Critical Priority
10. **SERVGN** (Line 788) - Validation - Medium Priority
11. **SFRVGN** (Line 822) - Validation - Medium Priority
12. **VACKRL** (Line 856) - Validation - Medium Priority
13. **VBCKRL** (Line 897) - Database Operations - Critical Priority
14. **XDCK2** (Line 970) - Validation - Medium Priority
15. **XDVC2T** (Line 1038) - Validation - Medium Priority
16. **Y0SET** (Line 1063) - Screen Control - Low Priority
17. **Y8TST** (Line 1070) - Validation - Low Priority
18. **Y9CLR** (Line 1087) - Screen Control - Low Priority
19. **ZASNMS** (Line 1094) - Validation - Medium Priority
20. **ZDVPMT** (Line 1122) - Validation - Medium Priority
21. **ZHHPKY** (Line 1159) - Business Logic - Medium Priority
22. **ZXEXPG** (Line 1193) - Program Control - Medium Priority
23. **ZYEXPG** (Line 1202) - Program Control - Medium Priority
24. **ZZINIT** (Line 1218) - Database Operations - Critical Priority


### Specific Instructions:

#### Sections 1-7:
Analyze the complete program to create:
1. **Business Context:** Extract from file headers, CALL statements, and file operations
2. **Inputs:** Parse F-specs for database files and parameter PLISTs
3. **Structure:** Analyze EXSR calls to build accurate call tree diagram
4. **Business Logic:** Trace main execution flow from entry point
5. **Logic Flow:** Create detailed process flow diagram with real decision points
6. **Data Operations:** Extract all CHAIN/READ/WRITE/UPDATE operations
7. **Dependencies:** Find all CALL statements and external programs

#### Section 8:
For EACH of the 24 functions:

1. **Read source code** at the specified line number
2. **Analyze actual logic:**
   - Identify input/output parameters
   - Trace IF/CASE/LOOP logic
   - Find EXSR calls to other functions
   - Determine database operations
3. **Generate function-specific Mermaid diagram** showing:
   - Real entry and exit points
   - Actual decision points (IF conditions)
   - Specific EXSR calls made
   - Error handling paths
4. **Write accurate narrative** based on code analysis

### Critical Requirements:
- ✅ Use **Mermaid flowcharts** for ALL diagrams (including Section 8 functions)
- ✅ Create **unique diagrams** for each function (not generic templates)
- ✅ Analyze **actual source code** for each function
- ✅ Use **business-focused language** (non-technical)
- ✅ Generate **complete 4-part analysis** for ALL functions

### Output Files:

- `OMEIPVR_Sections_1-7.html` - Complete Sections 1-7 with Mermaid diagrams
- `OMEIPVR_Section_8_Functions.html` - ALL 24 functions with unique Mermaid diagrams


### Quality Validation:
After generation, the documentation should pass:
```bash
python validate_documentation_quality.py "HTML_Outputs/{program_name}"
```

Expected result:
```
[OK] All functions with UNIQUE diagrams
[OK] Function-specific narratives (not templates)
[OK] Accurate business logic documentation
```

---

## Example Prompt to Use

Copy and paste this into Claude:


I need you to generate complete IBM i business documentation for OMEIPVR.

**Source File:** shipping_modules\OMEIPVR_RPG.txt
**Program Details:**
- Lines of Code: 1,492
- Functions: 24
- Structure: Multi-file (>15 functions)

**Requirements:**
1. Follow IBM_i_Universal_Documentation_Generator_Prompt.md exactly
2. Analyze ACTUAL source code for each function
3. Generate function-specific Mermaid diagrams (not generic templates)
4. Create accurate business narratives based on real logic
5. Use business-focused language for non-technical users

**Output:** HTML_Outputs/OMEIPVR/

**Functions List:**
1. CAEXFM (Line 291)
2. DAPRSC (Line 340)
3. DBVLSC (Line 422)
4. GADSA1 (Line 596)
5. MALDSC (Line 607)
6. SARVGN (Line 673)
7. SBRVGN (Line 702)
8. SCRVGN (Line 731)
9. SDRVGN (Line 763)
10. SERVGN (Line 788)
... and 14 more functions

For Section 8, analyze EACH function's source code at the specified line number
and create unique, accurate Mermaid diagrams showing the real logic flow.

Generate complete documentation now.
