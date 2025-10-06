# Comprehensive Repository-Wide Program Analysis Prompt with Progress Indexing

## Instructions
Intelligently analyze ALL files in the provided repository folder and extract **TWO TYPES OF RELATIONSHIPS** from every file:

1. **Program Call Relationships** (Control Flow): LEVEL 1 program calls, function calls, and subroutine calls
2. **DDS File References** (Data Flow): F-spec declarations and DCLF statements showing file dependencies

This prompt is **FULLY AGENTIC** - it adapts to any repository size and file type distribution with comprehensive progress tracking.

**SCOPE**: Analyzes ENTIRE REPOSITORY (any size) in batch mode with alphabetical processing and indexed progress tracking.

**COVERAGE**: Dynamically handles ALL file types found in IBM i repositories - automatically detects and adapts to whatever file types are present.

**DUAL OUTPUT**: Multiple CSV files with results aggregated in alphabetical order by filename with real-time progress indexing:
- **Call Analysis CSV**: Program → Program relationships (control flow)
- **DDS Dependency CSV**: Program → File references (data flow)
- **Error/Info CSV**: Processing logs and classifications

**LEVEL 1 FOCUS**: Extract only direct calls (Level 1) - do not follow the call chain deeper.

**PROGRESS TRACKING**: Each file processed shows index number (X of Y) for complete visibility.

## File Type Detection and Analysis Rules

### 1. RPG Programs (Extensions: _RPG.txt, or RPG-like syntax)
**Detection patterns:**
- Files ending with `_RPG.txt`
- Files containing RPG-style indicators (`F`, `D`, `C`, `P` specifications)
- Files with `CALL`, `EXSR`, `CALLP`, `EVAL` statements
- Files with `H/TITLE`, `CRTRPGPGM`, `CRTBNDRPG` headers

**Extraction rules for PROGRAM CALLS:**
- `CALL 'PROGRAMNAME'` or `CALL PROGRAMNAME`
- `EXSR SUBROUTINENAME`
- `CALLP PROCEDURENAME`
- `EVAL FUNCTION()`
- `MONITOR` and `ON-ERROR` blocks
- Service program calls and APIs

**Extraction rules for DDS FILE REFERENCES (F-specs):**
- **Fixed format F-specs** (column-specific):
  ```rpg
  FCUSTPF    IF   E           K DISK       (Database file, Input)
  FORDFILE   O    F             PRINTER    (Print file, Output)
  FCUSTDSP   CF   E             WORKSTN    (Display file, Combined)
  ```
  - Positions 7-16: File name (e.g., CUSTPF, ORDFILE, CUSTDSP)
  - Position 17: File type (I=Input, O=Output, U=Update, C=Combined)
  - Position 18: File designation (blank, P=Primary, S=Secondary, F=Full procedural)
  - Position 19: File format (E=Externally described, F=Program described)
  - Positions 36-42: Device type (DISK, WORKSTN, PRINTER, SPECIAL)

- **Free format F-specs** (modern RPG):
  ```rpg
  DCL-F CUSTPF USAGE(*INPUT) KEYED;
  DCL-F ORDFILE USAGE(*OUTPUT) DISK;
  DCL-F CUSTDSP WORKSTN USAGE(*UPDATE);
  ```

**IMPORTANT**: Extract BOTH program calls AND F-specs from RPG files in the same pass

### 2. Action Diagram Files (Extension: _AD.txt)
**Detection patterns:**
- Files ending with `_AD.txt`
- Contains structured pseudocode with indentation
- Uses `CALL PROGRAM` syntax

**Extraction rules:**
- `CALL PROGRAM 'PROGRAMNAME'`
- `CALL PROGRAM "PROGRAMNAME"`
- Function and procedure calls within action diagrams

### 3. Control Language Programs (Extensions: _CL.txt, _CLP.txt, CL.txt, CLP.txt, or CL-like syntax)
**Detection patterns:**
- Files ending with `_CL.txt`, `_CLP.txt`, `CL.txt`, `CLP.txt`
- Files containing `PGM`, `DCL`, `CALL PGM` statements
- Files with CLP-style comments (`/* */`)
- Control language specific commands

**Extraction rules for PROGRAM CALLS:**
- `CALL PGM(PROGRAMNAME)`
- `CALL PGM('PROGRAMNAME')`
- `SBMJOB CMD(CALL PGM(PROGRAMNAME))`
- System commands that invoke programs

**Extraction rules for DDS FILE REFERENCES (DCLF statements):**
- **DCLF (Declare File) statements**:
  ```cl
  DCLF FILE(CUSTPF)                    (Database file declaration)
  DCLF FILE(ORDFILE) RCDFMT(ORDREC)    (File with record format)
  DCLF FILE(CUSTDSP)                   (Display file declaration)
  ```
  - Extract file name from FILE(filename) parameter
  - Track file usage based on subsequent operations (RCVF=Input, SNDF=Output)

**IMPORTANT**: Extract BOTH program calls AND DCLF statements from CL files in the same pass

### 4. Specialized IBM i File Types (Extensions: UFR, XFR, DFR, PVR, EFR, UPR, UPC, BV, etc.)
**Detection patterns:**
- Files with specialized suffixes: `UFR.txt`, `XFR.txt`, `DFR.txt`, `PVR.txt`, `EFR.txt`, `UPR.txt`, `UPC.txt`, `BV.txt`, etc.
- These are typically RPG or CL programs with specific IBM i naming conventions
- May contain generated code from tools like SYNON/2E

**Extraction rules:**
- Auto-detect underlying language (RPG/CL) from syntax
- Apply RPG rules if RPG syntax detected (F-specs, C-specs, etc.)
- Apply CL rules if CL syntax detected (PGM, DCL, etc.)
- Look for program calls in comments and documentation
- Handle SYNON-generated call patterns

### 5. SQL Query Files
**Detection patterns:**
- Files containing `SELECT`, `INSERT`, `UPDATE`, `DELETE` statements
- Files with SQL-style comments (`--`)
- Database query structures

**Extraction rules:**
- Stored procedure calls: `CALL PROCEDURE_NAME`
- Function calls: `FUNCTION_NAME()`
- Embedded SQL in host programs

### 6. Display/Print Format Files (Extensions: DSPF, PRTF, or similar patterns)
**Detection patterns:**
- Files with display format specifications
- Print format definitions
- Screen and report layouts

**Extraction rules:**
- References to programs in format specifications
- Validation programs and edit codes

### 7. COBOL Programs (if present)
**Detection patterns:**
- Files containing COBOL syntax (`IDENTIFICATION DIVISION`, `PROCEDURE DIVISION`)
- COBOL-style comments (`*` in column 7)
- `CALL` statements in COBOL format

**Extraction rules:**
- `CALL 'PROGRAMNAME'`
- `CALL PROGRAMNAME`
- Copybook includes (`COPY COPYNAME`)

### 8. JCL/Job Control Files (if present)
**Detection patterns:**
- Job control language syntax
- `//` job statements
- `EXEC PGM=` statements

**Extraction rules:**
- `EXEC PGM=PROGRAMNAME`
- Job step program calls

### 9. Mixed/Unclassified Files
**Detection patterns:**
- Files without clear suffix patterns
- Mixed content files
- Utility and configuration files
- Test files and documentation

**Extraction rules:**
- Scan for any recognizable call patterns from above categories
- Look for program references in comments and documentation
- Check for embedded program names in text

## Analysis Output Format with Indexing

This analysis generates **THREE CSV files**:

### 1. Call Analysis CSV (Program → Program Relationships)

Create a table with the following columns including program index:

| Program Index | Caller | Caller Function | Call Type | Callee Program | Callee Function | File Type | Source File |
|---------------|--------|----------------|-----------|----------------|-----------------|-----------|-------------|
| 1 | MAINPGM | MAIN | CALL | CUSTLOOKUP | CUSTLOOKUP | RPG | MAINPGM_RPG.txt |
| 1 | MAINPGM | CALCPROCESS | EXSR | MAINPGM | CALCAMT | RPG | MAINPGM_RPG.txt |
| 1 | MAINPGM | DATEPROC | CALLP | DATESRV | GETDATE | RPG | MAINPGM_RPG.txt |
| 2 | CONTROLPGM | MAIN | CALL PGM | REPORTGEN | REPORTGEN | CL | CONTROLPGM_CL.txt |
| 3 | ACTIONPGM | PROCESS | CALL PROGRAM | VALIDATE | VALIDATE | AD | ACTIONPGM_AD.txt |
| 4 | LEAFPGM | MAIN | (none) | (none) | (none) | RPG | LEAFPGM_RPG.txt |

**IMPORTANT - Leaf Node Handling:**
- **Every executable file (RPG, CL, AD) must appear in the output at least once**
- If a program makes NO calls to other programs, it is logged as a **leaf node** with:
  - **Call Type**: `(none)`
  - **Callee Program**: `(none)`
  - **Callee Function**: `(none)`
- This ensures **complete call hierarchy** including terminal nodes
- **Why this matters**: Without leaf nodes, the call tree is incomplete. If Program A calls Program B, and Program B doesn't call anything, Program B would be invisible as a Caller in the output
- **Benefit**: Enables complete program inventory and proper dependency analysis

### 2. DDS Dependency CSV (Program → File References)

Create a table with the following columns:

| Program Index | Program | Program Type | File Reference Type | DDS File | File Usage | Access Type | Device Type | Source File |
|---------------|---------|-------------|---------------------|----------|------------|-------------|-------------|-------------|
| 1 | MAINPGM | RPG | Database File | CUSTPF | Input | Full Procedural | DISK | MAINPGM_RPG.txt |
| 1 | MAINPGM | RPG | Display File | CUSTDSP | Combined | Full Procedural | WORKSTN | MAINPGM_RPG.txt |
| 1 | MAINPGM | RPG | Print File | CUSTRPT | Output | Full Procedural | PRINTER | MAINPGM_RPG.txt |
| 2 | CONTROLPGM | CL | Database File | ORDFILE | Input | Full Procedural | DISK | CONTROLPGM_CL.txt |

**Column Definitions:**
- **Program Index**: Sequential number (1, 2, 3...) matching call analysis
- **Program**: Name of the program being analyzed
- **Program Type**: RPG or CL
- **File Reference Type**: Database File, Display File, Print File, Special Device
- **DDS File**: Name of the file from F-spec or DCLF
- **File Usage**: Input, Output, Update, Combined
- **Access Type**: Full Procedural, Cycle, Secondary, Table/Array
- **Device Type**: DISK, WORKSTN, PRINTER, SPECIAL
- **Source File**: Source filename

### 3. Error/Info CSV (Processing Logs)

Same format as before - tracks both call extraction and DDS extraction issues

## INDEXED BATCH ANALYSIS INSTRUCTIONS

### **Repository Processing Steps with Progress Tracking:**

1. **Initialize Session with Indexing and Error Handling**
   - Count total files in repository to establish index range
   - Create **Call Analysis CSV** with headers: `Program Index,Caller,Caller Function,Call Type,Callee Program,Callee Function,File Type,Source File`
   - Create **DDS Dependency CSV** with headers: `Program Index,Program,Program Type,File Reference Type,DDS File,File Usage,Access Type,Device Type,Source File`
   - Create **Error/Info log CSV** with headers: `File_Index,File_Name,Error_Type,Error_Description,Line_Number,Suggested_Action`
   - Set up batch processing counters, error tracking, and resilient processing mode
   - Display session header: "STARTING COMPREHENSIVE ANALYSIS OF [TOTAL] FILES WITH ERROR RECOVERY"

2. **Repository Scan and Indexing Setup**
   - Recursively scan the entire repository folder 
   - Count and identify all files for processing (15,630+ files)
   - Sort files alphabetically by filename
   - Initialize progress index counter to 1

3. **Indexed Alphabetical Sequential File Processing**
   For each file in alphabetical order with progress tracking:
   
   **PROGRESS HEADER (for each file):**
   ```
   === ANALYZING FILE [X of Y]: filename ===
   File Type Detected: [RPG/Action Diagram/CL/etc.]
   Processing Status: Starting Analysis
   Error Recovery Mode: ENABLED
   ```
   
   a. **Display indexed progress header** showing current file number and total
   b. **Auto-detect file type** using dynamic pattern recognition
   c. **Extract caller program name** from filename (remove extensions and suffixes)
   d. **Apply appropriate extraction rules** based on detected type
   e. **Extract BOTH in one pass:**

      **LEVEL 1 CALLS** → Append to Call Analysis CSV:
      - **Program Index**: Current file's index number (1, 2, 3, etc.)
      - **Caller**: Current program name being analyzed
      - **Caller Function**: Context-aware function/subroutine where call is made
      - **Call Type**: Type of call (CALL, EXSR, CALLP, CALL PGM, CALL PROGRAM, etc.)
      - **Callee Program**: Target program name (external) or current program name (internal)
      - **Callee Function**: Specific function/procedure/subroutine being called
      - **File Type**: Auto-detected file type (RPG, CL, AD, etc.)
      - **Source File**: Source filename

      **DDS FILE REFERENCES** → Append to DDS Dependency CSV:
      - **Program Index**: Same index as calls (for JOIN capability)
      - **Program**: Current program name being analyzed
      - **Program Type**: RPG or CL
      - **File Reference Type**: Database File, Display File, Print File, Special Device
      - **DDS File**: File name from F-spec or DCLF
      - **File Usage**: Input, Output, Update, Combined
      - **Access Type**: Full Procedural, Cycle, Secondary, etc.
      - **Device Type**: DISK, WORKSTN, PRINTER, SPECIAL
      - **Source File**: Source filename

   f. **Append results** to appropriate CSV files (maintaining alphabetical processing order)
   g. **Display completion status** with counts for both extractions
   ```
   Processing Status: Complete - Found X Level 1 calls, Y DDS references
   === END FILE [X of Y] ===
   ```
   OR if file processing fails:
   ```
   Processing Status: ERROR - [Error_Type] - Logged for review
   Error Details: [Brief description]
   === CONTINUING TO NEXT FILE [X of Y] ===
   ```
   h. **Log any errors** to error file and **increment index counter** - continue to next file alphabetically

   **LEVEL 1 RULE**: Extract only direct calls found in each file - do not follow call chains to deeper levels

4. **Handle Different Call Scenarios**:
   - External program calls: Fill Callee Program with target program name
   - Internal subroutine calls: Fill Callee Program with current program name, Callee Function with subroutine name  
   - Service program procedures: Fill Callee Program with service program name, Callee Function with procedure name
   - Dynamic calls: Fill Callee Program with the variable or expression used

5. **Generate Final Indexed Report with Comprehensive Summary**
   ```
   === COMPREHENSIVE ANALYSIS COMPLETE ===
   Total files processed: [SUCCESSFUL COUNT] of [TOTAL COUNT]
   Processing time: [DURATION]
   Actual errors: [ERROR COUNT] (excluding INFO entries)

   ====================================================================
   CALL EXTRACTION SUMMARY (Control Flow - Program → Program)
   ====================================================================
   Files with calls found: [COUNT] ([PERCENTAGE]%)
   Total call records generated: [CALL COUNT]
   Average calls per file (with calls): [AVG]

   Call Type Breakdown:
   - EXSR (internal subroutines): [COUNT] ([PERCENTAGE]%)
   - CALL (external programs): [COUNT] ([PERCENTAGE]%)
   - EXECUTE (function calls): [COUNT] ([PERCENTAGE]%)
   - CALL PROGRAM: [COUNT] ([PERCENTAGE]%)
   - CALL PGM: [COUNT] ([PERCENTAGE]%)

   File Type Analysis (Calls):
   - RPG programs: [COUNT] calls ([PERCENTAGE]%)
   - Action Diagrams: [COUNT] calls ([PERCENTAGE]%)
   - CL programs: [COUNT] calls ([PERCENTAGE]%)

   ====================================================================
   DDS DEPENDENCY SUMMARY (Data Flow - Program → File)
   ====================================================================
   Files with DDS references: [COUNT] ([PERCENTAGE]%)
   Total DDS file references: [DDS COUNT]
   Average references per file (with refs): [AVG]

   Device Type Distribution:
   - DISK (database files): [COUNT] references ([PERCENTAGE]%)
   - WORKSTN (display files): [COUNT] references ([PERCENTAGE]%)
   - PRINTER (print files): [COUNT] references ([PERCENTAGE]%)
   - SPECIAL (special devices): [COUNT] references ([PERCENTAGE]%)

   File Usage Distribution:
   - Input (read-only): [COUNT] references ([PERCENTAGE]%)
   - Combined (read/write): [COUNT] references ([PERCENTAGE]%)
   - Update (modify): [COUNT] references ([PERCENTAGE]%)
   - Output (write-only): [COUNT] references ([PERCENTAGE]%)

   File Reference Type Distribution:
   - Database File: [COUNT] references ([PERCENTAGE]%)
   - Display File: [COUNT] references ([PERCENTAGE]%)
   - Print File: [COUNT] references ([PERCENTAGE]%)
   - Special Device: [COUNT] references ([PERCENTAGE]%)

   ====================================================================
   INFO ENTRY BREAKDOWN (Files with No Relationships)
   ====================================================================
   Files with no calls or references: [INFO COUNT] ([PERCENTAGE]%)

   Breakdown by file classification:
   - [COUNT] files: Data Definition Specification (DDS) file - screen/file layout definitions
   - [COUNT] files: Unclassified file type - may be configuration, documentation, or data file
   - [COUNT] files: SQL Query file - database query definitions
   - [COUNT] files: Likely non-executable AD file - no program calls detected
   - [COUNT] files: Likely non-executable RPG file - no program calls or F-specs detected
   - [COUNT] files: Likely non-executable CL file - no program calls or DCLF detected

   ACTUAL PROCESSING ERRORS (if any):
   Total errors: [ERROR COUNT]

   ====================================================================
   OUTPUT FILES
   ====================================================================
   Call Analysis CSV: [PATH]
     - Total records: [CALL COUNT]
     - Source files with calls: [COUNT]

   DDS Dependency CSV: [PATH]
     - Total records: [DDS COUNT]
     - Source files with references: [COUNT]

   Error/Info Log CSV: [PATH]
     - INFO entries (no relationships): [INFO COUNT]
     - Actual errors: [ERROR COUNT]
   === SESSION END ===
   ```

## Enhanced Error Recovery and Resilient Processing

### **ERROR CLASSIFICATION SYSTEM:**

**CRITICAL ERRORS (Skip file, log for manual review):**
- **PARSE_ERROR**: Syntax too malformed to analyze reliably
- **READ_ERROR**: File system access issues, encoding problems, corruption
- **TIMEOUT_ERROR**: File too large or complex for reasonable processing time
- **INVALID_FORMAT**: File format completely unrecognizable

**WARNING ERRORS (Process file, flag issues):**
- **UNKNOWN_PATTERN**: Calls found but format unusual or non-standard
- **PARTIAL_ANALYSIS**: Some calls extracted successfully, others missed
- **TYPE_AMBIGUOUS**: File type detection uncertain, used best guess
- **MIXED_CONTENT**: Multiple programming languages detected in single file

**INFO MESSAGES (Successful processing with notes):**
- **NO_CALLS_FOUND**: Valid file successfully analyzed but contains no program calls
  - INFO entries are logged for ALL files with zero calls to provide complete audit trail
  - Each INFO entry includes file type classification explaining WHY no calls were found:
    - DDS files (Data Definition Specifications - screen/file layouts)
    - SQL Query files (database query definitions)
    - User Program files (_upc, _upr - data structures only)
    - Parameter Validation files (_pvr - validation rules)
    - Edit/Validation files (_etr, _efr - field edits)
    - Selection files (_srr - query/selection criteria)
    - Unclassified files (configuration, documentation, or data files)
  - INFO entries do NOT count toward error count (only actual errors are counted)
- **LEGACY_FORMAT**: Older format detected but processed successfully
- **GENERATED_CODE**: Code appears machine-generated (SYNON, etc.) but analyzed

### **RESILIENT PROCESSING PROTOCOL:**

1. **Continue-on-Error Mode**: Never stop entire analysis due to individual file issues
2. **Error Logging**: Create detailed error log with actionable remediation steps
3. **Progress Transparency**: Show error status in real-time progress display
4. **Partial Results**: Deliver complete results from successful files
5. **Error Summary**: Provide categorized breakdown of all issues encountered

### **ERROR LOG FORMAT:**
```csv
File_Index,File_Name,Error_Type,Error_Description,Line_Number,Suggested_Action
1,ABCABP030.txt,INFO,"No program calls found - SQL Query file - database query definitions",N/A,"File processed successfully but contains no program calls"
2,ACPD210_DDS.txt,INFO,"No program calls found - Data Definition Specification (DDS) file - screen/file layout definitions",N/A,"File processed successfully but contains no program calls"
1247,CORRUPTED.txt,READ_ERROR,"File encoding issue - invalid UTF-8",N/A,"Re-save file with proper encoding"
2156,MALFORMED.txt,PARSE_ERROR,"Invalid CALL syntax on line 45",45,"Manual syntax review required"
4521,MIXED.txt,WARNING,"Both RPG and CL syntax detected",N/A,"Review file type classification"
```

### **ERROR RECOVERY ACTIONS:**
- **File Access Issues**: Skip file, log details, continue processing
- **Parse Errors**: Extract what's possible, flag incomplete results
- **Unknown Formats**: Use pattern matching, note uncertainty level  
- **Performance Issues**: Set reasonable timeout limits, log if exceeded

## Context-Aware Caller Function Detection

### **CRITICAL REQUIREMENT**: Track the actual subroutine/function where each call originates

The analysis MUST track which subroutine or function each call is made from, not just default to "MAIN" for everything.

### **Implementation for RPG Files:**

1. **Track Subroutine Boundaries:**
   - RPG subroutines are defined by `BEGSR` and `ENDSR` statements
   - Pattern: `CSR   SUBRNAME   BEGSR` (column-specific format)
   - Build a line-number-to-function map while parsing

2. **Caller Function Logic:**
   - **Before first BEGSR**: Caller Function = "MAIN"
   - **Between BEGSR and ENDSR**: Caller Function = [SUBROUTINE_NAME]
   - **After ENDSR**: Caller Function = "MAIN" (until next BEGSR)

3. **Example:**
   ```rpg
   C                   CALL      'E1I3XFR'          <- Caller Function: MAIN
   CSR   ZASNMS        BEGSR
   C                   CALL      'Y2SNMGC'          <- Caller Function: ZASNMS
   CSR   ZASNMS        ENDSR
   CSR   ZZINIT        BEGSR
   C                   CALL      'Y2RTJCR'          <- Caller Function: ZZINIT
   CSR   ZZINIT        ENDSR
   C                   EXSR      ZASNMS             <- Caller Function: MAIN
   ```

### **Implementation for Action Diagram Files:**

1. **Track Function Blocks:**
   - Look for function definition patterns in structured pseudocode
   - Track indentation levels to determine scope
   - Map line numbers to function names

2. **Caller Function Logic:**
   - Default to "MAIN" if no specific function context
   - Use actual function name when inside defined function block
   - Track EXECUTE FUNCTION calls with their originating context

### **Implementation for CL Files:**

1. **Track Subroutine Boundaries:**
   - CL subroutines defined by `SUBR` and `ENDSUBR` statements
   - Pattern: `SUBR SUBR(SUBRNAME)`

2. **Caller Function Logic:**
   - **Before first SUBR**: Caller Function = "MAIN"
   - **Between SUBR and ENDSUBR**: Caller Function = [SUBROUTINE_NAME]
   - **After ENDSUBR**: Caller Function = "MAIN"

### **Expected Output Examples:**

**Correct (Context-Aware):**
```csv
17,ACTSEL,MAIN,CALL,E1I3XFR,E1I3XFR,RPG,ACTSEL_RPG.txt
17,ACTSEL,ZASNMS,CALL,Y2SNMGC,Y2SNMGC,RPG,ACTSEL_RPG.txt
17,ACTSEL,ZZINIT,CALL,Y2RTJCR,Y2RTJCR,RPG,ACTSEL_RPG.txt
17,ACTSEL,MAIN,EXSR,ACTSEL,ZASNMS,RPG,ACTSEL_RPG.txt
```

**Incorrect (Non-Context-Aware):**
```csv
17,ACTSEL,MAIN,CALL,E1I3XFR,E1I3XFR,RPG,ACTSEL_RPG.txt
17,ACTSEL,MAIN,CALL,Y2SNMGC,Y2SNMGC,RPG,ACTSEL_RPG.txt  <- WRONG! Should be ZASNMS
17,ACTSEL,MAIN,CALL,Y2RTJCR,Y2RTJCR,RPG,ACTSEL_RPG.txt  <- WRONG! Should be ZZINIT
17,ACTSEL,MAIN,EXSR,ACTSEL,ZASNMS,RPG,ACTSEL_RPG.txt
```

### **Implementation Strategy:**

1. **Pre-scan phase**: Build context map (line_number -> function_name)
2. **Extraction phase**: Look up caller function for each call's line number
3. **Validation**: Verify context tracking with test files (ACTSEL_RPG.txt)

## Special Considerations

- **Conditional calls**: Note IF conditions, error handlers, loops
- **Dynamic calls**: Identify calls where program name is in a variable
- **System calls**: Include OS/400 system program calls
- **Service programs**: Note service program procedure calls
- **Copy members**: Identify /COPY and INCLUDE statements
- **Generated code**: Note if code appears to be generated (SYNON, etc.)

## Example Analysis with Indexing

```
=== ANALYZING FILE [1247 of 15630]: CUSTOMER_RPG.txt ===
File Type Detected: RPG Program
Processing Status: Starting Analysis
```

**File Type Detected**: RPG Program (_RPG.txt or RPG syntax)
**File Name**: CUSTOMER_RPG.txt

| Program Index | Caller | Caller Function | Call Type | Callee Program | Callee Function | File Type |
|---------------|--------|----------------|-----------|----------------|-----------------|-----------|
| 1247 | CUSTOMER | MAIN | CALL | CUSTMAINT | CUSTMAINT | RPG |
| 1247 | CUSTOMER | CALCPROCESS | EXSR | CUSTOMER | CALCAMT | RPG |
| 1247 | CUSTOMER | DATEPROC | CALLP | DATESRV | GETDATE | RPG |
| 1247 | CUSTOMER | ERRORHANDLE | CALL | LOGERROR | LOGERROR | RPG |

```
Processing Status: Complete - Found 4 Level 1 calls
=== END FILE [1247 of 15630] ===
```

**Column Definitions:**
- **Program Index**: The sequential number of the file being processed (1, 2, 3, etc.)
- **Caller**: The name of the current program/file being analyzed
- **Caller Function**: The function/subroutine within the caller where the call is made - **CONTEXT-AWARE** (tracks actual subroutine, not just "MAIN")
- **Call Type**: The type of call (CALL, EXSR, CALLP, CALL PGM, CALL PROGRAM, etc.)
- **Callee Program**: The target program name (for external calls) or current program name (for internal calls)
- **Callee Function**: The specific function/procedure/subroutine being called
- **File Type**: The detected file type (RPG, CL, AD, SQL, etc.)

**Summary**: Found 4 program calls - 2 external programs (CUSTMAINT from MAIN, LOGERROR from ERRORHANDLE), 1 internal subroutine (CALCAMT from CALCPROCESS), 1 service program procedure (GETDATE from DATEPROC). Notice how each call shows the **actual subroutine** it was called from, not just "MAIN".

---

## AGENTIC PROCESSING WORKFLOW

### **Step 1: Intelligent Repository Discovery**
- Dynamically scan the repository folder (any size - 10 files or 50,000 files)
- Auto-discover all file types present in the repository
- Build processing queue sorted **ALPHABETICALLY** by filename

### **Step 2: Adaptive File Type Classification**
- Automatically detect and classify each file type using pattern recognition
- Adapt to whatever file types are found (RPG, AD, CL, specialized types, etc.)
- Create dynamic processing rules based on discovered file types

### **Step 3: Indexed Alphabetical Sequential Processing**
- Process files in **STRICT ALPHABETICAL ORDER** by filename with progress indexing
- Display progress header: `=== ANALYZING FILE [X of Y]: filename ===`
- Extract **LEVEL 1 CALLS ONLY** from each file (direct calls, not call chains)
- Apply appropriate extraction rules based on detected file type
- Show completion status: `Processing Status: Complete - Found X Level 1 calls`

### **Step 4: Indexed Sequential Result Aggregation**
- Start with first file alphabetically (index 1) → extract calls → append to CSV
- Process second file alphabetically (index 2) → extract calls → append to CSV  
- Continue sequentially through all files with incremental indexing

### **Step 5: Dynamic CSV Output Generation with Progress Indexing**
- Generate CSV with columns: Program Index,Caller,Caller Function,Call Type,Callee Program,Callee Function,File Type
- Each call includes the Program Index showing which file order it was processed in
- File processing order maintained in CSV (alphabetical by source file)
- Each file's progress tracked and displayed during processing

## **COMPREHENSIVE COVERAGE SUMMARY**

### **Adaptive File Type Coverage (Dynamic Discovery):**
1. **RPG Programs** - `_RPG.txt` and RPG syntax detection
2. **Action Diagrams** - `_AD.txt` format recognition
3. **Control Language** - `CL.txt`, `CLP.txt`, `_CL.txt`, `_CLP.txt` patterns
4. **Specialized IBM i Programs** (Auto-detected):
   - **UPC Programs** - User Program Controls
   - **UPR Programs** - User Program Reports  
   - **UFR Programs** - User Function Reports
   - **XFR Programs** - Transfer/Export Functions
   - **PVR Programs** - Print Validation Reports
   - **PFR Programs** - Print Format Reports
   - **DFR Programs** - Data Format Reports
   - **ETR Programs** - Error/Exception Handling
   - **CPP Programs** - Copy/Process Programs
   - **EFR Programs** - Error Format Reports
   - **JOP, QFC, RSV, REP, SRR, SAV, EST** - And ANY other specialized types found
5. **Display/Print Formats** - DSPF, PRTF files (if present)
6. **Business Views** - BV.txt files (if present)
7. **SQL Queries** - Database queries and procedures (if present)
8. **Test Files** - TEST.txt and validation files (if present)
9. **Mixed/Utility Files** - Configuration, documentation, utilities (if present)

**KEY**: The system **automatically adapts** to whatever file types are found in the repository

### **Call Types Extracted:**
- **RPG**: CALL, EXSR, CALLP, EVAL, MONITOR, APIs, Service Programs
- **Action Diagram**: CALL PROGRAM statements  
- **Control Language**: CALL PGM, SBMJOB CMD, System commands
- **Specialized Programs**: Apply RPG/CL rules based on syntax detection
- **SQL**: Stored procedures, functions, embedded calls
- **Dynamic**: Variable program names and runtime calls

### **CSV Output Format:**
```
Caller,Caller Function,Call Type,Callee Program,Callee Function,File Type,Source File
CUSTPGM,MAIN,CALL,CUSTMAINT,CUSTMAINT,RPG,CUSTPGM_RPG.txt
CUSTPGM,CALCAMT,EXSR,CUSTPGM,CALCAMT,RPG,CUSTPGM_RPG.txt  
CTRLPGM,MAIN,CALL PGM,REPORTGEN,REPORTGEN,CL,CTRLPGM_CL.txt
```

---

## **EXECUTION INSTRUCTIONS**

### **For AI/Tool Execution:**

1. **Input**: Provide the path to the repository root folder containing 15,630+ files
2. **Process**: Execute batch analysis on ALL files in the repository using the comprehensive rules above
3. **Output**: Generate single CSV file named `repository_call_analysis_YYYYMMDD.csv`

### **Expected Results (Adaptive to Any Repository):**
- **Complete Coverage**: ALL files in repository analyzed (10 files or 50,000 files)
- **Level 1 Extraction**: Direct program calls captured from each file
- **Alphabetical Processing**: Results ordered by source filename alphabetically
- **Structured Output**: CSV format ready for database import or Excel analysis
- **Full Traceability**: Each call linked back to its source file
- **Dynamic Adaptation**: Automatically handles whatever file types are present

### **Sample Indexed Processing Output:**
```
=== ANALYZING FILE [1 of 15630]: ABCABP030.txt ===
File Type Detected: SQL
Processing Status: Starting Analysis
Processing Status: Complete - Found 1 Level 1 calls
=== END FILE [1 of 15630] ===

=== ANALYZING FILE [2 of 15630]: ACTBAL.txt ===
File Type Detected: RPG
Processing Status: Starting Analysis
Processing Status: Complete - Found 1 Level 1 calls
=== END FILE [2 of 15630] ===
```

### **Sample CSV Output (Alphabetical Order with Program Indexing):**
```csv
Program Index,Caller,Caller Function,Call Type,Callee Program,Callee Function,File Type
1,ABCABP030,MAIN,SELECT,PRDSOMF,ABP030,SQL
2,ACTBAL,MAIN,CALL,ACTBALS,ACTBALS,RPG
3,ACTSEL,MAIN,CALL,CUSTLOOKUP,CUSTLOOKUP,RPG
3,ACTSEL,VALIDATION,EXSR,ACTSEL,VALIDATECUST,RPG
4,AMBWORKCL,MAIN,CALL PGM,REPORTGEN,REPORTGEN,CL
```

### **Dynamic Repository Statistics (Generated at Runtime):**
- **Total Files**: [Auto-discovered count with indexing: X of Y]
- **File Type Distribution**: [Auto-generated based on discovery]
- **Processing Order**: Strict alphabetical by filename with index tracking
- **Call Extraction**: Level 1 only (direct calls per file)
- **Progress Visibility**: Real-time index tracking ([X of Y] format)

**Instructions for AI**: Execute this INDEXED AGENTIC analysis on the provided repository folder. Automatically discover all files and file types, process them in strict alphabetical order by filename with progress indexing ([X of Y] format), extract LEVEL 1 CALLS ONLY from each file, and generate a single consolidated CSV output with results aggregated alphabetically. Display real-time progress tracking for each file processed. Adapt dynamically to whatever repository size and file types are encountered.