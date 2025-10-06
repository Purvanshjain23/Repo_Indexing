# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains a collection of IBM i (AS/400) legacy system source code in the `AnkitBansal_Sources/` directory. The codebase consists of **19,854 source files** containing RPG (Report Program Generator) programs, Synon Action Diagrams, DDS specifications, and other supporting files for what appears to be a comprehensive business application system.

## Code Architecture

### File Structure and Naming Conventions

The repository follows a consistent naming pattern:
- **RPG Programs**: Files ending with `_RPG.txt` contain IBM RPG source code (5,493 files)
- **Action Diagrams**: Files ending with `_AD.txt` contain Synon Action Diagram logic (5,458 files)
- **DDS Specifications**: Files ending with `_DDS.txt` contain Data Description Specifications (4,202 files)
- **Other Files**: UPC, UPR, ETR, DFR, EFR, PFR, SRR, PVR, XFR, and other file types (4,701 files)
- **Program Names**: Follow IBM i conventions (8-character limit) with functional prefixes

### Functional Modules

The codebase is organized into functional modules identified by 3-letter prefixes:

**Major Business Processing Modules (Top RPG Program Prefixes):**
- **PDP**: Primary Data Processing modules (56 files) - Core data management
- **PDC**: Product Data Core modules (55 files) - Product management
- **PDZ**: Product Data Zone modules (54 files) - Product data zones
- **PDX/PDT/PDF**: Product Data Extended modules (49 files each) - Product extensions
- **PDS/PDL**: Product Data Standard/Line modules (48 files each) - Product standards
- **PDN/PDD**: Product Data Node/Detail modules (47 files each) - Product details
- **PDG**: Product Data Group modules (46 files) - Product grouping
- **PKP/PKG**: Package Processing/Group modules (45 files each) - Package management
- **PDU/PDR/PDM**: Product Data Utility/Reference/Master (45 files each) - Product utilities

**Account and Reporting Modules:**
- **ACT**: Account-related functions (Account Balance, Retrieve, Select)
- **ARA**: Account reporting and analysis functions
- **ARJ/ARU**: Account reporting utilities

**Utility and Support Modules:**
- **HPJ/HPE**: Help and processing utilities
- **OPS/OPT**: Operations and option processing
- **CCR/CCS**: Additional core system functions

### Technology Stack

- **Language**: IBM RPG (Report Program Generator) - legacy IBM i programming language
- **Development Tool**: CA 2E (Synon) - CASE tool that generates RPG code from Action Diagrams
- **Platform**: IBM i (formerly AS/400) mainframe system
- **Database**: DB2/400 with physical and logical files
- **Architecture**: Traditional procedural programming with database file processing

### Code Generation Pattern

Most programs follow the CA 2E generated code pattern:
1. **Action Diagrams** (`_AD.txt`): High-level business logic in pseudo-code format (5,458 files)
2. **Generated RPG** (`_RPG.txt`): Compiled RPG code auto-generated from Action Diagrams (5,493 files)
3. **DDS Files** (`_DDS.txt`): Data Description Specifications for screen and file layouts (4,202 files)
4. **Supporting Files**: User programs (UPC, UPR), Edit files (ETR, EFR), Processing files (PFR, DFR), Selection/Validation (SRR, PVR), and Execute functions (XFR)
5. **Function Types**: Execute external functions, retrieve objects, process data, display screens, validate inputs

## Working with This Codebase

### Understanding Program Flow
1. Start with Action Diagram files (`_AD.txt`) to understand business logic
2. Reference corresponding RPG files (`_RPG.txt`) for implementation details
3. Look for `CALL PROGRAM` statements to trace program interactions
4. Examine file specifications (F-specs) to understand database dependencies

### Key Patterns to Recognize
- **Function Calls**: Programs frequently call other programs using `CALL PROGRAM`
- **Parameter Passing**: Parameters are passed between programs for data exchange
- **Database Operations**: File processing using READ, WRITE, UPDATE, DELETE operations
- **Error Handling**: Standard IBM i error handling patterns
- **Date/Time Processing**: Legacy date formats (CYMD, YMD) are common

### Development Considerations
- This is legacy code that should be treated as read-only for analysis purposes
- Programs are tightly coupled with IBM i database files and system functions
- Modern development practices may not apply to this legacy architecture
- Code analysis should focus on understanding business logic and data flow

### Analysis Approach
When analyzing this codebase:
1. Focus on business logic rather than technical implementation details
2. Trace data flow between programs to understand system architecture
3. Identify key business processes and their supporting programs
4. Document dependencies between different functional modules
5. Map database file relationships where evident from the code

## Program Dependencies and Integration

### Understanding Program Relationships
- Programs call each other using `CALL PROGRAM('PROGRAMID')` statements
- Dependencies can be traced by searching for these calls in Action Diagram files
- Example: `ACTBALS_AD.txt` depends on `E1I3XFR`, `E1JUXFR`, `ACTBALE1`, and `ACTBALW`
- Some utility programs (like `PUQ4XFR`, `PUQ6XFR`) act as service functions called by multiple modules

### Database File Integration
- Programs access database files through F-specs (File specifications)
- Common file patterns: `E10902LE` (Account Balances), `CAABREP`, `CADNREP`, etc.
- Programs perform standard database operations: READ, WRITE, UPDATE, DELETE, SETLL, READE

### System Integration Points
- **Company Determination**: Programs like `E1I3XFR` determine company context
- **Live System Routing**: `E1JUXFR` determines whether to route to JDE Enterprise 1, JDE World, or M3 systems
- **External Dependencies**: Some programs reference external systems (OMSMDL) not in this codebase

## Repository Statistics

- **Total Files**: 19,854 source files
- **RPG Programs**: 5,493 files (27.7% of codebase)
- **Action Diagrams**: 5,458 files (27.5% of codebase)
- **DDS Specifications**: 4,202 files (21.2% of codebase)
- **Other Supporting Files**: 4,701 files (23.7% of codebase)
- **File Types**: RPG, AD, DDS, UPC, UPR, ETR, DFR, EFR, PFR, SRR, PVR, XFR, and others
- **File Coverage**: Comprehensive coverage across all business domains

## Important Notes

- This repository contains legacy IBM i source code for analysis purposes
- No build/compilation commands are applicable as this requires IBM i development environment
- Programs reference external database files and system functions not present in this repository
- Code represents a snapshot of a larger enterprise system with external dependencies
- Recent file modifications indicate active maintenance and updates to the codebase