# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains a large collection of legacy business applications and programs, primarily consisting of:

- **Legacy RPG Programs**: ~5,493 files with `_RPG.txt` extensions containing RPG (Report Program Generator) business logic
- **Synon Action Diagrams**: ~5,458 files with `_AD.txt` extensions containing Synon/2E workflow definitions
- **Business Application Files**: ~4,677 additional `.txt` files containing program modules, data definitions, and support code
- **Total Files**: ~15,628+ source code files

The codebase represents a comprehensive business management system for a food processing/distribution company with modules for:
- Account Receivables (AR300, AR301, AR302, etc.)
- Production scheduling and management (P* series programs)
- Customer account management (CA* series programs)
- Inventory and product availability (PD* series programs)
- Financial reporting and data processing
- JD Edwards World/Enterprise One ERP integration

## File Structure and Naming Conventions

### Program Types
- `*_RPG.txt` - RPG programs containing business logic implementation with IBM i specific syntax
- `*_AD.txt` - Synon/2E Action Diagram files containing structured workflow definitions and business rules
- `*.txt` (others) - Program modules, data definitions, CL programs, and configuration files

### Common Program Prefixes
- `AR*` - Accounts Receivable programs (AR300, AR301, AR362A, AR410, AR600, AR601, AR605, AR610, AR800)
- `ACT*` - Account processing functions (ACTBAL, ACTRET, ACTSEL, ARACTEDT, ARACTRET, ARACTSEL)
- `CA*` - Customer/Client processing programs with extensive subfile operations
- `PD*` - Product availability, scheduling, and production management programs
- `P*` - Various business process programs (production, purchasing, processing)
- `E1*` - JD Edwards Enterprise One integration programs (E1BEXFR, E1BFDFR, E1BIDFR, etc.)
- `ICQ*` - Inquiry processing programs
- `PKG*` - Package/processing programs

## Development Context

### Technology Stack
- **Primary Language**: RPG IV/ILE RPG - IBM i business programming language with fixed-format and free-format syntax
- **Framework**: Synon/2E (System Generation) - Model-driven development framework for IBM i
- **Platform**: IBM i (formerly AS/400) - Enterprise business system platform with integrated database
- **Integration**: JD Edwards World and JD Edwards Enterprise One ERP systems
- **Database**: DB2 for i - Integrated relational database with native file system

### Business Domain
This is a comprehensive business management system for a food processing/distribution company with modules for:
- **Customer Management**: Account processing, customer data, credit management
- **Accounts Receivable**: Aging reports, payment processing, credit analysis
- **Production Management**: Scheduling, availability calculations, capacity planning
- **Inventory Management**: Product availability, warehouse management, allocation
- **Financial Reporting**: Business intelligence, data transformation, reporting
- **ERP Integration**: Bidirectional data exchange with JD Edwards systems

### Architecture Patterns
The system follows Synon/2E architectural patterns:
- **Function Types**: Processing Functions (PFR), External Functions (XFR), Display Functions (DFR), Report Functions (SRR)
- **Data Access**: Model-driven database operations with automatic CRUD generation
- **User Interface**: 5250 green screen with subfile processing
- **Program Structure**: Action diagrams compiled to RPG with consistent parameter passing

## Analysis and Reporting

The repository includes AI-generated dependency analysis reports (e.g., PDFPDFR_dependency_graph.md) that map:
- Program interdependencies and call hierarchies
- Business function relationships
- Performance bottlenecks and critical paths
- Integration points with JD Edwards systems

## Working with This Codebase

### Code Analysis Approach
When analyzing programs:
- **RPG Files**: Look for H-specs, file definitions (F-specs), data structures (D-specs), and calculation logic (C-specs)
- **Action Diagrams**: Focus on business logic flow, program calls, parameter passing, and database operations
- **Integration Patterns**: Many programs determine JDE World vs JDE E1 routing based on company values
- **Modification History**: Programs contain extensive modification logs with dates, authors, and change descriptions

### File Reading Considerations
- Text files use CRLF line terminators
- RPG files contain fixed-format code with position-sensitive syntax
- Action diagram files use structured pseudo-code with Synon/2E syntax
- Program headers contain comprehensive metadata including creation dates, authors, and functional descriptions

### Search and Navigation Patterns
- **Business Functions**: Use prefixes (AR, CA, PD, P) to find related programs
- **Integration Points**: Search for "E1", "JDE", "World", "Enterprise" to find ERP integration code
- **Cross-References**: Programs frequently call each other - trace CALL statements and parameter lists
- **Modification History**: Search modification logs for specific changes or bug fixes
- **Database Access**: Look for file specifications and database operation patterns

### Key Business Processes
- **Product Availability**: PDFPDFR serves as central hub called by 14+ programs via F16 function key
- **Account Processing**: ACTBAL, ACTRET, ACTSEL handle core account operations with E1/JW routing
- **Customer Management**: CA* series provides comprehensive customer processing with extensive subfile operations
- **AR Processing**: AR300-series handles accounts receivable with complex aging and payment logic

## Development and Maintenance Notes

### Common Development Tasks
This is a legacy system with no active development toolchain in this repository. The files are source code extracts for analysis and documentation purposes.

### System Integration Points
- **JD Edwards Integration**: Extensive integration with both JD Edwards World and Enterprise One
- **Database Operations**: Heavy use of IBM i native database with Synon-generated access methods
- **Function Key Operations**: F16 (Product Availability), standard 5250 function keys for navigation
- **Batch Processing**: Many programs designed for background execution with job submission patterns

### Performance Considerations
- **Database Access**: Programs make extensive use of database retrieval and update operations
- **Calculation Intensive**: Heavy emphasis on real-time calculations for availability, pricing, and scheduling
- **Integration Overhead**: JD Edwards integration adds complexity and potential performance bottlenecks