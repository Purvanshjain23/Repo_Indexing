#!/usr/bin/env python3
"""
IBM i Repository-Wide Program Analysis Script
Analyzes all files in a repository to extract LEVEL 1 program calls, function calls, and subroutine calls.
Implements context-aware caller function detection for accurate call tracing.
"""

import os
import csv
import re
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Tuple, Set
import time

# Comprehensive blacklist for false positive elimination
BLACKLIST = {
    # Common noise words (2 characters or less)
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AI', 'AJ', 'AK', 'AL', 'AM', 'AN', 'AO', 'AP', 'AQ', 'AR', 'AS', 'AT', 'AU', 'AV', 'AW', 'AX', 'AY', 'AZ',
    'BA', 'BB', 'BC', 'BD', 'BE', 'BF', 'BG', 'BH', 'BI', 'BJ', 'BK', 'BL', 'BM', 'BN', 'BO', 'BP', 'BQ', 'BR', 'BS', 'BT', 'BU', 'BV', 'BW', 'BX', 'BY', 'BZ',
    # Common RPG/IBM i keywords and operators
    'ADD', 'SUB', 'DIV', 'MUL', 'AND', 'OR', 'NOT', 'IF', 'ELSE', 'DO', 'DOW', 'DOU', 'FOR', 'END', 'EVAL', 'MOVE', 'MOVEL', 'CLEAR',
    'READ', 'WRITE', 'UPDATE', 'DELETE', 'CHAIN', 'SETLL', 'READE', 'READP', 'READC', 'UNLOCK', 'OPEN', 'CLOSE',
    'PARM', 'PLIST', 'ENTRY', 'TAG', 'GOTO', 'CAS', 'CAB', 'CABNE', 'COMP', 'TESTN', 'TEST', 'SCAN', 'CHECK', 'CHECKR',
    'IN', 'OUT', 'EXCEPT', 'FORCE', 'POST', 'ACQ', 'REL', 'FEOD', 'SETGT', 'SETOF', 'SETON', 'TIME', 'RETURN', 'LEAVE',
    # Common CL keywords
    'PGM', 'DCL', 'VAR', 'TYPE', 'LEN', 'VALUE', 'RTNVAL', 'MONMSG', 'MSGID', 'SNDPGMMSG', 'RCVF', 'SNDF', 'CHGVAR',
    # Common file operations
    'FILE', 'LIB', 'MBR', 'RCDLEN', 'KEY', 'SEQ', 'DATA', 'OUTPUT', 'INPUT', 'USROPN',
    # Common data types and operators
    'CHAR', 'NUM', 'PACKED', 'ZONED', 'DATE', 'TIME', 'TIMESTAMP', 'FLOAT', 'INT', 'BINARY',
    # Boolean and special values
    'YES', 'NO', 'ON', 'OFF', 'BLANK', 'BLANKS', 'ZERO', 'ZEROS', 'NULL', 'HIVAL', 'LOVAL', 'ALL',
    # Common abbreviations
    'PGM', 'FLD', 'CTL', 'IND', 'VAR', 'ARR', 'DS', 'PTR',
}

# Minimum length for valid program names (3 characters to eliminate noise)
MIN_PROGRAM_NAME_LENGTH = 3


class CallRecord:
    """Represents a program call record"""
    def __init__(self, program_index: int, caller: str, caller_function: str, call_type: str,
                 callee_program: str, callee_function: str, file_type: str, source_file: str):
        self.program_index = program_index
        self.caller = caller
        self.caller_function = caller_function
        self.call_type = call_type
        self.callee_program = callee_program
        self.callee_function = callee_function
        self.file_type = file_type
        self.source_file = source_file

    def to_dict(self) -> Dict:
        """Convert to dictionary for CSV writing"""
        return {
            'Program Index': self.program_index,
            'Caller': self.caller,
            'Caller Function': self.caller_function,
            'Call Type': self.call_type,
            'Callee Program': self.callee_program,
            'Callee Function': self.callee_function,
            'File Type': self.file_type,
            'Source File': self.source_file
        }

    def __hash__(self):
        """Hash for deduplication"""
        return hash((self.caller, self.caller_function, self.call_type,
                    self.callee_program, self.callee_function, self.file_type))

    def __eq__(self, other):
        """Equality for deduplication"""
        if not isinstance(other, CallRecord):
            return False
        return (self.caller == other.caller and
                self.caller_function == other.caller_function and
                self.call_type == other.call_type and
                self.callee_program == other.callee_program and
                self.callee_function == other.callee_function and
                self.file_type == other.file_type)


class ErrorRecord:
    """Represents an error during processing"""
    def __init__(self, file_index: int, file_name: str, error_type: str,
                 error_description: str, line_number: str, suggested_action: str):
        self.file_index = file_index
        self.file_name = file_name
        self.error_type = error_type
        self.error_description = error_description
        self.line_number = line_number
        self.suggested_action = suggested_action

    def to_dict(self) -> Dict:
        """Convert to dictionary for CSV writing"""
        return {
            'File_Index': self.file_index,
            'File_Name': self.file_name,
            'Error_Type': self.error_type,
            'Error_Description': self.error_description,
            'Line_Number': self.line_number,
            'Suggested_Action': self.suggested_action
        }


class RPGAnalyzer:
    """Analyzes RPG programs with context-aware subroutine tracking"""

    # RPG patterns
    BEGSR_PATTERN = re.compile(r'^\s*C\s*SR\s+(\w+)\s+BEGSR', re.IGNORECASE)
    ENDSR_PATTERN = re.compile(r'^\s*C\s*SR\s+(\w+)\s+ENDSR', re.IGNORECASE)

    # Call patterns for RPG
    CALL_PATTERN = re.compile(r'^\s*C\s+(?:\d{2})?\s*CALL\s+[\'"]?(\w+)[\'"]?', re.IGNORECASE)
    EXSR_PATTERN = re.compile(r'^\s*C\s+(?:\d{2})?\s*EXSR\s+(\w+)', re.IGNORECASE)
    CALLP_PATTERN = re.compile(r'^\s*C\s+(?:\d{2})?\s*CALLP\s+(\w+)', re.IGNORECASE)
    CALLB_PATTERN = re.compile(r'^\s*C\s+(?:\d{2})?\s*CALLB\s+[\'"]?(\w+)[\'"]?', re.IGNORECASE)

    # Free-format RPG patterns
    FREE_CALL_PATTERN = re.compile(r'\bCALL\s+[\'"]?(\w+)[\'"]?', re.IGNORECASE)
    FREE_EXSR_PATTERN = re.compile(r'\bEXSR\s+(\w+)', re.IGNORECASE)
    FREE_CALLP_PATTERN = re.compile(r'\bCALLP\s+(\w+)', re.IGNORECASE)

    @staticmethod
    def is_valid_program_name(name: str) -> bool:
        """Check if program name is valid (not in blacklist and meets minimum length)"""
        if not name or len(name) < MIN_PROGRAM_NAME_LENGTH:
            return False
        if name.upper() in BLACKLIST:
            return False
        return True

    @staticmethod
    def build_context_map(lines: List[str]) -> Dict[int, str]:
        """
        Build a map of line numbers to current function/subroutine context.
        This enables context-aware caller function detection.
        """
        context_map = {}
        current_function = "MAIN"

        for line_num, line in enumerate(lines, 1):
            # Check for BEGSR (start of subroutine)
            begsr_match = RPGAnalyzer.BEGSR_PATTERN.match(line)
            if begsr_match:
                current_function = begsr_match.group(1).strip()

            # Check for ENDSR (end of subroutine)
            endsr_match = RPGAnalyzer.ENDSR_PATTERN.match(line)
            if endsr_match:
                current_function = "MAIN"

            # Map this line to the current function
            context_map[line_num] = current_function

        return context_map

    @staticmethod
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[ErrorRecord]]:
        """Analyze RPG file with context-aware caller function detection"""
        calls = []
        errors = []

        try:
            # Try different encodings
            content = None
            for encoding in ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']:
                try:
                    with open(file_path, 'r', encoding=encoding, errors='ignore') as f:
                        content = f.read()
                    break
                except:
                    continue

            if content is None:
                errors.append(ErrorRecord(
                    program_index, os.path.basename(file_path), 'READ_ERROR',
                    'Unable to read file with any encoding', 'N/A',
                    'Check file encoding and accessibility'
                ))
                return calls, errors

            lines = content.split('\n')

            # Build context map for caller function detection
            context_map = RPGAnalyzer.build_context_map(lines)

            # Extract calls with context awareness
            for line_num, line in enumerate(lines, 1):
                caller_function = context_map.get(line_num, "MAIN")

                # Fixed-format CALL
                match = RPGAnalyzer.CALL_PATTERN.match(line)
                if match:
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'CALL',
                            callee, callee, 'RPG', os.path.basename(file_path)
                        ))

                # EXSR (internal subroutine call)
                match = RPGAnalyzer.EXSR_PATTERN.match(line)
                if match:
                    subroutine = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(subroutine):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'EXSR',
                            caller, subroutine, 'RPG', os.path.basename(file_path)
                        ))

                # CALLP (procedure call)
                match = RPGAnalyzer.CALLP_PATTERN.match(line)
                if match:
                    procedure = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(procedure):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'CALLP',
                            procedure, procedure, 'RPG', os.path.basename(file_path)
                        ))

                # CALLB (bound call)
                match = RPGAnalyzer.CALLB_PATTERN.match(line)
                if match:
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'CALLB',
                            callee, callee, 'RPG', os.path.basename(file_path)
                        ))

                # Free-format calls (additional patterns)
                if '/FREE' in line.upper() or not line.strip().startswith('C'):
                    for pattern, call_type in [
                        (RPGAnalyzer.FREE_CALL_PATTERN, 'CALL'),
                        (RPGAnalyzer.FREE_EXSR_PATTERN, 'EXSR'),
                        (RPGAnalyzer.FREE_CALLP_PATTERN, 'CALLP')
                    ]:
                        for match in pattern.finditer(line):
                            callee = match.group(1).strip()
                            if RPGAnalyzer.is_valid_program_name(callee):
                                if call_type == 'EXSR':
                                    calls.append(CallRecord(
                                        program_index, caller, caller_function, call_type,
                                        caller, callee, 'RPG', os.path.basename(file_path)
                                    ))
                                else:
                                    calls.append(CallRecord(
                                        program_index, caller, caller_function, call_type,
                                        callee, callee, 'RPG', os.path.basename(file_path)
                                    ))

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing RPG file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, errors


class CLAnalyzer:
    """Analyzes CL (Control Language) programs with context-aware subroutine tracking"""

    # CL subroutine patterns
    SUBR_PATTERN = re.compile(r'\bSUBR\s+SUBR\((\w+)\)', re.IGNORECASE)
    ENDSUBR_PATTERN = re.compile(r'\bENDSUBR\b', re.IGNORECASE)

    # CL call patterns
    CALL_PGM_PATTERN = re.compile(r'\bCALL\s+PGM\([\'\"]?(\w+)[\'\"]?\)', re.IGNORECASE)
    SBMJOB_PATTERN = re.compile(r'\bSBMJOB\s+CMD\(CALL\s+PGM\([\'\"]?(\w+)[\'\"]?\)', re.IGNORECASE)

    @staticmethod
    def build_context_map(lines: List[str]) -> Dict[int, str]:
        """Build context map for CL subroutines"""
        context_map = {}
        current_function = "MAIN"

        for line_num, line in enumerate(lines, 1):
            # Check for SUBR
            subr_match = CLAnalyzer.SUBR_PATTERN.search(line)
            if subr_match:
                current_function = subr_match.group(1).strip()

            # Check for ENDSUBR
            if CLAnalyzer.ENDSUBR_PATTERN.search(line):
                current_function = "MAIN"

            context_map[line_num] = current_function

        return context_map

    @staticmethod
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[ErrorRecord]]:
        """Analyze CL file with context-aware caller function detection"""
        calls = []
        errors = []

        try:
            content = None
            for encoding in ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']:
                try:
                    with open(file_path, 'r', encoding=encoding, errors='ignore') as f:
                        content = f.read()
                    break
                except:
                    continue

            if content is None:
                errors.append(ErrorRecord(
                    program_index, os.path.basename(file_path), 'READ_ERROR',
                    'Unable to read file with any encoding', 'N/A',
                    'Check file encoding and accessibility'
                ))
                return calls, errors

            lines = content.split('\n')

            # Build context map
            context_map = CLAnalyzer.build_context_map(lines)

            # Extract calls
            for line_num, line in enumerate(lines, 1):
                caller_function = context_map.get(line_num, "MAIN")

                # CALL PGM
                for match in CLAnalyzer.CALL_PGM_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'CALL PGM',
                            callee, callee, 'CL', os.path.basename(file_path)
                        ))

                # SBMJOB CMD(CALL PGM
                for match in CLAnalyzer.SBMJOB_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, caller_function, 'SBMJOB',
                            callee, callee, 'CL', os.path.basename(file_path)
                        ))

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing CL file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, errors


class ActionDiagramAnalyzer:
    """Analyzes Action Diagram files"""

    # Action Diagram patterns
    CALL_PROGRAM_PATTERN = re.compile(r'\bCALL\s+PROGRAM\s+[\'\"]?(\w+)[\'\"]?', re.IGNORECASE)
    EXECUTE_PATTERN = re.compile(r'\bEXECUTE\s+(?:FUNCTION\s+)?[\'\"]?(\w+)[\'\"]?', re.IGNORECASE)

    @staticmethod
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[ErrorRecord]]:
        """Analyze Action Diagram file"""
        calls = []
        errors = []

        try:
            content = None
            for encoding in ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']:
                try:
                    with open(file_path, 'r', encoding=encoding, errors='ignore') as f:
                        content = f.read()
                    break
                except:
                    continue

            if content is None:
                errors.append(ErrorRecord(
                    program_index, os.path.basename(file_path), 'READ_ERROR',
                    'Unable to read file with any encoding', 'N/A',
                    'Check file encoding and accessibility'
                ))
                return calls, errors

            lines = content.split('\n')

            for line in lines:
                # CALL PROGRAM
                for match in ActionDiagramAnalyzer.CALL_PROGRAM_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, 'MAIN', 'CALL PROGRAM',
                            callee, callee, 'AD', os.path.basename(file_path)
                        ))

                # EXECUTE FUNCTION
                for match in ActionDiagramAnalyzer.EXECUTE_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_program_name(callee):
                        calls.append(CallRecord(
                            program_index, caller, 'MAIN', 'EXECUTE',
                            callee, callee, 'AD', os.path.basename(file_path)
                        ))

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing Action Diagram file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, errors


class RepositoryAnalyzer:
    """Main repository analyzer with indexed batch processing"""

    def __init__(self, repo_path: str):
        self.repo_path = repo_path
        self.total_files = 0
        self.processed_files = 0
        self.error_count = 0
        self.call_count = 0
        self.file_type_distribution = {}
        self.start_time = None
        self.timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')

    def detect_file_type(self, file_path: str) -> str:
        """Detect file type based on extension and content"""
        file_name = os.path.basename(file_path)
        file_lower = file_name.lower()

        # Extension-based detection
        if file_lower.endswith('_rpg.txt'):
            return 'RPG'
        elif file_lower.endswith('_ad.txt'):
            return 'AD'
        elif file_lower.endswith(('_cl.txt', '_clp.txt', 'cl.txt', 'clp.txt')):
            return 'CL'
        elif file_lower.endswith(('.txt',)):
            # Try to detect from content
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    first_lines = ''.join([f.readline() for _ in range(20)])

                    # Check for RPG indicators
                    if re.search(r'^\s*[FDHC]\s+', first_lines, re.MULTILINE):
                        return 'RPG'
                    # Check for CL syntax
                    elif re.search(r'\bPGM\b.*\bDCL\b', first_lines, re.IGNORECASE):
                        return 'CL'
                    # Check for Action Diagram syntax
                    elif re.search(r'\bCALL\s+PROGRAM\b', first_lines, re.IGNORECASE):
                        return 'AD'
            except:
                pass

        return 'UNKNOWN'

    def extract_caller_name(self, file_path: str) -> str:
        """Extract caller program name from filename"""
        file_name = os.path.basename(file_path)
        # Remove common suffixes
        name = re.sub(r'(_RPG|_AD|_CL|_CLP|_DDS|_UPC|_UPR|_UFR|_XFR|_PVR|_PFR|_DFR|_ETR|_EFR|_CPP|_SRR)\.txt$', '', file_name, flags=re.IGNORECASE)
        name = re.sub(r'\.txt$', '', name, flags=re.IGNORECASE)
        return name.upper()

    def analyze_file(self, file_path: str, program_index: int) -> Tuple[List[CallRecord], List[ErrorRecord]]:
        """Analyze a single file based on its type"""
        file_type = self.detect_file_type(file_path)
        caller = self.extract_caller_name(file_path)

        # Update file type distribution
        self.file_type_distribution[file_type] = self.file_type_distribution.get(file_type, 0) + 1

        if file_type == 'RPG':
            return RPGAnalyzer.analyze(file_path, caller, program_index)
        elif file_type == 'CL':
            return CLAnalyzer.analyze(file_path, caller, program_index)
        elif file_type == 'AD':
            return ActionDiagramAnalyzer.analyze(file_path, caller, program_index)
        else:
            # Try RPG patterns as fallback for specialized types
            return RPGAnalyzer.analyze(file_path, caller, program_index)

    def classify_no_calls_reason(self, file_path: str, file_type: str) -> str:
        """Classify why a file has no program calls"""
        file_name = os.path.basename(file_path).lower()

        # Check for DDS files (Data Description Specifications)
        if '_dds' in file_name or file_name.endswith('dds.txt'):
            return "Data Definition Specification (DDS) file - screen/file layout definitions"

        # Check for specific file type suffixes
        if '_upc' in file_name or '_upr' in file_name:
            return "User Program file - may contain data structures only"
        if '_pvr' in file_name:
            return "Parameter Validation file - data validation rules"
        if '_etr' in file_name or '_efr' in file_name:
            return "Edit/Validation file - field edits and validations"
        if '_srr' in file_name:
            return "Selection file - query/selection criteria"

        # Check file content for SQL
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                first_lines = ''.join([f.readline() for _ in range(10)])
                if 'SELECT' in first_lines.upper() and 'FROM' in first_lines.upper():
                    return "SQL Query file - database query definitions"
        except:
            pass

        # File type based classification
        if file_type == 'UNKNOWN':
            return "Unclassified file type - may be configuration, documentation, or data file"

        return f"Likely non-executable {file_type} file - no program calls detected"

    def scan_repository(self) -> List[str]:
        """Scan repository and return sorted list of files"""
        print("Scanning repository...")
        files = []

        for root, dirs, filenames in os.walk(self.repo_path):
            for filename in filenames:
                if filename.endswith('.txt'):
                    file_path = os.path.join(root, filename)
                    files.append(file_path)

        # Sort alphabetically by filename
        files.sort(key=lambda x: os.path.basename(x).upper())
        self.total_files = len(files)

        print(f"Found {self.total_files} files to analyze")
        return files

    def analyze_repository(self) -> Tuple[str, str]:
        """
        Main analysis method with indexed alphabetical processing
        Returns paths to output CSV and error log CSV
        """
        self.start_time = time.time()

        # Create output filenames in parent directory (not inside source repo)
        parent_dir = os.path.dirname(self.repo_path)
        output_csv = os.path.join(parent_dir, f'repository_call_analysis_{self.timestamp}.csv')
        error_csv = os.path.join(parent_dir, f'repository_errors_{self.timestamp}.csv')

        print(f"\n{'='*70}")
        print(f"STARTING ANALYSIS OF REPOSITORY WITH ERROR RECOVERY")
        print(f"Repository: {self.repo_path}")
        print(f"Timestamp: {self.timestamp}")
        print(f"{'='*70}\n")

        # Scan repository
        files = self.scan_repository()

        # Initialize CSV files
        with open(output_csv, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=[
                'Program Index', 'Caller', 'Caller Function', 'Call Type',
                'Callee Program', 'Callee Function', 'File Type', 'Source File'
            ])
            writer.writeheader()

        with open(error_csv, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=[
                'File_Index', 'File_Name', 'Error_Type', 'Error_Description',
                'Line_Number', 'Suggested_Action'
            ])
            writer.writeheader()

        # Process files alphabetically with progress tracking
        all_calls_set = set()  # For deduplication within each file

        for idx, file_path in enumerate(files, 1):
            file_name = os.path.basename(file_path)

            print(f"\n{'='*70}")
            print(f"=== ANALYZING FILE [{idx} of {self.total_files}]: {file_name} ===")

            # Detect file type
            file_type = self.detect_file_type(file_path)
            print(f"File Type Detected: {file_type}")
            print(f"Processing Status: Starting Analysis")
            print(f"Error Recovery Mode: ENABLED")

            try:
                # Analyze file
                calls, errors = self.analyze_file(file_path, idx)

                # Deduplicate calls for this file
                unique_calls = []
                seen = set()
                for call in calls:
                    if call not in seen:
                        seen.add(call)
                        unique_calls.append(call)

                # Log files with no calls as INFO entries
                if len(unique_calls) == 0 and len(errors) == 0:
                    # Classify file type reason
                    reason = self.classify_no_calls_reason(file_path, file_type)
                    info_record = ErrorRecord(
                        idx, file_name, 'INFO',
                        f'No program calls found - {reason}', 'N/A',
                        'File processed successfully but contains no program calls'
                    )
                    errors.append(info_record)

                # Write calls to CSV
                with open(output_csv, 'a', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=[
                        'Program Index', 'Caller', 'Caller Function', 'Call Type',
                        'Callee Program', 'Callee Function', 'File Type', 'Source File'
                    ])
                    for call in unique_calls:
                        writer.writerow(call.to_dict())

                # Write errors to CSV
                if errors:
                    with open(error_csv, 'a', newline='', encoding='utf-8') as f:
                        writer = csv.DictWriter(f, fieldnames=[
                            'File_Index', 'File_Name', 'Error_Type', 'Error_Description',
                            'Line_Number', 'Suggested_Action'
                        ])
                        for error in errors:
                            writer.writerow(error.to_dict())

                    # Only count non-INFO errors as actual errors
                    error_count_increment = sum(1 for e in errors if e.error_type != 'INFO')
                    self.error_count += error_count_increment

                # Update counters
                self.call_count += len(unique_calls)
                self.processed_files += 1

                # Status message
                if errors:
                    print(f"Processing Status: PARTIAL - Found {len(unique_calls)} calls with {len(errors)} errors")
                else:
                    print(f"Processing Status: Complete - Found {len(unique_calls)} Level 1 calls")

            except Exception as e:
                # Catch-all error handling
                error = ErrorRecord(
                    idx, file_name, 'CRITICAL_ERROR',
                    f'Unexpected error: {str(e)}', 'N/A',
                    'Manual review required'
                )

                with open(error_csv, 'a', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=[
                        'File_Index', 'File_Name', 'Error_Type', 'Error_Description',
                        'Line_Number', 'Suggested_Action'
                    ])
                    writer.writerow(error.to_dict())

                self.error_count += 1
                print(f"Processing Status: ERROR - {str(e)}")
                print(f"=== CONTINUING TO NEXT FILE [{idx} of {self.total_files}] ===")

            print(f"=== END FILE [{idx} of {self.total_files}] ===")

        # Generate final report
        self.generate_final_report(output_csv, error_csv)

        return output_csv, error_csv

    def generate_final_report(self, output_csv: str, error_csv: str):
        """Generate final analysis report"""
        elapsed_time = time.time() - self.start_time

        print(f"\n{'='*70}")
        print(f"=== ANALYSIS COMPLETE ===")
        print(f"{'='*70}")
        print(f"Total files processed: {self.processed_files} of {self.total_files}")
        print(f"Processing time: {elapsed_time:.2f} seconds")
        print(f"Actual errors: {self.error_count}")

        # Analyze call statistics and INFO entries
        try:
            import csv
            from collections import Counter

            info_reasons = []
            actual_errors = 0
            files_with_no_calls = 0

            with open(error_csv, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    if row['Error_Type'] == 'INFO':
                        files_with_no_calls += 1
                        # Extract the reason from the error description
                        desc = row['Error_Description']
                        if 'No program calls found - ' in desc:
                            reason = desc.replace('No program calls found - ', '')
                            info_reasons.append(reason)
                    else:
                        actual_errors += 1

            files_with_calls = self.processed_files - files_with_no_calls

            # POSITIVE SUMMARY - Files that generated calls
            print(f"\n{'='*70}")
            print(f"CALL EXTRACTION SUMMARY (Positive Results)")
            print(f"{'='*70}")
            print(f"Files with calls found: {files_with_calls:,} ({files_with_calls/self.processed_files*100:.1f}%)")
            print(f"Total call records generated: {self.call_count:,}")
            if files_with_calls > 0:
                avg_calls = self.call_count / files_with_calls
                print(f"Average calls per file (with calls): {avg_calls:.1f}")

            # File type distribution
            print(f"\n{'='*70}")
            print(f"FILE TYPE DISTRIBUTION")
            print(f"{'='*70}")
            for file_type, count in sorted(self.file_type_distribution.items(), key=lambda x: x[1], reverse=True):
                print(f"  {file_type}: {count:,} files")

            # INFO ENTRY BREAKDOWN - Files with no calls
            print(f"\n{'='*70}")
            print(f"INFO ENTRY BREAKDOWN (Files with No Program Calls)")
            print(f"{'='*70}")

            if info_reasons:
                reason_counts = Counter(info_reasons)
                print(f"Files with no calls: {files_with_no_calls:,} ({files_with_no_calls/self.processed_files*100:.1f}%)")
                print(f"\nBreakdown by file classification:")
                for reason, count in reason_counts.most_common():
                    print(f"  - {count:,} files: {reason}")
            else:
                print("No INFO entries found (all files contained program calls)")

            if actual_errors > 0:
                print(f"\n{'='*70}")
                print(f"ACTUAL PROCESSING ERRORS")
                print(f"{'='*70}")
                print(f"Total errors: {actual_errors}")

        except Exception as e:
            print(f"Unable to analyze detailed statistics: {str(e)}")

        print(f"\n{'='*70}")
        print(f"OUTPUT FILES")
        print(f"{'='*70}")
        print(f"Call Analysis CSV: {output_csv}")
        print(f"  - Total records: {self.call_count:,}")
        print(f"  - Source files with calls: {files_with_calls:,}")
        print(f"\nError/Info Log CSV: {error_csv}")
        print(f"  - INFO entries (no calls): {files_with_no_calls:,}")
        print(f"  - Actual errors: {actual_errors}")
        print(f"{'='*70}")
        print(f"=== SESSION END ===")
        print(f"{'='*70}\n")


def main():
    """Main entry point"""
    # Repository path
    repo_path = r'C:\Users\PurvanshJain\OneDrive - Programmers.IO\Desktop\Full_Repo_Analysis\AnkitBansal_Sources'

    # Create analyzer
    analyzer = RepositoryAnalyzer(repo_path)

    # Run analysis
    output_csv, error_csv = analyzer.analyze_repository()

    print(f"\nAnalysis complete!")
    print(f"Results saved to: {output_csv}")
    print(f"Errors logged to: {error_csv}")


if __name__ == '__main__':
    main()
