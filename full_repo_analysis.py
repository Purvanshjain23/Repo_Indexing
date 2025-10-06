#!/usr/bin/env python3
"""
IBM i Comprehensive Repository Analysis Script
Analyzes all files in a repository to extract:
1. LEVEL 1 program calls, function calls, and subroutine calls (Control Flow)
2. DDS file references from F-specs and DCLF statements (Data Flow)

Implements context-aware caller function detection for accurate call tracing.
Generates multiple CSV outputs in a single pass for efficiency.
"""

import os
import csv
import re
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Tuple, Set
import time
from collections import Counter

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

# Minimum length for valid program/file names (3 characters to eliminate noise)
MIN_NAME_LENGTH = 3


class CallRecord:
    """Represents a program call record"""
    def __init__(self, program_index: int, caller: str, caller_function: str, call_type: str,
                 callee_program: str, callee_function: str, file_type: str, source_file: str,
                 record_type: str = 'CALL'):
        self.program_index = program_index
        self.caller = caller
        self.caller_function = caller_function
        self.call_type = call_type
        self.callee_program = callee_program
        self.callee_function = callee_function
        self.file_type = file_type
        self.source_file = source_file
        self.record_type = record_type  # 'CALL' or 'LEAF'

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
            'Source File': self.source_file,
            'Record Type': self.record_type
        }

    def __hash__(self):
        """Hash for deduplication"""
        return hash((self.caller, self.caller_function, self.call_type,
                    self.callee_program, self.callee_function, self.file_type, self.record_type))

    def __eq__(self, other):
        """Equality for deduplication"""
        if not isinstance(other, CallRecord):
            return False
        return (self.caller == other.caller and
                self.caller_function == other.caller_function and
                self.call_type == other.call_type and
                self.callee_program == other.callee_program and
                self.callee_function == other.callee_function and
                self.file_type == other.file_type and
                self.record_type == other.record_type)


class DDSReference:
    """Represents a DDS file reference record"""
    def __init__(self, program_index: int, program: str, program_type: str,
                 file_reference_type: str, dds_file: str, file_usage: str,
                 access_type: str, device_type: str, source_file: str):
        self.program_index = program_index
        self.program = program
        self.program_type = program_type
        self.file_reference_type = file_reference_type
        self.dds_file = dds_file
        self.file_usage = file_usage
        self.access_type = access_type
        self.device_type = device_type
        self.source_file = source_file

    def to_dict(self) -> Dict:
        """Convert to dictionary for CSV writing"""
        return {
            'Program Index': self.program_index,
            'Program': self.program,
            'Program Type': self.program_type,
            'File Reference Type': self.file_reference_type,
            'DDS File': self.dds_file,
            'File Usage': self.file_usage,
            'Access Type': self.access_type,
            'Device Type': self.device_type,
            'Source File': self.source_file
        }

    def __hash__(self):
        """Hash for deduplication"""
        return hash((self.program, self.dds_file, self.file_usage, self.device_type))

    def __eq__(self, other):
        """Equality for deduplication"""
        if not isinstance(other, DDSReference):
            return False
        return (self.program == other.program and
                self.dds_file == other.dds_file and
                self.file_usage == other.file_usage and
                self.device_type == other.device_type)


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
    """Analyzes RPG programs with context-aware subroutine tracking AND F-spec extraction"""

    # RPG patterns for subroutines
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

    # F-spec patterns for DDS file references
    FIXED_FSPEC_PATTERN = re.compile(
        r'^\s*F(\w+)\s+([IOUC])\s*([AFE])?\s*([FE])?\s*\s+([AKRD])?\s+(DISK|WORKSTN|PRINTER|SPECIAL|SEQ)',
        re.IGNORECASE
    )
    FREE_FSPEC_PATTERN = re.compile(
        r'^\s*DCL-F\s+(\w+)\s+.*?(DISK|WORKSTN|PRINTER|SPECIAL)',
        re.IGNORECASE
    )
    ALT_FSPEC_PATTERN = re.compile(
        r'^\s*F\s*(\w+)\s+.*?(DISK|WORKSTN|PRINTER|SPECIAL)',
        re.IGNORECASE
    )

    @staticmethod
    def is_valid_name(name: str) -> bool:
        """Check if name is valid (not in blacklist and meets minimum length)"""
        if not name or len(name) < MIN_NAME_LENGTH:
            return False
        if name.upper() in BLACKLIST:
            return False
        return True

    @staticmethod
    def parse_file_usage(file_type_char: str, file_designation: str, device: str) -> Tuple[str, str, str]:
        """Parse F-spec to determine file usage, access type, and reference type"""
        file_type_char = file_type_char.upper() if file_type_char else ''
        file_designation = file_designation.upper() if file_designation else ''
        device = device.upper() if device else ''

        # Determine file usage
        if file_type_char == 'I':
            file_usage = 'Input'
        elif file_type_char == 'O':
            file_usage = 'Output'
        elif file_type_char == 'U':
            file_usage = 'Update'
        elif file_type_char == 'C':
            file_usage = 'Combined'
        else:
            file_usage = 'Unknown'

        # Determine access type
        if file_designation in ('P', 'S'):
            access_type = 'Cycle' if file_designation == 'P' else 'Secondary'
        elif file_designation == 'F':
            access_type = 'Full Procedural'
        elif file_designation == 'T':
            access_type = 'Table/Array'
        elif file_designation == 'R':
            access_type = 'Record Address'
        else:
            access_type = 'Full Procedural'

        # Determine reference type based on device
        if device == 'DISK':
            reference_type = 'Database File'
        elif device == 'WORKSTN':
            reference_type = 'Display File'
        elif device == 'PRINTER':
            reference_type = 'Print File'
        elif device == 'SPECIAL':
            reference_type = 'Special Device'
        else:
            reference_type = 'Unknown'

        return file_usage, access_type, reference_type

    @staticmethod
    def build_context_map(lines: List[str]) -> Dict[int, str]:
        """Build a map of line numbers to current function/subroutine context"""
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
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[DDSReference], List[ErrorRecord]]:
        """Analyze RPG file with context-aware caller function detection AND F-spec extraction
        Implements function-level leaf node tracking (write-before-check logic)"""
        calls = []
        dds_refs = []
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
                return calls, dds_refs, errors

            lines = content.split('\n')

            # Build context map for caller function detection
            context_map = RPGAnalyzer.build_context_map(lines)

            # Get all unique functions from context map
            all_functions = set(context_map.values())

            # Track calls per function
            function_calls = {func: [] for func in all_functions}

            # Extract both calls and F-specs in one pass
            for line_num, line in enumerate(lines, 1):
                # Skip comments
                if line.strip().startswith('*') or line.strip().startswith('//'):
                    continue

                caller_function = context_map.get(line_num, "MAIN")

                # ===== EXTRACT PROGRAM CALLS =====

                # Fixed-format CALL
                match = RPGAnalyzer.CALL_PATTERN.match(line)
                if match:
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'CALL',
                            callee, callee, 'RPG', os.path.basename(file_path), 'CALL'
                        ))

                # EXSR (internal subroutine call)
                match = RPGAnalyzer.EXSR_PATTERN.match(line)
                if match:
                    subroutine = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(subroutine):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'EXSR',
                            caller, subroutine, 'RPG', os.path.basename(file_path), 'CALL'
                        ))

                # CALLP (procedure call)
                match = RPGAnalyzer.CALLP_PATTERN.match(line)
                if match:
                    procedure = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(procedure):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'CALLP',
                            procedure, procedure, 'RPG', os.path.basename(file_path), 'CALL'
                        ))

                # CALLB (bound call)
                match = RPGAnalyzer.CALLB_PATTERN.match(line)
                if match:
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'CALLB',
                            callee, callee, 'RPG', os.path.basename(file_path), 'CALL'
                        ))

                # Free-format calls
                if '/FREE' in line.upper() or not line.strip().startswith('C'):
                    for pattern, call_type in [
                        (RPGAnalyzer.FREE_CALL_PATTERN, 'CALL'),
                        (RPGAnalyzer.FREE_EXSR_PATTERN, 'EXSR'),
                        (RPGAnalyzer.FREE_CALLP_PATTERN, 'CALLP')
                    ]:
                        for match in pattern.finditer(line):
                            callee = match.group(1).strip()
                            if RPGAnalyzer.is_valid_name(callee):
                                if call_type == 'EXSR':
                                    function_calls[caller_function].append(CallRecord(
                                        program_index, caller, caller_function, call_type,
                                        caller, callee, 'RPG', os.path.basename(file_path), 'CALL'
                                    ))
                                else:
                                    function_calls[caller_function].append(CallRecord(
                                        program_index, caller, caller_function, call_type,
                                        callee, callee, 'RPG', os.path.basename(file_path), 'CALL'
                                    ))

                # ===== EXTRACT F-SPECS (DDS FILE REFERENCES) =====

                # Try fixed format F-spec
                match = RPGAnalyzer.FIXED_FSPEC_PATTERN.match(line)
                if match:
                    file_name = match.group(1).strip()
                    file_type_char = match.group(2).strip() if match.group(2) else ''
                    file_designation = match.group(4).strip() if match.group(4) else ''
                    device = match.group(6).strip() if match.group(6) else ''

                    if RPGAnalyzer.is_valid_name(file_name):
                        file_usage, access_type, reference_type = RPGAnalyzer.parse_file_usage(
                            file_type_char, file_designation, device
                        )

                        dds_refs.append(DDSReference(
                            program_index, caller, 'RPG', reference_type,
                            file_name, file_usage, access_type, device,
                            os.path.basename(file_path)
                        ))
                    continue

                # Try free format F-spec
                match = RPGAnalyzer.FREE_FSPEC_PATTERN.match(line)
                if match:
                    file_name = match.group(1).strip()
                    device = match.group(2).strip() if match.group(2) else ''

                    if RPGAnalyzer.is_valid_name(file_name):
                        if 'USAGE(*INPUT)' in line.upper():
                            file_usage = 'Input'
                        elif 'USAGE(*OUTPUT)' in line.upper():
                            file_usage = 'Output'
                        elif 'USAGE(*UPDATE)' in line.upper():
                            file_usage = 'Update'
                        else:
                            file_usage = 'Combined'

                        access_type = 'Full Procedural'

                        if device.upper() == 'DISK':
                            reference_type = 'Database File'
                        elif device.upper() == 'WORKSTN':
                            reference_type = 'Display File'
                        elif device.upper() == 'PRINTER':
                            reference_type = 'Print File'
                        else:
                            reference_type = 'Unknown'

                        dds_refs.append(DDSReference(
                            program_index, caller, 'RPG', reference_type,
                            file_name, file_usage, access_type, device,
                            os.path.basename(file_path)
                        ))
                    continue

                # Try alternative F-spec pattern
                if line.strip().startswith('F'):
                    match = RPGAnalyzer.ALT_FSPEC_PATTERN.match(line)
                    if match:
                        file_name = match.group(1).strip()
                        device = match.group(2).strip() if match.group(2) else ''

                        if RPGAnalyzer.is_valid_name(file_name):
                            line_upper = line.upper()
                            if ' I ' in line_upper or line_upper.startswith('FI'):
                                file_usage = 'Input'
                            elif ' O ' in line_upper or line_upper.startswith('FO'):
                                file_usage = 'Output'
                            elif ' U ' in line_upper or line_upper.startswith('FU'):
                                file_usage = 'Update'
                            elif ' C ' in line_upper or line_upper.startswith('FC'):
                                file_usage = 'Combined'
                            else:
                                file_usage = 'Unknown'

                            access_type = 'Full Procedural'

                            if device.upper() == 'DISK':
                                reference_type = 'Database File'
                            elif device.upper() == 'WORKSTN':
                                reference_type = 'Display File'
                            elif device.upper() == 'PRINTER':
                                reference_type = 'Print File'
                            else:
                                reference_type = 'Special Device'

                            dds_refs.append(DDSReference(
                                program_index, caller, 'RPG', reference_type,
                                file_name, file_usage, access_type, device,
                                os.path.basename(file_path)
                            ))

            # Generate LEAF records for functions with no calls
            for func in all_functions:
                if len(function_calls[func]) == 0:
                    # This is a leaf function - it doesn't call anything
                    calls.append(CallRecord(
                        program_index, caller, func, '(none)',
                        '(none)', '(none)', 'RPG', os.path.basename(file_path), 'LEAF'
                    ))
                else:
                    # This function has calls - add them all to the main list
                    calls.extend(function_calls[func])

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing RPG file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, dds_refs, errors


class CLAnalyzer:
    """Analyzes CL (Control Language) programs with context-aware subroutine tracking AND DCLF extraction"""

    # CL subroutine patterns
    SUBR_PATTERN = re.compile(r'\bSUBR\s+SUBR\((\w+)\)', re.IGNORECASE)
    ENDSUBR_PATTERN = re.compile(r'\bENDSUBR\b', re.IGNORECASE)

    # CL call patterns
    CALL_PGM_PATTERN = re.compile(r'\bCALL\s+PGM\([\'\"]?(\w+)[\'\"]?\)', re.IGNORECASE)
    SBMJOB_PATTERN = re.compile(r'\bSBMJOB\s+CMD\(CALL\s+PGM\([\'\"]?(\w+)[\'\"]?\)', re.IGNORECASE)

    # CL file declaration patterns
    DCLF_PATTERN = re.compile(r'\bDCLF\s+FILE\((\w+)\)', re.IGNORECASE)

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
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[DDSReference], List[ErrorRecord]]:
        """Analyze CL file with context-aware caller function detection AND DCLF extraction"""
        calls = []
        dds_refs = []
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
                return calls, dds_refs, errors

            lines = content.split('\n')

            # Build context map
            context_map = CLAnalyzer.build_context_map(lines)

            # Get all unique functions from context map
            all_functions = set(context_map.values())

            # Track calls per function for leaf node detection
            function_calls = {func: [] for func in all_functions}

            # Extract both calls and DCLF in one pass
            for line_num, line in enumerate(lines, 1):
                caller_function = context_map.get(line_num, "MAIN")

                # ===== EXTRACT PROGRAM CALLS =====

                # CALL PGM
                for match in CLAnalyzer.CALL_PGM_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'CALL PGM',
                            callee, callee, 'CL', os.path.basename(file_path), 'CALL'
                        ))

                # SBMJOB CMD(CALL PGM
                for match in CLAnalyzer.SBMJOB_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        function_calls[caller_function].append(CallRecord(
                            program_index, caller, caller_function, 'SBMJOB',
                            callee, callee, 'CL', os.path.basename(file_path), 'CALL'
                        ))

                # ===== EXTRACT DCLF (FILE DECLARATIONS) =====

                for match in CLAnalyzer.DCLF_PATTERN.finditer(line):
                    file_name = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(file_name):
                        # For CL files, we'll determine usage based on context
                        # Default to Input for DCLF (most common)
                        file_usage = 'Input'
                        access_type = 'Full Procedural'
                        device_type = 'DISK'  # Most DCLF are database files
                        reference_type = 'Database File'

                        # Check for RCVF (input) or SNDF (output) context
                        line_upper = line.upper()
                        if 'RCVF' in line_upper or 'FILE(' in line_upper:
                            file_usage = 'Input'
                        elif 'SNDF' in line_upper:
                            file_usage = 'Output'

                        dds_refs.append(DDSReference(
                            program_index, caller, 'CL', reference_type,
                            file_name, file_usage, access_type, device_type,
                            os.path.basename(file_path)
                        ))

            # Generate LEAF records for functions with no calls
            for func in all_functions:
                if len(function_calls[func]) == 0:
                    # This is a leaf function - it doesn't call anything
                    calls.append(CallRecord(
                        program_index, caller, func, '(none)',
                        '(none)', '(none)', 'CL', os.path.basename(file_path), 'LEAF'
                    ))
                else:
                    # This function has calls - add them all to the main list
                    calls.extend(function_calls[func])

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing CL file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, dds_refs, errors


class ActionDiagramAnalyzer:
    """Analyzes Action Diagram files (no DDS references - AD files don't have F-specs)"""

    # Action Diagram patterns
    CALL_PROGRAM_PATTERN = re.compile(r'\bCALL\s+PROGRAM\s+[\'\"]?(\w+)[\'\"]?', re.IGNORECASE)
    EXECUTE_PATTERN = re.compile(r'\bEXECUTE\s+(?:FUNCTION\s+)?[\'\"]?(\w+)[\'\"]?', re.IGNORECASE)

    @staticmethod
    def analyze(file_path: str, caller: str, program_index: int) -> Tuple[List[CallRecord], List[DDSReference], List[ErrorRecord]]:
        """Analyze Action Diagram file (calls only, no DDS)"""
        calls = []
        dds_refs = []  # Always empty for AD files
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
                return calls, dds_refs, errors

            lines = content.split('\n')

            # Track calls for leaf node detection
            found_calls = []

            for line in lines:
                # CALL PROGRAM
                for match in ActionDiagramAnalyzer.CALL_PROGRAM_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        found_calls.append(CallRecord(
                            program_index, caller, 'MAIN', 'CALL PROGRAM',
                            callee, callee, 'AD', os.path.basename(file_path), 'CALL'
                        ))

                # EXECUTE FUNCTION
                for match in ActionDiagramAnalyzer.EXECUTE_PATTERN.finditer(line):
                    callee = match.group(1).strip()
                    if RPGAnalyzer.is_valid_name(callee):
                        found_calls.append(CallRecord(
                            program_index, caller, 'MAIN', 'EXECUTE',
                            callee, callee, 'AD', os.path.basename(file_path), 'CALL'
                        ))

            # Generate LEAF record if no calls found
            if len(found_calls) == 0:
                calls.append(CallRecord(
                    program_index, caller, 'MAIN', '(none)',
                    '(none)', '(none)', 'AD', os.path.basename(file_path), 'LEAF'
                ))
            else:
                calls.extend(found_calls)

        except Exception as e:
            errors.append(ErrorRecord(
                program_index, os.path.basename(file_path), 'PARSE_ERROR',
                f'Error analyzing Action Diagram file: {str(e)}', 'N/A',
                'Review file syntax and structure'
            ))

        return calls, dds_refs, errors


class RepositoryAnalyzer:
    """Main repository analyzer with indexed batch processing for both calls and DDS"""

    def __init__(self, repo_path: str):
        self.repo_path = repo_path
        self.total_files = 0
        self.processed_files = 0
        self.error_count = 0
        self.call_count = 0
        self.dds_count = 0
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

    def analyze_file(self, file_path: str, program_index: int) -> Tuple[List[CallRecord], List[DDSReference], List[ErrorRecord]]:
        """Analyze a single file based on its type - extract BOTH calls and DDS in one pass"""
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

    def classify_no_relationships_reason(self, file_path: str, file_type: str, has_calls: bool, has_dds: bool) -> str:
        """Classify why a file has no relationships"""
        file_name = os.path.basename(file_path).lower()

        # Check for DDS files (Data Description Specifications)
        if '_dds' in file_name or file_name.endswith('dds.txt'):
            return "Data Definition Specification (DDS) file - defines files, doesn't reference them"

        # Check for specific file type suffixes
        if '_upc' in file_name or '_upr' in file_name:
            return "User Program file - may contain only data structures or parameters"
        if '_pvr' in file_name:
            return "Parameter Validation file - typically no file I/O or calls"
        if '_etr' in file_name or '_efr' in file_name:
            return "Edit/Validation file - may not access database files or call programs"
        if '_srr' in file_name:
            return "Selection file - query/selection criteria, may not have calls or file refs"

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

        if file_type == 'AD':
            return f"Action Diagram with no program calls detected"

        if file_type == 'RPG':
            if not has_calls and not has_dds:
                return f"RPG program with no calls or F-specs - may use only data structures"
            elif not has_calls:
                return f"RPG program with no program calls detected"
            elif not has_dds:
                return f"RPG program with no F-spec file references"

        if file_type == 'CL':
            if not has_calls and not has_dds:
                return f"CL program with no calls or DCLF statements"
            elif not has_calls:
                return f"CL program with no program calls detected"
            elif not has_dds:
                return f"CL program with no DCLF file declarations"

        return f"Likely non-executable {file_type} file - no relationships detected"

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

    def analyze_repository(self) -> Tuple[str, str, str]:
        """
        Main analysis method with indexed alphabetical processing
        Returns paths to: call CSV, DDS CSV, error log CSV
        """
        self.start_time = time.time()

        # Create output filenames
        parent_dir = os.path.dirname(self.repo_path)
        call_csv = os.path.join(parent_dir, f'repository_call_analysis_{self.timestamp}.csv')
        dds_csv = os.path.join(parent_dir, f'repository_dds_dependencies_{self.timestamp}.csv')
        error_csv = os.path.join(parent_dir, f'repository_errors_{self.timestamp}.csv')

        print(f"\n{'='*70}")
        print(f"STARTING COMPREHENSIVE ANALYSIS WITH ERROR RECOVERY")
        print(f"Repository: {self.repo_path}")
        print(f"Timestamp: {self.timestamp}")
        print(f"{'='*70}\n")

        # Scan repository
        files = self.scan_repository()

        # Initialize CSV files
        with open(call_csv, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=[
                'Program Index', 'Caller', 'Caller Function', 'Call Type',
                'Callee Program', 'Callee Function', 'File Type', 'Source File', 'Record Type'
            ])
            writer.writeheader()

        with open(dds_csv, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=[
                'Program Index', 'Program', 'Program Type', 'File Reference Type',
                'DDS File', 'File Usage', 'Access Type', 'Device Type', 'Source File'
            ])
            writer.writeheader()

        with open(error_csv, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=[
                'File_Index', 'File_Name', 'Error_Type', 'Error_Description',
                'Line_Number', 'Suggested_Action'
            ])
            writer.writeheader()

        # Process files alphabetically with progress tracking
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
                # Analyze file - extract BOTH calls and DDS in one pass
                calls, dds_refs, errors = self.analyze_file(file_path, idx)

                # Deduplicate calls
                unique_calls = []
                seen_calls = set()
                for call in calls:
                    if call not in seen_calls:
                        seen_calls.add(call)
                        unique_calls.append(call)

                # Deduplicate DDS refs
                unique_dds = []
                seen_dds = set()
                for dds_ref in dds_refs:
                    if dds_ref not in seen_dds:
                        seen_dds.add(dds_ref)
                        unique_dds.append(dds_ref)

                # Log files with no relationships as INFO entries
                if len(unique_calls) == 0 and len(unique_dds) == 0 and len(errors) == 0:
                    reason = self.classify_no_relationships_reason(file_path, file_type, False, False)
                    info_record = ErrorRecord(
                        idx, file_name, 'INFO',
                        f'No relationships found - {reason}', 'N/A',
                        'File processed successfully but contains no program calls or DDS references'
                    )
                    errors.append(info_record)

                # Write calls to CSV
                with open(call_csv, 'a', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=[
                        'Program Index', 'Caller', 'Caller Function', 'Call Type',
                        'Callee Program', 'Callee Function', 'File Type', 'Source File', 'Record Type'
                    ])

                    # Write all calls (including LEAF records generated by analyzers)
                    for call in unique_calls:
                        writer.writerow(call.to_dict())

                # Write DDS refs to CSV
                with open(dds_csv, 'a', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=[
                        'Program Index', 'Program', 'Program Type', 'File Reference Type',
                        'DDS File', 'File Usage', 'Access Type', 'Device Type', 'Source File'
                    ])
                    for dds_ref in unique_dds:
                        writer.writerow(dds_ref.to_dict())

                # Write errors to CSV
                if errors:
                    with open(error_csv, 'a', newline='', encoding='utf-8') as f:
                        writer = csv.DictWriter(f, fieldnames=[
                            'File_Index', 'File_Name', 'Error_Type', 'Error_Description',
                            'Line_Number', 'Suggested_Action'
                        ])
                        for error in errors:
                            writer.writerow(error.to_dict())

                    # Only count non-INFO errors
                    error_count_increment = sum(1 for e in errors if e.error_type != 'INFO')
                    self.error_count += error_count_increment

                # Update counters
                # Count leaf nodes (programs with no calls that we still logged)
                if len(unique_calls) == 0 and file_type in ('RPG', 'CL', 'AD'):
                    self.call_count += 1  # Count the leaf node record
                else:
                    self.call_count += len(unique_calls)

                self.dds_count += len(unique_dds)
                self.processed_files += 1

                # Status message
                if errors:
                    if len(unique_calls) == 0 and file_type in ('RPG', 'CL', 'AD'):
                        print(f"Processing Status: PARTIAL - Leaf node (no calls), {len(unique_dds)} DDS refs with {len(errors)} errors/info")
                    else:
                        print(f"Processing Status: PARTIAL - Found {len(unique_calls)} calls, {len(unique_dds)} DDS refs with {len(errors)} errors/info")
                else:
                    if len(unique_calls) == 0 and file_type in ('RPG', 'CL', 'AD'):
                        print(f"Processing Status: Complete - Leaf node (no calls), {len(unique_dds)} DDS references")
                    else:
                        print(f"Processing Status: Complete - Found {len(unique_calls)} Level 1 calls, {len(unique_dds)} DDS references")

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
        self.generate_final_report(call_csv, dds_csv, error_csv)

        return call_csv, dds_csv, error_csv

    def generate_final_report(self, call_csv: str, dds_csv: str, error_csv: str):
        """Generate final comprehensive analysis report"""
        elapsed_time = time.time() - self.start_time

        print(f"\n{'='*70}")
        print(f"=== COMPREHENSIVE ANALYSIS COMPLETE ===")
        print(f"{'='*70}")
        print(f"Total files processed: {self.processed_files} of {self.total_files}")
        print(f"Processing time: {elapsed_time:.2f} seconds")
        print(f"Actual errors: {self.error_count}")

        # Analyze statistics
        try:
            # Analyze calls
            call_type_counts = Counter()
            call_file_type_counts = Counter()

            with open(call_csv, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    call_type_counts[row['Call Type']] += 1
                    call_file_type_counts[row['File Type']] += 1

            # Analyze DDS references
            device_type_counts = Counter()
            file_usage_counts = Counter()
            reference_type_counts = Counter()

            with open(dds_csv, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    device_type_counts[row['Device Type']] += 1
                    file_usage_counts[row['File Usage']] += 1
                    reference_type_counts[row['File Reference Type']] += 1

            # Analyze error CSV
            info_reasons = []
            actual_errors = 0
            files_with_no_relationships = 0

            with open(error_csv, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    if row['Error_Type'] == 'INFO':
                        files_with_no_relationships += 1
                        desc = row['Error_Description']
                        if 'No relationships found - ' in desc:
                            reason = desc.replace('No relationships found - ', '')
                            info_reasons.append(reason)
                    else:
                        actual_errors += 1

            files_with_relationships = self.processed_files - files_with_no_relationships
            files_with_calls = len(set(row.get('Caller', '') for row in csv.DictReader(open(call_csv, 'r', encoding='utf-8'))))
            files_with_dds = len(set(row.get('Program', '') for row in csv.DictReader(open(dds_csv, 'r', encoding='utf-8'))))

            # CALL EXTRACTION SUMMARY
            print(f"\n{'='*70}")
            print(f"CALL EXTRACTION SUMMARY (Control Flow - Program → Program)")
            print(f"{'='*70}")
            print(f"NOTE: Includes leaf nodes (programs with no calls) to ensure complete hierarchy")
            if files_with_calls > 0:
                print(f"Files with calls found: {files_with_calls:,} ({files_with_calls/self.processed_files*100:.1f}%)")
            print(f"Total call records generated: {self.call_count:,}")
            if files_with_calls > 0:
                avg_calls = self.call_count / files_with_calls
                print(f"Average calls per file (with calls): {avg_calls:.1f}")

            if call_type_counts:
                print(f"\nCall Type Breakdown:")
                for call_type, count in call_type_counts.most_common():
                    print(f"  - {call_type}: {count:,} ({count/self.call_count*100:.1f}%)")

            if call_file_type_counts:
                print(f"\nFile Type Analysis (Calls):")
                for file_type, count in call_file_type_counts.most_common():
                    print(f"  - {file_type} programs: {count:,} calls ({count/self.call_count*100:.1f}%)")

            # DDS DEPENDENCY SUMMARY
            print(f"\n{'='*70}")
            print(f"DDS DEPENDENCY SUMMARY (Data Flow - Program → File)")
            print(f"{'='*70}")
            if files_with_dds > 0:
                print(f"Files with DDS references: {files_with_dds:,} ({files_with_dds/self.processed_files*100:.1f}%)")
            print(f"Total DDS file references: {self.dds_count:,}")
            if files_with_dds > 0:
                avg_dds = self.dds_count / files_with_dds
                print(f"Average references per file (with refs): {avg_dds:.1f}")

            if device_type_counts:
                print(f"\nDevice Type Distribution:")
                for device, count in device_type_counts.most_common():
                    print(f"  - {device}: {count:,} references ({count/self.dds_count*100:.1f}%)")

            if file_usage_counts:
                print(f"\nFile Usage Distribution:")
                for usage, count in file_usage_counts.most_common():
                    print(f"  - {usage}: {count:,} references ({count/self.dds_count*100:.1f}%)")

            if reference_type_counts:
                print(f"\nFile Reference Type Distribution:")
                for ref_type, count in reference_type_counts.most_common():
                    print(f"  - {ref_type}: {count:,} references ({count/self.dds_count*100:.1f}%)")

            # INFO ENTRY BREAKDOWN
            print(f"\n{'='*70}")
            print(f"INFO ENTRY BREAKDOWN (Files with No Relationships)")
            print(f"{'='*70}")

            if info_reasons:
                reason_counts = Counter(info_reasons)
                print(f"Files with no calls or references: {files_with_no_relationships:,} ({files_with_no_relationships/self.processed_files*100:.1f}%)")
                print(f"\nBreakdown by file classification:")
                for reason, count in reason_counts.most_common():
                    print(f"  - {count:,} files: {reason}")
            else:
                print("No INFO entries found (all files contained relationships)")

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
        print(f"Call Analysis CSV: {call_csv}")
        print(f"  - Total records: {self.call_count:,}")
        print(f"  - Source files with calls: {files_with_calls:,}")
        print(f"\nDDS Dependency CSV: {dds_csv}")
        print(f"  - Total records: {self.dds_count:,}")
        print(f"  - Source files with references: {files_with_dds:,}")
        print(f"\nError/Info Log CSV: {error_csv}")
        print(f"  - INFO entries (no relationships): {files_with_no_relationships:,}")
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

    # Run comprehensive analysis
    call_csv, dds_csv, error_csv = analyzer.analyze_repository()

    print(f"\nComprehensive Analysis complete!")
    print(f"Call relationships saved to: {call_csv}")
    print(f"DDS dependencies saved to: {dds_csv}")
    print(f"Errors/Info logged to: {error_csv}")


if __name__ == '__main__':
    main()
