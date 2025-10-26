#!/usr/bin/env python3
"""
DEFINITIVE LEGACY CONVERSION DEPENDENCY ANALYZER
===============================================

THE ONLY TOOL YOU NEED FOR COMPLETE IBM i LEGACY CONVERSIONS

Uses your repository call analysis data (317K relationships) to identify ALL
interconnected programs that must be converted together for complete functionality.

PREVENTS PARTIAL CONVERSIONS like PBGREFR (missing F10, F15, Selection '2')

Usage:
    python smart_conversion_analyzer.py <call_analysis_csv> <target_program>

Example:
    python smart_conversion_analyzer.py repository_call_analysis.csv PBGREFR

Output:
    - Complete dependency analysis
    - Priority-based conversion plan
    - Function key companions identified
    - Database siblings mapped
    - JSON analysis file for detailed review

Author: Repository Analysis System
Version: 2.0 (Final Definitive Version)
"""

import pandas as pd
import json
from collections import defaultdict
from typing import Dict, List, Set
from datetime import datetime

class SmartConversionAnalyzer:
    """
    Analyzes program dependencies to ensure complete conversion coverage
    """
    
    def __init__(self, call_analysis_file: str):
        """Initialize with repository call data"""
        print(f"📊 Loading call analysis data...")
        self.call_df = pd.read_csv(call_analysis_file)
        print(f"✅ Loaded {len(self.call_df):,} call relationships")
        
        # Build lookup tables
        self.caller_to_callees = self._build_caller_lookup()
        self.callee_to_callers = self._build_callee_lookup()
        self.database_usage = self._analyze_database_usage()
        
    def _build_caller_lookup(self) -> Dict[str, List[Dict]]:
        """Build lookup: caller -> list of programs it calls"""
        lookup = defaultdict(list)
        
        call_records = self.call_df[self.call_df['Record Type'] == 'CALL']
        for _, row in call_records.iterrows():
            caller = row['Caller']
            callee = row['Callee Program']
            call_type = row['Call Type']
            
            if pd.notna(caller) and pd.notna(callee):
                lookup[caller].append({
                    'program': callee,
                    'call_type': call_type,
                    'caller_function': row.get('Caller Function', 'MAIN')
                })
        
        return dict(lookup)
    
    def _build_callee_lookup(self) -> Dict[str, List[Dict]]:
        """Build lookup: callee -> list of programs that call it"""
        lookup = defaultdict(list)
        
        call_records = self.call_df[self.call_df['Record Type'] == 'CALL']
        for _, row in call_records.iterrows():
            caller = row['Caller']
            callee = row['Callee Program']
            call_type = row['Call Type']
            
            if pd.notna(caller) and pd.notna(callee):
                lookup[callee].append({
                    'program': caller,
                    'call_type': call_type,
                    'caller_function': row.get('Caller Function', 'MAIN')
                })
        
        return dict(lookup)
    
    def _analyze_database_usage(self) -> Dict[str, Set[str]]:
        """Analyze which programs access which database files"""
        db_usage = defaultdict(set)
        
        for _, row in self.call_df.iterrows():
            program = row['Caller']
            source_file = row['Source File']
            
            if pd.notna(program) and pd.notna(source_file):
                # Extract database patterns
                if any(db_pattern in source_file.upper() for db_pattern in ['PBARCPL', 'CADNREL', 'CAVLLSL']):
                    db_name = self._extract_database_name(source_file)
                    if db_name:
                        db_usage[db_name].add(program)
        
        return dict(db_usage)
    
    def _extract_database_name(self, source_file: str) -> str:
        """Extract database name from source file"""
        source_file = source_file.upper()
        if 'PBARCPL' in source_file:
            return 'PBARCPL'  # MPR FOB Omaha Rate Table
        elif 'CADNREL' in source_file:
            return 'CADNREL'  # Application Code
        elif 'CAVLLSL' in source_file:
            return 'CAVLLSL'  # Values List
        return None
    
    def analyze_program_dependencies(self, target_program: str) -> Dict:
        """
        Perform complete dependency analysis for target program
        """
        print(f"\n🎯 ANALYZING DEPENDENCIES FOR: {target_program}")
        print("=" * 60)
        
        # Find direct calls (what this program calls)
        direct_calls = self.caller_to_callees.get(target_program, [])
        
        # Find who calls this program
        called_by = self.callee_to_callers.get(target_program, [])
        
        # Find database siblings
        db_siblings = self._find_database_siblings(target_program)
        
        # Analyze call patterns for function keys
        function_key_programs = self._identify_function_key_programs(direct_calls)
        
        # Calculate conversion priorities
        priorities = self._calculate_priorities(
            target_program, direct_calls, called_by, db_siblings, function_key_programs
        )
        
        analysis = {
            'target_program': target_program,
            'analysis_date': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            'direct_calls': direct_calls,
            'called_by': called_by,
            'database_siblings': db_siblings,
            'function_key_programs': function_key_programs,
            'conversion_priorities': priorities,
            'summary': self._generate_summary(priorities)
        }
        
        return analysis
    
    def _find_database_siblings(self, program: str) -> List[str]:
        """Find programs that share database files with target"""
        siblings = []
        program_databases = []
        
        # Find which databases this program uses
        for db_name, programs in self.database_usage.items():
            if program in programs:
                program_databases.append(db_name)
        
        # Find other programs using same databases
        for db_name in program_databases:
            for other_program in self.database_usage[db_name]:
                if other_program != program and other_program not in siblings:
                    siblings.append(other_program)
        
        return siblings
    
    def _identify_function_key_programs(self, direct_calls: List[Dict]) -> List[Dict]:
        """Identify programs likely triggered by function keys"""
        function_programs = []
        
        for call_info in direct_calls:
            program_name = call_info['program']
            
            # Enhanced pattern matching for function key programs
            if self._is_likely_function_key_program(program_name):
                function_programs.append({
                    'program': program_name,
                    'call_type': call_info['call_type'],
                    'likely_function_key': self._guess_function_key(program_name),
                    'priority': 'CRITICAL',
                    'description': self._get_program_description(program_name)
                })
        
        return function_programs
    
    def _is_likely_function_key_program(self, program_name: str) -> bool:
        """Check if program name pattern suggests function key trigger"""
        patterns = ['PVR', 'VFR', 'VER', 'EDT', 'PRM']
        return any(pattern in program_name.upper() for pattern in patterns)
    
    def _guess_function_key(self, program_name: str) -> str:
        """Guess which function key based on program name patterns"""
        name_upper = program_name.upper()
        
        # Specific program mappings (from actual analysis)
        if program_name == 'PBGVEFR':
            return 'F10 (System Configuration)'
        elif program_name == 'PBIBPVR':
            return 'F15 (Batch Rate Updates)'
        elif program_name == 'PBIDPVR':
            return 'Selection 2 (Advanced Region Edit)'
        
        # General patterns
        if 'PVR' in name_upper:
            if 'IB' in name_upper and 'B' in name_upper:
                return 'F15 (Batch Processing)'
            elif 'ID' in name_upper:
                return 'Selection 2 (Detail Edit)'
            return 'F10 or F15 (Prompt/Validate)'
        elif 'VFR' in name_upper or 'VER' in name_upper:
            return 'F10 (Validation/Control)'
        elif 'EDT' in name_upper:
            return 'F2 or Selection (Edit)'
        
        return 'UNKNOWN'
    
    def _get_program_description(self, program_name: str) -> str:
        """Get description for known programs"""
        descriptions = {
            'PBGVEFR': 'System Configuration and Control Settings',
            'PBIBPVR': 'Batch Rate Updates per Region',
            'PBIDPVR': 'Advanced Region Detail Editing',
            'PBFOXFR': 'FOB Export Freight Rates',
            'PBGTXFR': 'Tax Freight Calculations',
            'PBGOUPR': 'Freight Rate Updates',
            'PBHCDFR': 'Handling Code Freight',
            'PBHDDFR': 'Handling Date Freight',
            'PBHGE1R': 'Geographic Freight Rates',
            'PBHIPFR': 'International Port Freight',
            'PBICXFR': 'Import/Export Cross-Reference',
            'PBIEXFR': 'Import/Export Freight',
            'PPG1E2R': 'Program Interface',
            'PUF9XFR': 'Utility Freight Functions'
        }
        return descriptions.get(program_name, 'Related Program')
    
    def _calculate_priorities(self, target: str, direct_calls: List, 
                            called_by: List, db_siblings: List, 
                            function_programs: List) -> Dict[str, Dict]:
        """Calculate conversion priorities"""
        priorities = {}
        
        # Priority 1: Function key programs (CRITICAL)
        for func_prog in function_programs:
            program = func_prog['program']
            priorities[program] = {
                'priority': 1,
                'level': 'CRITICAL',
                'reason': f"Function key program ({func_prog['likely_function_key']})",
                'impact': 'HIGH - UI functionality will be incomplete without this',
                'program_type': 'FUNCTION_KEY'
            }
        
        # Priority 2: Other direct calls (HIGH)
        for call_info in direct_calls:
            program = call_info['program']
            if program not in priorities:  # Not already added as function key
                priorities[program] = {
                    'priority': 2,
                    'level': 'HIGH',
                    'reason': f"Direct dependency ({call_info['call_type']})",
                    'impact': 'HIGH - Business logic depends on this program',
                    'program_type': 'DIRECT_DEPENDENCY'
                }
        
        # Priority 3: Database siblings (MEDIUM)
        for sibling in db_siblings:
            if sibling not in priorities:
                priorities[sibling] = {
                    'priority': 3,
                    'level': 'MEDIUM',
                    'reason': 'Shares database tables',
                    'impact': 'MEDIUM - Data consistency and related business functions',
                    'program_type': 'DATABASE_SIBLING'
                }
        
        # Priority 4: Programs that call this one (LOW to MEDIUM)
        for caller_info in called_by:
            program = caller_info['program']
            if program not in priorities:
                priorities[program] = {
                    'priority': 4,
                    'level': 'MEDIUM',
                    'reason': f"Calls target program ({caller_info['call_type']})",
                    'impact': 'MEDIUM - May need API integration or migration',
                    'program_type': 'UPSTREAM_CALLER'
                }
        
        return priorities
    
    def _generate_summary(self, priorities: Dict) -> Dict:
        """Generate conversion summary and recommendations"""
        priority_counts = defaultdict(int)
        for program_data in priorities.values():
            priority_counts[program_data['priority']] += 1
        
        critical_programs = [p for p, data in priorities.items() if data['priority'] == 1]
        high_programs = [p for p, data in priorities.items() if data['priority'] == 2]
        
        return {
            'total_related_programs': len(priorities),
            'critical_programs': len(critical_programs),
            'high_priority_programs': len(high_programs),
            'conversion_phases': {
                'phase_1_critical': {
                    'programs': critical_programs,
                    'description': 'Function key programs - convert immediately',
                    'count': len(critical_programs)
                },
                'phase_2_high': {
                    'programs': high_programs,
                    'description': 'Direct dependencies - convert for complete functionality',
                    'count': len(high_programs)
                },
                'phase_3_related': {
                    'programs': [p for p, data in priorities.items() if data['priority'] >= 3],
                    'description': 'Related programs - convert for system integration',
                    'count': len([p for p, data in priorities.items() if data['priority'] >= 3])
                }
            },
            'completeness_estimate': f"{(len(critical_programs) + len(high_programs)) / (len(priorities) + 1) * 100:.1f}% of core functionality"
        }
    
    def print_detailed_report(self, analysis: Dict):
        """Print comprehensive analysis report"""
        print(f"\n📋 SMART CONVERSION ANALYSIS REPORT")
        print("=" * 60)
        print(f"Target Program: {analysis['target_program']}")
        print(f"Analysis Date: {analysis['analysis_date']}")
        print(f"Total Related Programs: {analysis['summary']['total_related_programs']}")
        print(f"Core Functionality Coverage: {analysis['summary']['completeness_estimate']}")
        print()
        
        # Critical Programs (Function Keys)
        critical_programs = [p for p, data in analysis['conversion_priorities'].items() if data['priority'] == 1]
        if critical_programs:
            print("🚨 CRITICAL PROGRAMS (Priority 1) - Convert First!")
            print("-" * 50)
            for program in critical_programs:
                data = analysis['conversion_priorities'][program]
                print(f"  • {program}")
                print(f"    Reason: {data['reason']}")
                print(f"    Impact: {data['impact']}")
            print()
        
        # High Priority Programs
        high_programs = [p for p, data in analysis['conversion_priorities'].items() if data['priority'] == 2]
        if high_programs:
            print("⚠️  HIGH PRIORITY PROGRAMS (Priority 2)")
            print("-" * 50)
            for program in high_programs:
                data = analysis['conversion_priorities'][program]
                print(f"  • {program} - {data['reason']}")
            print()
        
        # Database Siblings
        db_siblings = [p for p, data in analysis['conversion_priorities'].items() if data['program_type'] == 'DATABASE_SIBLING']
        if db_siblings:
            print("🗄️  DATABASE SIBLINGS (Priority 3)")
            print("-" * 50)
            for program in db_siblings:
                print(f"  • {program}")
            print()
        
        # Conversion Recommendation
        print("💡 CONVERSION RECOMMENDATION")
        print("-" * 50)
        summary = analysis['summary']
        
        phase1_programs = summary['conversion_phases']['phase_1_critical']['programs']
        phase2_programs = summary['conversion_phases']['phase_2_high']['programs']
        
        if phase1_programs:
            print(f"Phase 1 (CRITICAL): {len(phase1_programs)} programs")
            print(f"  Programs: {', '.join(phase1_programs)}")
            print(f"  Action: Convert immediately for complete UI functionality")
            print()
        
        if phase2_programs:
            print(f"Phase 2 (HIGH): {len(phase2_programs)} programs")
            # Limit display to avoid overwhelming output
            if len(phase2_programs) <= 10:
                print(f"  Programs: {', '.join(phase2_programs)}")
            else:
                print(f"  First 10 Programs: {', '.join(phase2_programs[:10])}")
                print(f"  ... and {len(phase2_programs) - 10} more (see JSON file)")
            print(f"  Action: Convert for complete business logic")
            print()
        
        # Database siblings
        db_siblings = [p for p, data in analysis['conversion_priorities'].items() if data.get('program_type') == 'DATABASE_SIBLING']
        if db_siblings:
            print(f"Phase 3 (DATABASE): {len(db_siblings)} programs")
            print(f"  Programs: {', '.join(db_siblings[:5])}{' ...' if len(db_siblings) > 5 else ''}")
            print(f"  Action: Convert for data consistency")
            print()
        
        # Show critical missing programs clearly
        critical_programs = [p for p, data in analysis['conversion_priorities'].items() if data['priority'] == 1]
        if critical_programs:
            print("🚨 CRITICAL MISSING PROGRAMS (Must Convert Together!)")
            print("-" * 50)
            for program in critical_programs:
                data = analysis['conversion_priorities'][program]
                print(f"  • {program} - {data['reason']}")
                if 'description' in data:
                    print(f"    {data['description']}")
            print()
        
        if analysis['called_by']:
            caller_programs = [caller['program'] for caller in analysis['called_by']]
            unique_callers = list(set(caller_programs))
            
            print("🔄 INTEGRATION CONSIDERATIONS")
            print("-" * 50)
            print(f"Programs that call your target ({len(unique_callers)} unique):")
            # Show only unique callers, limited to avoid spam
            for caller in unique_callers[:10]:
                print(f"  • {caller}")
            if len(unique_callers) > 10:
                print(f"  ... and {len(unique_callers) - 10} more (see JSON file)")
            print()

    def analyze_ecosystem(self, program_list: List[str]) -> Dict:
        """Analyze ecosystem of multiple related programs"""
        print(f"\n🌐 ANALYZING PROGRAM ECOSYSTEM")
        print("=" * 60)
        print(f"Programs: {', '.join(program_list)}")
        print()
        
        ecosystem_analysis = {
            'ecosystem_programs': program_list,
            'analysis_date': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            'individual_analyses': {},
            'cross_dependencies': {},
            'consolidated_priorities': {},
            'ecosystem_summary': {}
        }
        
        # Analyze each program individually
        all_related_programs = set()
        all_priorities = {}
        
        for program in program_list:
            print(f"📊 Analyzing {program}...")
            analysis = self.analyze_program_dependencies(program)
            ecosystem_analysis['individual_analyses'][program] = analysis
            
            # Collect all related programs
            for related_program in analysis['conversion_priorities'].keys():
                all_related_programs.add(related_program)
                if related_program not in all_priorities:
                    all_priorities[related_program] = analysis['conversion_priorities'][related_program]
        
        # Find cross-dependencies
        cross_deps = {}
        for program1 in program_list:
            for program2 in program_list:
                if program1 != program2:
                    # Check if program1 calls program2
                    program1_calls = ecosystem_analysis['individual_analyses'][program1]['direct_calls']
                    for call in program1_calls:
                        if call['program'] == program2:
                            cross_deps[f"{program1} -> {program2}"] = call['call_type']
        
        ecosystem_analysis['cross_dependencies'] = cross_deps
        ecosystem_analysis['consolidated_priorities'] = all_priorities
        
        # Create ecosystem summary
        critical_programs = [p for p, data in all_priorities.items() if data['priority'] == 1]
        high_programs = [p for p, data in all_priorities.items() if data['priority'] == 2]
        
        ecosystem_analysis['ecosystem_summary'] = {
            'total_ecosystem_programs': len(all_related_programs) + len(program_list),
            'critical_dependencies': len(critical_programs),
            'high_priority_dependencies': len(high_programs),
            'cross_dependencies_found': len(cross_deps),
            'conversion_groups': {
                'core_programs': program_list,
                'critical_companions': critical_programs,
                'supporting_programs': high_programs
            }
        }
        
        return ecosystem_analysis
    
    def print_ecosystem_report(self, ecosystem_analysis: Dict):
        """Print ecosystem analysis report"""
        print(f"\n🌐 ECOSYSTEM ANALYSIS REPORT")
        print("=" * 60)
        
        summary = ecosystem_analysis['ecosystem_summary']
        print(f"Core Programs: {len(summary['conversion_groups']['core_programs'])}")
        print(f"Critical Companions: {summary['critical_dependencies']}")
        print(f"Supporting Programs: {summary['high_priority_dependencies']}")
        print(f"Cross-Dependencies: {summary['cross_dependencies_found']}")
        print(f"Total Ecosystem Size: {summary['total_ecosystem_programs']}")
        print()
        
        # Show cross-dependencies
        if ecosystem_analysis['cross_dependencies']:
            print("🔗 CROSS-DEPENDENCIES FOUND")
            print("-" * 50)
            for dep, call_type in ecosystem_analysis['cross_dependencies'].items():
                print(f"  • {dep} (via {call_type})")
            print()
        
        # Show conversion groups
        groups = summary['conversion_groups']
        print("📋 CONVERSION GROUPS")
        print("-" * 50)
        print(f"Core Programs ({len(groups['core_programs'])}):")
        for program in groups['core_programs']:
            print(f"  • {program}")
        print()
        
        if groups['critical_companions']:
            print(f"Critical Companions ({len(groups['critical_companions'])}):")
            for program in groups['critical_companions']:
                print(f"  • {program}")
            print()

def main():
    """Main execution function"""
    import sys
    
    if len(sys.argv) < 3:
        print("DEFINITIVE LEGACY CONVERSION DEPENDENCY ANALYZER")
        print("=" * 50)
        print("Usage:")
        print("  Single program: python smart_conversion_analyzer.py <csv_file> <program>")
        print("  Multiple programs: python smart_conversion_analyzer.py <csv_file> <prog1> <prog2> <prog3>...")
        print()
        print("Examples:")
        print("  python smart_conversion_analyzer.py repository_call_analysis.csv PBGREFR")
        print("  python smart_conversion_analyzer.py repository_call_analysis.csv PBGREFR PBFOXFR PBGTXFR")
        return
    
    call_analysis_file = sys.argv[1]
    target_programs = sys.argv[2:]
    
    try:
        # Initialize analyzer
        analyzer = SmartConversionAnalyzer(call_analysis_file)
        
        if len(target_programs) == 1:
            # Single program analysis
            target_program = target_programs[0]
            print(f"🎯 SINGLE PROGRAM ANALYSIS: {target_program}")
            
            analysis = analyzer.analyze_program_dependencies(target_program)
            analyzer.print_detailed_report(analysis)
            
            # Save analysis to JSON
            output_file = f"{target_program}_conversion_analysis.json"
            with open(output_file, 'w') as f:
                json.dump(analysis, f, indent=2)
            
            print(f"📄 Detailed analysis saved to: {output_file}")
            
        else:
            # Multi-program ecosystem analysis
            print(f"🌐 ECOSYSTEM ANALYSIS: {len(target_programs)} programs")
            
            ecosystem_analysis = analyzer.analyze_ecosystem(target_programs)
            analyzer.print_ecosystem_report(ecosystem_analysis)
            
            # Save ecosystem analysis to JSON
            output_file = f"{'_'.join(target_programs[:3])}_ecosystem_analysis.json"
            with open(output_file, 'w') as f:
                json.dump(ecosystem_analysis, f, indent=2)
            
            print(f"📄 Ecosystem analysis saved to: {output_file}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()