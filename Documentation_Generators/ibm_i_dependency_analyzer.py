# IBM i Dependency Analysis and Interactive Navigation Tool
# Analyzes 300,000+ call relationships to create hierarchical dependency maps
# Includes interactive navigation capabilities for real-time exploration
# 
# MERGED CAPABILITIES:
# - Comprehensive dependency analysis (enhanced from bottom_up_analyzer.py)
# - Interactive hierarchy navigation (from hierarchy_navigator.py)
# -         # Pr        # Print summary - CLEAN TOTALS WITH VERIFICATION
        print("\n" + "="*80)
        print("BOTTOM-UP DEPENDENCY ANALYSIS SUMMARY")
        print("="*80)
        print(f"📋 Total CSV Records: {self.total_csv_records:,}")
        print(f"   📞 Call Relationships: {self.actual_call_records:,}")
        print(f"   🍃 Leaf Functions: {self.leaf_records:,}")
        print(f"   📊 DDS Info Records: {self.dds_info_records:,}")
        verification_sum = self.actual_call_records + self.leaf_records + self.dds_info_records
        print(f"   ✅ Verification: {verification_sum:,} = {self.total_csv_records:,}")
        print(f"🌳 Root Functions (Never Called): {len(self.root_functions):,}")
        print(f"📚 Modules Detected: {len(self.module_programs)} (stored in separate modules file)")
        print(f"🔗 Dependency Levels: {len(functions_by_level)}") - CORRECTED TOTALS WITH VERIFICATION
        print("\n" + "="*80)
        print("BOTTOM-UP DEPENDENCY ANALYSIS SUMMARY")
        print("="*80)
        print(f"📋 Total CSV Records: {self.total_csv_records:,}")
        print(f"   📞 Call Relationships: {self.actual_call_records:,}")
        print(f"   🍃 Leaf Functions: {self.leaf_records:,}")
        print(f"   📊 DDS Info Records: {self.dds_info_records:,}")
        verification_sum = self.actual_call_records + self.leaf_records + self.dds_info_records
        print(f"   ✅ Verification: {self.actual_call_records:,} + {self.leaf_records:,} + {self.dds_info_records:,} = {verification_sum:,}")
        print(f"🌳 Root Functions (Never Called): {len(self.root_functions):,}")
        print(f"📚 Modules Detected: {len(self.module_programs)} (stored in separate modules file)")
        print(f"🔗 Dependency Levels: {len(functions_by_level)}")ing and auto-detection
# - Implementation variance analysis
# - Real-time command-line exploration interface
# 
# USAGE GUIDE: See IBM_i_DEPENDENCY_ANALYSIS_GUIDE.md for complete documentation

import csv
import json
from collections import defaultdict, deque
import argparse
from pathlib import Path

class DependencyAnalyzer:
    """
    Bottom-up dependency analyzer for IBM i call hierarchy
    Maps from leaf nodes (bottom) to root callers (top)
    """
    
    def __init__(self, csv_path, overview_csv_path=None):
        self.csv_path = csv_path
        self.overview_csv_path = overview_csv_path
        
        # Core data structures
        self.call_relationships = []  # All call records
        self.reverse_calls = defaultdict(set)  # callee -> set of callers
        self.forward_calls = defaultdict(set)  # caller -> set of callees
        self.leaf_functions = set()  # Functions that make no calls
        self.root_functions = set()  # Functions that are never called
        
        # Record counting
        self.total_csv_records = 0  # Total records in CSV
        self.actual_call_records = 0  # Only CALL records
        self.leaf_records = 0  # LEAF records
        self.dds_info_records = 0  # DDS_INFO records
        
        # Implementation analysis structures (NEW)
        self.unique_functions = set()  # Unique function names
        self.total_implementations = 0  # Total function implementations across files
        self.function_implementations = defaultdict(list)  # function -> list of implementations
        self.file_type_distribution = defaultdict(int)  # file_type -> count
        
        # Module mapping structures
        self.program_modules = {}  # program -> module mapping
        self.module_dependencies = defaultdict(set)  # module -> dependent modules
        self.module_programs = defaultdict(set)  # module -> programs in module
        
        # Analysis results
        self.dependency_levels = {}  # function -> level from bottom
        self.impact_chains = {}  # function -> all functions that depend on it
        self.usage_counts = defaultdict(int)  # function -> how many times it's called
    
    def load_data(self):
        """Load and process the CSV call analysis data"""
        print(f"Loading call hierarchy from {self.csv_path}...")
        
        with open(self.csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            
            for row in reader:
                self.total_csv_records += 1
                
                caller = row['Caller']
                caller_func = row['Caller Function']
                callee = row['Callee Program']
                callee_func = row['Callee Function']
                call_type = row['Call Type']
                record_type = row['Record Type']
                
                # Create full function identifiers
                caller_id = f"{caller}.{caller_func}"
                
                if record_type == 'LEAF':
                    # This is a leaf function (makes no calls)
                    self.leaf_functions.add(caller_id)
                    self.leaf_records += 1
                elif record_type == 'CALL':
                    # This is an actual call relationship
                    callee_id = f"{callee}.{callee_func}"
                    
                    # Store the relationship
                    self.call_relationships.append({
                        'caller': caller_id,
                        'callee': callee_id,
                        'call_type': call_type,
                        'caller_program': caller,
                        'callee_program': callee
                    })
                    
                    # Build reverse and forward call maps
                    self.reverse_calls[callee_id].add(caller_id)
                    self.forward_calls[caller_id].add(callee_id)
                    
                    # Count usage
                    self.usage_counts[callee_id] += 1
                    self.actual_call_records += 1
                elif record_type == 'DDS_INFO':
                    # DDS information record
                    self.dds_info_records += 1
        
        print(f"Total CSV records processed: {self.total_csv_records}")
        print(f"Actual call relationships: {self.actual_call_records}")
        print(f"Leaf functions: {len(self.leaf_functions)}")
        print(f"DDS info records: {self.dds_info_records}")
        
        # Find root functions (never called by anyone)
        all_functions = set(self.leaf_functions)
        for rel in self.call_relationships:
            all_functions.add(rel['caller'])
            all_functions.add(rel['callee'])
        
        called_functions = set(rel['callee'] for rel in self.call_relationships)
        self.root_functions = all_functions - called_functions
        
        print(f"Found {len(self.root_functions)} root functions (never called)")
        print(f"Total unique functions: {len(all_functions)}")
    
    def load_implementation_data(self):
        """Load function implementation data from overview CSV"""
        if not self.overview_csv_path:
            print("No overview CSV provided - skipping implementation analysis")
            return
        
        print(f"Loading implementation data from {self.overview_csv_path}...")
        
        try:
            with open(self.overview_csv_path, 'r', encoding='utf-8') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    caller = row['Caller']
                    function = row['Caller Function']
                    file_type = row['File Type']
                    source_file = row['Source File']
                    total_calls = int(row['Total Calls'])
                    unique_callees = int(row['Unique Callees'])
                    call_type = row['Call Type']
                    record_type = row['Record Type']
                    is_leaf = record_type == 'LEAF'
                    
                    function_key = f"{caller}.{function}"
                    self.unique_functions.add(function_key)
                    self.total_implementations += 1
                    
                    # Store implementation details
                    implementation = {
                        'file_type': file_type,
                        'source_file': source_file,
                        'total_calls': total_calls,
                        'unique_callees': unique_callees,
                        'call_type': call_type,
                        'record_type': record_type,
                        'is_leaf_function': is_leaf
                    }
                    
                    self.function_implementations[function_key].append(implementation)
                    self.file_type_distribution[file_type] += 1
            
            print(f"✅ Loaded {self.total_implementations:,} function implementations")
            print(f"✅ Found {len(self.unique_functions):,} unique functions")
            print(f"✅ Implementation variance: {self.total_implementations - len(self.unique_functions):,}")
            
        except Exception as e:
            print(f"❌ Error loading overview CSV: {e}")
    
    def calculate_dependency_levels(self):
        """
        Calculate dependency levels using bottom-up approach
        Level 0: Leaf functions (make no calls)
        Level 1: Functions that only call leaf functions
        Level N: Functions that call functions at level N-1 or lower
        """
        print("Calculating dependency levels (bottom-up)...")
        
        # Start with leaf functions at level 0
        for leaf in self.leaf_functions:
            self.dependency_levels[leaf] = 0
        
        # Process functions level by level
        current_level = 0
        processed = set(self.leaf_functions)
        
        while len(processed) < len(self.leaf_functions) + len(self.call_relationships):
            current_level += 1
            level_functions = set()
            
            # Find functions where all callees have been processed
            for rel in self.call_relationships:
                caller = rel['caller']
                callee = rel['callee']
                
                if caller not in processed:
                    # Check if all callees of this caller have been processed
                    all_callees_processed = all(
                        c in processed for c in self.forward_calls[caller]
                    )
                    
                    if all_callees_processed:
                        # Calculate level as max of all callee levels + 1
                        max_callee_level = max(
                            self.dependency_levels.get(c, 0) 
                            for c in self.forward_calls[caller]
                        )
                        self.dependency_levels[caller] = max_callee_level + 1
                        level_functions.add(caller)
            
            if not level_functions:
                # Handle circular dependencies or isolated functions
                unprocessed = set()
                for rel in self.call_relationships:
                    if rel['caller'] not in processed:
                        unprocessed.add(rel['caller'])
                        self.dependency_levels[rel['caller']] = current_level
                        level_functions.add(rel['caller'])
                
                if not level_functions:
                    break
            
            processed.update(level_functions)
            print(f"  Level {current_level}: {len(level_functions)} functions")
        
        print(f"Calculated {len(self.dependency_levels)} function levels")
    
    def build_impact_chains(self):
        """
        Build impact chains - OPTIMIZED VERSION (skips expensive recursive calculation)
        For large datasets, we only calculate direct dependents to avoid exponential complexity
        """
        print("Building impact chains (optimized for large datasets)...")
        
        # For large datasets, only calculate direct dependents (not recursive)
        # This avoids the exponential time complexity issue
        all_functions = set(self.leaf_functions)
        for rel in self.call_relationships:
            all_functions.add(rel['caller'])
            all_functions.add(rel['callee'])
        
        print(f"Processing {len(all_functions):,} functions...")
        
        # Only direct dependents for performance
        for function in all_functions:
            # Direct dependents only (not recursive to avoid exponential complexity)
            direct_dependents = self.reverse_calls.get(function, set())
            self.impact_chains[function] = direct_dependents
        
        print(f"Built direct impact chains for {len(self.impact_chains):,} functions")
    
    def auto_detect_modules(self):
        """
        Automatically detect modules based on naming patterns and call relationships
        """
        print("Auto-detecting modules from program names...")
        
        # Extract all program names
        programs = set()
        for rel in self.call_relationships:
            programs.add(rel['caller_program'])
            programs.add(rel['callee_program'])
        
        # Add programs from leaf functions
        for leaf in self.leaf_functions:
            program = leaf.split('.')[0]
            programs.add(program)
        
        # Group programs into modules based on naming patterns
        modules = defaultdict(set)
        
        for program in programs:
            # Common IBM i module patterns
            if len(program) >= 3:
                # Pattern 1: First 3 characters (e.g., ACT*, CUS*, INV*)
                prefix3 = program[:3]
                modules[f"MODULE_{prefix3}"].add(program)
                
                # Pattern 2: First 2 characters + type (e.g., AR*, AP*, GL*)
                if len(program) >= 4:
                    prefix2 = program[:2]
                    if program[2:4] in ['30', '31', '40', '41', '60', '61']:  # Common IBM i patterns
                        modules[f"MODULE_{prefix2}_REPORTS"].add(program)
                    elif program.endswith(('XFR', 'PFR', 'DFR', 'UFR')):
                        modules[f"MODULE_{prefix2}_TRANSFERS"].add(program)
                    else:
                        modules[f"MODULE_{prefix2}"].add(program)
        
        # Store module mappings
        for module_name, program_set in modules.items():
            self.module_programs[module_name] = program_set
            for program in program_set:
                self.program_modules[program] = module_name
        
        print(f"Detected {len(modules)} modules from {len(programs)} programs")
        
        # Calculate inter-module dependencies
        for rel in self.call_relationships:
            caller_module = self.program_modules.get(rel['caller_program'])
            callee_module = self.program_modules.get(rel['callee_program'])
            
            if caller_module and callee_module and caller_module != callee_module:
                self.module_dependencies[caller_module].add(callee_module)
        
        print(f"Found {sum(len(deps) for deps in self.module_dependencies.values())} inter-module dependencies")
    
    def generate_bottom_up_analysis(self, output_file="dependency_analysis.json"):
        """Generate comprehensive bottom-up analysis report"""
        print(f"Generating bottom-up analysis report...")
        
        # Organize functions by dependency level
        functions_by_level = defaultdict(list)
        for function, level in self.dependency_levels.items():
            functions_by_level[level].append(function)
        
        # Find most critical functions (highest impact)
        critical_functions = []
        for function, dependents in self.impact_chains.items():
            if len(dependents) > 0:  # Has dependents
                critical_functions.append({
                    'function': function,
                    'dependent_count': len(dependents),
                    'usage_count': self.usage_counts.get(function, 0),
                    'dependency_level': self.dependency_levels.get(function, 0),
                    'dependents': list(dependents)
                })
        
        # Sort by impact (number of dependents)
        critical_functions.sort(key=lambda x: x['dependent_count'], reverse=True)
        
        # Calculate implementation statistics (NEW)
        multiple_implementations = {k: v for k, v in self.function_implementations.items() if len(v) > 1}
        implementation_count_distribution = defaultdict(int)
        for implementations in self.function_implementations.values():
            implementation_count_distribution[len(implementations)] += 1
        
        # Create comprehensive report - FOCUSED ON ACTIONABLE MODERNIZATION METRICS
        # CSV Record Breakdown: actual_call_relationships + leaf_functions + dds_info_records = total_csv_records
        report = {
            'summary': {
                'total_csv_records': self.total_csv_records,
                'actual_call_relationships': self.actual_call_records,
                'leaf_functions': self.leaf_records,  # Renamed from leaf_function_records to avoid duplication
                'dds_info_records': self.dds_info_records,
                'total_functions_analyzed': len(self.dependency_levels),
                'root_functions': len(self.root_functions),
                'dependency_levels': len(functions_by_level),
                'modules_detected': len(self.module_programs)
            },
            'implementation_analysis': {
                'implementation_count_distribution': dict(implementation_count_distribution),
                'file_type_distribution': dict(self.file_type_distribution),
                'multiple_implementation_examples': []
            },
            'dependency_levels': {
                str(level): {
                    'function_count': len(functions),
                    'functions': functions
                }
                for level, functions in sorted(functions_by_level.items())
            },
            'critical_functions': critical_functions[:50],  # Top 50 most critical
            # Note: Module data is generated separately in dependency_analysis_modules.json
            'leaf_functions': list(self.leaf_functions),
            'root_functions': list(self.root_functions)
        }
        
        # Add examples of multiple implementations (NEW)
        if multiple_implementations:
            top_examples = sorted(multiple_implementations.items(), key=lambda x: len(x[1]), reverse=True)[:10]
            for func_key, implementations in top_examples:
                behavior_descriptions = []
                for impl in implementations:
                    behavior_descriptions.append(f"{impl['file_type']} makes {impl['total_calls']} calls")
                
                report['implementation_analysis']['multiple_implementation_examples'].append({
                    'function': func_key,
                    'implementation_count': len(implementations),
                    'file_types': [impl['file_type'] for impl in implementations],
                    'source_files': [impl['source_file'] for impl in implementations],
                    'call_patterns': [f"{impl['total_calls']} calls to {impl['unique_callees']} functions" for impl in implementations],
                    'behavior_variance': f"Different behaviors: {' vs '.join(behavior_descriptions)}"
                })
        
        # Save report
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"✅ Bottom-up analysis saved to {output_file}")
        
        # Print summary - CORRECTED TOTALS
        print("\n" + "="*80)
        print("BOTTOM-UP DEPENDENCY ANALYSIS SUMMARY")
        print("="*80)
        print(f"� Total CSV Records: {self.total_csv_records:,}")
        print(f"📞 Actual Call Relationships: {self.actual_call_records:,}")
        print(f"🍃 Leaf Functions (Level 0): {len(self.leaf_functions):,}")
        print(f"📊 DDS Info Records: {self.dds_info_records:,}")
        print(f"🌳 Root Functions (Never Called): {len(self.root_functions):,}")
        print(f"📚 Modules Detected: {len(self.module_programs)} (stored in separate modules file)")
        print(f"🔗 Dependency Levels: {len(functions_by_level)}")
        
        # Implementation Analysis Summary (NEW)
        if self.unique_functions:
            print(f"\n📋 IMPLEMENTATION ANALYSIS:")
            print(f"   Unique Functions: {len(self.unique_functions):,}")
            print(f"   Total Implementations: {self.total_implementations:,}")
            print(f"   Implementation Variance: {self.total_implementations - len(self.unique_functions):,}")
            print(f"   Functions with Multiple Implementations: {len(multiple_implementations):,} ({round(len(multiple_implementations) / len(self.unique_functions) * 100, 1)}%)")
            
            if self.file_type_distribution:
                print(f"\n📁 FILE TYPE DISTRIBUTION:")
                for file_type, count in sorted(self.file_type_distribution.items(), key=lambda x: x[1], reverse=True):
                    percentage = round(count / self.total_implementations * 100, 1)
                    print(f"   {file_type}: {count:,} ({percentage}%)")
            
            if multiple_implementations:
                print(f"\n🔄 TOP 5 FUNCTIONS WITH MULTIPLE IMPLEMENTATIONS:")
                top_examples = sorted(multiple_implementations.items(), key=lambda x: len(x[1]), reverse=True)[:5]
                for i, (func_key, implementations) in enumerate(top_examples, 1):
                    file_types = [impl['file_type'] for impl in implementations]
                    calls = [impl['total_calls'] for impl in implementations]
                    print(f"   {i}. {func_key}: {len(implementations)} implementations ({', '.join(file_types)}) - calls: {calls}")
        
        print(f"\n🎯 TOP 10 MOST CRITICAL FUNCTIONS (Highest Impact):")
        for i, func in enumerate(critical_functions[:10], 1):
            print(f"  {i:2d}. {func['function']:<30} → {func['dependent_count']:4d} dependents, Level {func['dependency_level']}")
        
        print(f"\n📈 DEPENDENCY LEVEL DISTRIBUTION:")
        for level in sorted(functions_by_level.keys()):
            count = len(functions_by_level[level])
            print(f"  Level {level:2d}: {count:5d} functions")
        
        return report
    
    def generate_module_map(self, output_file="module_dependencies.json"):
        """Generate module-level dependency map"""
        print("Generating module dependency map...")
        
        # Calculate module statistics (OPTIMIZED)
        print("Calculating module statistics...")
        module_stats = {}
        
        # Pre-calculate function counts by program for efficiency
        program_function_counts = defaultdict(int)
        for function in self.dependency_levels.keys():
            program = function.split('.')[0]
            program_function_counts[program] += 1
        
        # Pre-calculate call statistics
        module_internal_calls = defaultdict(int)
        module_external_calls = defaultdict(int)
        
        for rel in self.call_relationships:
            caller_module = self.program_modules.get(rel['caller_program'])
            callee_module = self.program_modules.get(rel['callee_program'])
            
            if caller_module:
                if caller_module == callee_module:
                    module_internal_calls[caller_module] += 1
                else:
                    module_external_calls[caller_module] += 1
        
        # Build module statistics efficiently
        for module, programs in self.module_programs.items():
            # Count functions efficiently
            function_count = sum(program_function_counts[program] for program in programs)
            
            # Get pre-calculated call counts
            internal_calls = module_internal_calls[module]
            external_calls = module_external_calls[module]
            
            module_stats[module] = {
                'programs': list(programs),
                'program_count': len(programs),
                'function_count': function_count,
                'internal_calls': internal_calls,
                'external_calls': external_calls,
                'dependencies': list(self.module_dependencies.get(module, set())),
                'dependency_count': len(self.module_dependencies.get(module, set()))
            }
        
        # Save module map
        with open(output_file, 'w') as f:
            json.dump(module_stats, f, indent=2)
        
        print(f"✅ Module dependency map saved to {output_file}")
        
        # Print module summary
        print(f"\n📚 MODULE ANALYSIS SUMMARY:")
        print(f"Total Modules: {len(module_stats)}")
        
        # Sort modules by size
        sorted_modules = sorted(module_stats.items(), key=lambda x: x[1]['program_count'], reverse=True)
        
        print(f"\n🏢 TOP 10 LARGEST MODULES:")
        for i, (module, stats) in enumerate(sorted_modules[:10], 1):
            print(f"  {i:2d}. {module:<25} → {stats['program_count']:3d} programs, {stats['function_count']:4d} functions, {stats['dependency_count']:2d} deps")
        
        return module_stats
    
    def interactive_navigation(self):
        """Interactive navigation mode with enhanced formatting from hierarchy_navigator"""
        print("\n🎯 INTERACTIVE NAVIGATION MODE")
        print("="*80)
        print("Available commands:")
        print("  function <name> - Analyze specific function")
        print("  module <name>   - Analyze specific module") 
        print("  critical [N]    - Show N critical functions")
        print("  bottom-up <fn>  - Bottom-up trace from function")
        print("  hierarchy       - Show module hierarchy")
        print("  quit           - Exit interactive mode")
        print("="*80)
        
        while True:
            try:
                command = input("\n🔍 Enter command: ").strip().split()
                if not command:
                    continue
                    
                cmd = command[0].lower()
                
                if cmd == 'quit':
                    break
                elif cmd == 'function' and len(command) > 1:
                    self.interactive_function_analysis(command[1])
                elif cmd == 'module' and len(command) > 1:
                    self.interactive_module_analysis(command[1])
                elif cmd == 'critical':
                    n = int(command[1]) if len(command) > 1 else 10
                    self.interactive_critical_paths(n)
                elif cmd == 'bottom-up' and len(command) > 1:
                    self.interactive_bottom_up_trace(command[1])
                elif cmd == 'hierarchy':
                    self.interactive_hierarchy_overview()
                else:
                    print("❌ Invalid command. Type 'quit' to exit.")
                    
            except (KeyboardInterrupt, EOFError):
                break
            except Exception as e:
                print(f"❌ Error: {e}")
        
        print("\n👋 Exiting interactive mode.")
    
    def interactive_function_analysis(self, function):
        """Enhanced function analysis with hierarchy_navigator formatting"""
        print(f"\n🔍 FUNCTION ANALYSIS: {function}")
        print("="*80)
        
        if function in self.impact_chains:
            dependents = self.impact_chains[function]
            level = self.dependency_levels.get(function, "Unknown")
            usage = self.usage_counts.get(function, 0)
            
            print(f"📊 Function Impact Summary:")
            print(f"   • Dependency Level: {level}")
            print(f"   • Direct Usage Count: {usage}")
            print(f"   • Total Dependents: {len(dependents)}")
            
            if dependents:
                # Group dependents by program/module
                dependents_by_program = defaultdict(list)
                for dependent in dependents:
                    program = dependent.split('.')[0]
                    dependents_by_program[program].append(dependent)
                
                print(f"\n📈 PROGRAMS THAT DEPEND ON {function}:")
                print(f"   Total Programs Affected: {len(dependents_by_program)}")
                
                # Show top programs by usage
                sorted_programs = sorted(dependents_by_program.items(), 
                                       key=lambda x: len(x[1]), reverse=True)
                
                for i, (program, functions) in enumerate(sorted_programs[:20], 1):
                    print(f"   {i:2d}. {program:<20} → {len(functions):3d} functions depend on it")
                    
                    # Show specific functions if not too many
                    if len(functions) <= 5:
                        for func in functions:
                            print(f"       • {func}")
                    else:
                        for func in functions[:3]:
                            print(f"       • {func}")
                        print(f"       ... and {len(functions)-3} more functions")
                
                if len(sorted_programs) > 20:
                    print(f"   ... and {len(sorted_programs)-20} more programs")
        else:
            print(f"❌ Function {function} not found in analysis")
    
    def interactive_module_analysis(self, module_name):
        """Enhanced module analysis with hierarchy_navigator formatting"""
        if not hasattr(self, '_module_stats'):
            print("❌ Module analysis not available. Run full analysis first.")
            return
            
        if module_name not in self._module_stats:
            print(f"❌ Module {module_name} not found")
            available = list(self._module_stats.keys())[:10]
            print(f"Available modules: {', '.join(available)}...")
            return
        
        module = self._module_stats[module_name]
        print(f"\n🏢 MODULE ANALYSIS: {module_name}")
        print("="*80)
        print(f"📊 Programs: {module['program_count']}")
        print(f"📊 Functions: {module['function_count']}")
        print(f"📊 Internal Calls: {module['internal_calls']}")
        print(f"📊 External Calls: {module['external_calls']}")
        print(f"📊 Dependencies: {module['dependency_count']}")
        
        if module['dependencies']:
            print(f"\n🔗 DEPENDS ON MODULES:")
            for i, dep_module in enumerate(module['dependencies'], 1):
                dep_info = self._module_stats.get(dep_module, {})
                print(f"   {i:2d}. {dep_module:<30} ({dep_info.get('program_count', 0)} programs)")
        
        # Find modules that depend on this one
        dependents = []
        for mod_name, mod_data in self._module_stats.items():
            if module_name in mod_data.get('dependencies', []):
                dependents.append((mod_name, mod_data))
        
        if dependents:
            print(f"\n📈 MODULES THAT DEPEND ON {module_name}:")
            for i, (dep_name, dep_data) in enumerate(dependents, 1):
                print(f"   {i:2d}. {dep_name:<30} ({dep_data.get('program_count', 0)} programs)")
    
    def interactive_critical_paths(self, max_paths=10):
        """Enhanced critical paths display with hierarchy_navigator formatting"""
        print(f"\n🎯 TOP {max_paths} CRITICAL DEPENDENCY PATHS")
        print("="*80)
        
        # Get critical functions from existing analysis
        critical_functions = []
        for function, dependents in self.impact_chains.items():
            if len(dependents) > 0:  # Has dependents
                critical_functions.append({
                    'function': function,
                    'dependent_count': len(dependents),
                    'usage_count': self.usage_counts.get(function, 0),
                    'dependency_level': self.dependency_levels.get(function, 0),
                    'dependents': list(dependents)
                })
        
        # Sort by impact
        critical_functions.sort(key=lambda x: x['dependent_count'], reverse=True)
        critical_functions = critical_functions[:max_paths]
        
        for i, func_info in enumerate(critical_functions, 1):
            function = func_info['function']
            level = func_info['dependency_level']
            dependents = func_info['dependent_count']
            usage = func_info['usage_count']
            
            print(f"\n{i:2d}. {function}")
            print(f"    Level: {level} | Dependents: {dependents:,} | Usage: {usage}")
            
            # Show impact breakdown
            if dependents > 0:
                # Group dependents by program
                dependent_programs = set()
                for dep in func_info.get('dependents', [])[:100]:  # Limit for performance
                    dependent_programs.add(dep.split('.')[0])
                
                print(f"    💥 Impact: Affects {len(dependent_programs)} programs")
                
                # Show sample programs
                sample_programs = sorted(list(dependent_programs))[:5]
                print(f"    📁 Sample Programs: {', '.join(sample_programs)}")
                
                if len(dependent_programs) > 5:
                    print(f"       ... and {len(dependent_programs)-5} more programs")
    
    def interactive_bottom_up_trace(self, leaf_function):
        """Enhanced bottom-up trace with hierarchy_navigator formatting"""
        print(f"\n🌱 BOTTOM-UP TRACE FROM: {leaf_function}")
        print("="*80)
        
        # Check if it's a leaf function
        if leaf_function not in self.leaf_functions:
            print(f"❌ {leaf_function} is not a leaf function")
            print(f"Available leaf functions: {list(self.leaf_functions)[:10]}...")
            return
        
        # Find all functions that depend on this leaf
        dependents = self.impact_chains.get(leaf_function, set())
        
        if not dependents:
            print(f"ℹ️  {leaf_function} has no dependents (unused function)")
            return
        
        print(f"📊 This leaf function is used by {len(dependents)} functions")
        
        # Group by dependency level
        dependents_by_level = defaultdict(list)
        for dep in dependents:
            dep_level = self.dependency_levels.get(dep)
            if dep_level is not None:
                dependents_by_level[dep_level].append(dep)
        
        print(f"\n📈 USAGE BY DEPENDENCY LEVEL (Bottom → Top):")
        for level in sorted(dependents_by_level.keys()):
            functions = dependents_by_level[level]
            print(f"\n   Level {level}: {len(functions)} functions")
            
            # Group by program
            by_program = defaultdict(list)
            for func in functions:
                program = func.split('.')[0]
                by_program[program].append(func)
            
            # Show top programs at this level
            sorted_programs = sorted(by_program.items(), key=lambda x: len(x[1]), reverse=True)
            for program, funcs in sorted_programs[:5]:
                print(f"      {program:<20} → {len(funcs)} functions")
            
            if len(sorted_programs) > 5:
                print(f"      ... and {len(sorted_programs)-5} more programs")
    
    def interactive_hierarchy_overview(self):
        """Enhanced hierarchy overview with hierarchy_navigator formatting"""
        if not hasattr(self, '_module_stats'):
            print("❌ Module hierarchy not available. Run full analysis first.")
            return
            
        print(f"\n🏢 MODULE HIERARCHY OVERVIEW")
        print("="*80)
        print(f"Total Modules: {len(self._module_stats)}")
        
        # Calculate module dependencies
        dependency_levels = defaultdict(list)
        
        for module_name, module_data in self._module_stats.items():
            dep_count = len(module_data.get('dependencies', []))
            dependency_levels[dep_count].append((module_name, module_data))
        
        print(f"\n📊 MODULES BY DEPENDENCY COUNT:")
        for dep_count in sorted(dependency_levels.keys(), reverse=True)[:10]:
            modules = dependency_levels[dep_count]
            print(f"\n   {dep_count:2d} Dependencies ({len(modules)} modules):")
            
            # Show top modules at this dependency level
            sorted_modules = sorted(modules, key=lambda x: x[1]['program_count'], reverse=True)
            for module_name, module_data in sorted_modules[:5]:
                print(f"      • {module_name:<25} ({module_data['program_count']:3d} programs)")

def main():
    parser = argparse.ArgumentParser(
        description="IBM i Dependency Analysis and Interactive Navigation Tool - See IBM_i_DEPENDENCY_ANALYSIS_GUIDE.md for complete usage documentation",
        epilog="For detailed examples and strategic modernization guidance, refer to IBM_i_DEPENDENCY_ANALYSIS_GUIDE.md"
    )
    parser.add_argument("csv_file", help="Path to call analysis CSV file")
    parser.add_argument("--overview", help="Path to overview CSV file for implementation analysis")
    parser.add_argument("--output", "-o", default="dependency_analysis", help="Output file prefix")
    parser.add_argument("--function", "-f", help="Analyze specific function")
    parser.add_argument("--module", "-m", help="Analyze specific module")
    parser.add_argument("--impact", "-i", help="Show impact analysis for function")
    parser.add_argument("--critical", "-c", type=int, default=10, help="Show N critical paths")
    parser.add_argument("--bottom-up", "-b", help="Trace bottom-up from leaf function")
    parser.add_argument("--hierarchy", action="store_true", help="Show module hierarchy overview")
    parser.add_argument("--interactive", action="store_true", help="Enter interactive navigation mode")
    
    args = parser.parse_args()
    
    # Initialize analyzer
    analyzer = DependencyAnalyzer(args.csv_file, args.overview)
    
    # Load and process data
    analyzer.load_data()
    analyzer.load_implementation_data()  # NEW: Load implementation data
    analyzer.calculate_dependency_levels()
    analyzer.build_impact_chains()
    analyzer.auto_detect_modules()
    
    # Generate reports
    dependency_report = analyzer.generate_bottom_up_analysis(f"{args.output}.json")
    module_report = analyzer.generate_module_map(f"{args.output}_modules.json")
    
    # Store module stats for interactive mode
    analyzer._module_stats = module_report
    
    # Interactive mode
    if args.interactive:
        analyzer.interactive_navigation()
        return
    
    # Handle specific queries
    if args.function:
        analyzer.interactive_function_analysis(args.function)
    
    if args.module:
        analyzer.interactive_module_analysis(args.module)
    
    if args.hierarchy:
        analyzer.interactive_hierarchy_overview()
    
    if args.bottom_up:
        analyzer.interactive_bottom_up_trace(args.bottom_up)
    
    if args.critical:
        analyzer.interactive_critical_paths(args.critical)

if __name__ == "__main__":
    main()