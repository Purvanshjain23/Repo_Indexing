#!/usr/bin/env python3
"""
Simple Call Hierarchy Analyzer - No external dependencies
Demonstrates complete function inventory with LEAF nodes
"""

import csv
from collections import defaultdict

class SimpleCallAnalyzer:
    """Analyze call hierarchy without visualization libraries"""

    def __init__(self, csv_path):
        self.csv_path = csv_path
        self.call_graph = defaultdict(list)
        self.leaf_nodes = set()
        self.call_nodes = set()
        self.program_functions = defaultdict(set)
        self.all_records = []

    def load_data(self):
        """Load call analysis CSV"""
        print(f"Loading data from {self.csv_path}...")

        with open(self.csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                self.all_records.append(row)

                caller = row['Caller']
                caller_func = row['Caller Function']
                callee = row['Callee Program']
                callee_func = row['Callee Function']
                call_type = row['Call Type']
                record_type = row['Record Type']

                caller_node = f"{caller}.{caller_func}"
                self.program_functions[caller].add(caller_func)

                if record_type == 'LEAF':
                    self.leaf_nodes.add(caller_node)
                else:
                    callee_node = f"{callee}.{callee_func}"
                    self.call_graph[caller_node].append({
                        'target': callee_node,
                        'call_type': call_type,
                        'callee': callee,
                        'callee_func': callee_func
                    })
                    self.call_nodes.add(caller_node)

        print(f"[OK] Loaded {len(self.all_records):,} records")
        print(f"     - CALL records: {len(self.call_nodes):,}")
        print(f"     - LEAF records: {len(self.leaf_nodes):,}")
        print(f"     - Total programs: {len(self.program_functions)}")

    def show_program_summary(self, program_name):
        """Generate detailed program summary"""
        if program_name not in self.program_functions:
            print(f"\n[ERROR] Program {program_name} not found in dataset")
            print(f"   Hint: Try one of these programs:")
            sample_programs = sorted(list(self.program_functions.keys()))[:20]
            for prog in sample_programs:
                print(f"      - {prog}")
            return

        print(f"\n{'='*80}")
        print(f"COMPLETE FUNCTION INVENTORY: {program_name}")
        print(f"{'='*80}")

        functions = sorted(self.program_functions[program_name])

        for idx, func in enumerate(functions, 1):
            node_id = f"{program_name}.{func}"

            print(f"\n[{idx}/{len(functions)}] Function: {func}")

            if node_id in self.leaf_nodes:
                print(f"     Type: [LEAF] LEAF (Terminal Function)")
                print(f"     Calls: None - End of call chain")
                print(f"     Impact: [OK] Safe to modify (no downstream dependencies)")
            elif node_id in self.call_graph:
                calls = self.call_graph[node_id]
                print(f"     Type: [CALL] CALL (Makes {len(calls)} call(s))")
                print(f"     Calls:")
                for call_info in calls:
                    target = call_info['target']
                    call_type = call_info['call_type']
                    print(f"       * {call_type:12s} -> {target}")
                print(f"     Impact: [WARNING]  Careful - Affects {len(calls)} function(s)")
            else:
                print(f"     Type: [UNKNOWN] UNKNOWN")

        # Statistics
        total_funcs = len(functions)
        leaf_count = len([f for f in functions if f"{program_name}.{f}" in self.leaf_nodes])
        call_count = len([f for f in functions if f"{program_name}.{f}" in self.call_nodes])

        print(f"\n{'='*80}")
        print(f"PROGRAM STATISTICS:")
        print(f"{'='*80}")
        print(f"  Total functions:    {total_funcs:3d}")
        print(f"  CALL functions:     {call_count:3d} ({100*call_count/total_funcs:.1f}% - Make calls to others)")
        print(f"  LEAF functions:     {leaf_count:3d} ({100*leaf_count/total_funcs:.1f}% - Terminal nodes)")
        print(f"{'='*80}")

        # Complexity assessment
        if leaf_count / total_funcs > 0.5:
            print("\n[OK] LOW COUPLING: More than 50% leaf functions")
            print("   -> Good candidate for refactoring/microservices")
        elif leaf_count / total_funcs < 0.2:
            print("\n[WARNING]  HIGH COUPLING: Less than 20% leaf functions")
            print("   -> Consider breaking down into smaller functions")

    def trace_call_chain(self, program_name, function_name, max_depth=5):
        """Trace complete call chain from a function"""
        start_node = f"{program_name}.{function_name}"

        if start_node not in self.call_nodes and start_node not in self.leaf_nodes:
            print(f"\n[ERROR] Function {start_node} not found")
            return

        print(f"\n{'='*80}")
        print(f"CALL CHAIN TRACE: {start_node}")
        print(f"{'='*80}")

        visited = set()

        def trace_recursive(node_id, depth=0, prefix=""):
            """Trace calls recursively"""
            if depth > max_depth or node_id in visited:
                if depth > max_depth:
                    print(f"{prefix}`-- ... (max depth {max_depth} reached)")
                return

            visited.add(node_id)

            if node_id in self.leaf_nodes:
                print(f"{prefix}`-- [LEAF] {node_id} [LEAF - Terminal]")
            elif node_id in self.call_graph:
                is_last = True
                calls = self.call_graph[node_id]
                print(f"{prefix}|-- [CALL] {node_id} [CALL - {len(calls)} call(s)]")

                for idx, call_info in enumerate(calls):
                    target = call_info['target']
                    call_type = call_info['call_type']
                    is_last_call = (idx == len(calls) - 1)

                    new_prefix = prefix + ("    " if is_last else "|   ")
                    connector = "`--" if is_last_call else "|--"

                    print(f"{new_prefix}{connector} {call_type} ->")
                    trace_recursive(target, depth + 1, new_prefix + ("    " if is_last_call else "|   "))
            else:
                print(f"{prefix}`-- [EXTERNAL] {node_id} [External]")

        trace_recursive(start_node)

        print(f"\n{'='*80}")
        print(f"Chain Statistics:")
        print(f"  Total functions in chain: {len(visited)}")
        print(f"  Maximum depth reached: {max_depth}")
        print(f"{'='*80}")

    def find_top_programs_by_complexity(self, top_n=20):
        """Find most complex programs"""
        print(f"\n{'='*80}")
        print(f"TOP {top_n} MOST COMPLEX PROGRAMS (by function count)")
        print(f"{'='*80}")

        program_stats = []
        for program, functions in self.program_functions.items():
            func_count = len(functions)
            leaf_count = len([f for f in functions if f"{program}.{f}" in self.leaf_nodes])
            call_count = len([f for f in functions if f"{program}.{f}" in self.call_nodes])

            program_stats.append({
                'program': program,
                'total': func_count,
                'calls': call_count,
                'leafs': leaf_count,
                'coupling': (call_count / func_count * 100) if func_count > 0 else 0
            })

        # Sort by total functions
        program_stats.sort(key=lambda x: x['total'], reverse=True)

        print(f"\n{'Program':<20} {'Total':>6} {'CALL':>6} {'LEAF':>6} {'Coupling':>10}")
        print(f"{'-'*20} {'-'*6} {'-'*6} {'-'*6} {'-'*10}")

        for stat in program_stats[:top_n]:
            print(f"{stat['program']:<20} {stat['total']:>6} "
                  f"{stat['calls']:>6} {stat['leafs']:>6} "
                  f"{stat['coupling']:>9.1f}%")

        print(f"\n{'='*80}")

    def find_deepest_chains(self, top_n=20):
        """Find deepest call chains"""
        print(f"\n{'='*80}")
        print(f"TOP {top_n} DEEPEST CALL CHAINS")
        print(f"{'='*80}")

        def calculate_depth(node_id, visited=None):
            if visited is None:
                visited = set()

            if node_id in visited:
                return 0

            if node_id in self.leaf_nodes:
                return 1

            if node_id not in self.call_graph:
                return 1

            visited.add(node_id)
            max_depth = 0

            for call_info in self.call_graph[node_id]:
                target = call_info['target']
                depth = calculate_depth(target, visited.copy())
                max_depth = max(max_depth, depth)

            return max_depth + 1

        depths = {}
        for node in self.call_nodes:
            depths[node] = calculate_depth(node)

        sorted_chains = sorted(depths.items(), key=lambda x: x[1], reverse=True)[:top_n]

        print(f"\n{'Rank':<5} {'Function':<50} {'Depth':>6}")
        print(f"{'-'*5} {'-'*50} {'-'*6}")

        for rank, (node, depth) in enumerate(sorted_chains, 1):
            print(f"{rank:<5} {node:<50} {depth:>6}")

        print(f"\n{'='*80}")


def main():
    import sys

    if len(sys.argv) < 2:
        print("Usage:")
        print("  python analyze_call_hierarchy.py <csv_file> --summary <program>")
        print("  python analyze_call_hierarchy.py <csv_file> --trace <program> <function>")
        print("  python analyze_call_hierarchy.py <csv_file> --top-programs")
        print("  python analyze_call_hierarchy.py <csv_file> --deepest-chains")
        sys.exit(1)

    csv_file = sys.argv[1]
    analyzer = SimpleCallAnalyzer(csv_file)
    analyzer.load_data()

    if '--summary' in sys.argv:
        idx = sys.argv.index('--summary')
        program = sys.argv[idx + 1]
        analyzer.show_program_summary(program)

    elif '--trace' in sys.argv:
        idx = sys.argv.index('--trace')
        program = sys.argv[idx + 1]
        function = sys.argv[idx + 2]
        analyzer.trace_call_chain(program, function)

    elif '--top-programs' in sys.argv:
        analyzer.find_top_programs_by_complexity(30)

    elif '--deepest-chains' in sys.argv:
        analyzer.find_deepest_chains(30)

    else:
        print("Please specify --summary, --trace, --top-programs, or --deepest-chains")


if __name__ == '__main__':
    main()
