#!/usr/bin/env python3
"""
Call Hierarchy Visualization Tool
Generates complete call trees with LEAF node highlighting
"""

import csv
import argparse
from collections import defaultdict
from graphviz import Digraph
import os

class CallHierarchyVisualizer:
    """Visualizes complete call hierarchy including LEAF nodes"""

    def __init__(self, csv_path):
        self.csv_path = csv_path
        self.call_graph = defaultdict(list)
        self.leaf_nodes = set()
        self.call_nodes = set()
        self.program_functions = defaultdict(set)

    def load_data(self):
        """Load call analysis CSV"""
        print(f"Loading data from {self.csv_path}...")

        with open(self.csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                caller = row['Caller']
                caller_func = row['Caller Function']
                callee = row['Callee Program']
                callee_func = row['Callee Function']
                call_type = row['Call Type']
                record_type = row['Record Type']

                # Create node IDs
                caller_node = f"{caller}.{caller_func}"

                # Track all functions in each program
                self.program_functions[caller].add(caller_func)

                if record_type == 'LEAF':
                    # This is a leaf node (terminal function)
                    self.leaf_nodes.add(caller_node)
                else:
                    # This is a call relationship
                    callee_node = f"{callee}.{callee_func}"
                    self.call_graph[caller_node].append({
                        'target': callee_node,
                        'call_type': call_type
                    })
                    self.call_nodes.add(caller_node)

        print(f"Loaded {len(self.call_nodes)} calling functions")
        print(f"Loaded {len(self.leaf_nodes)} leaf functions")
        print(f"Total programs: {len(self.program_functions)}")

    def visualize_program(self, program_name, output_file=None, max_depth=3):
        """Generate call tree for a specific program"""
        print(f"\nGenerating call tree for program: {program_name}")

        if program_name not in self.program_functions:
            print(f"Program {program_name} not found in dataset")
            return

        # Create Graphviz graph
        dot = Digraph(comment=f'Call Hierarchy: {program_name}')
        dot.attr(rankdir='TB')  # Top to bottom
        dot.attr('node', shape='box', style='rounded,filled')

        # Set graph attributes for better layout
        dot.attr(splines='ortho')
        dot.attr(nodesep='0.5')
        dot.attr(ranksep='0.8')

        visited = set()

        def add_nodes_recursive(node_id, depth=0):
            """Recursively add nodes and edges"""
            if depth > max_depth or node_id in visited:
                return

            visited.add(node_id)

            # Determine node style
            if node_id in self.leaf_nodes:
                # LEAF node - terminal function
                dot.node(node_id, node_id,
                        fillcolor='lightcoral',
                        fontcolor='black',
                        tooltip='LEAF: Makes no calls')
            elif node_id in self.call_nodes:
                # CALL node - makes calls
                dot.node(node_id, node_id,
                        fillcolor='lightblue',
                        fontcolor='black',
                        tooltip='CALL: Makes calls to other functions')
            else:
                # Referenced but not in this program
                dot.node(node_id, node_id,
                        fillcolor='lightgray',
                        fontcolor='black',
                        tooltip='External reference')

            # Add edges to callees
            if node_id in self.call_graph:
                for call_info in self.call_graph[node_id]:
                    target = call_info['target']
                    call_type = call_info['call_type']

                    # Add edge with call type label
                    dot.edge(node_id, target, label=call_type, fontsize='10')

                    # Recurse to target
                    add_nodes_recursive(target, depth + 1)

        # Start from all functions in the program
        for func in self.program_functions[program_name]:
            start_node = f"{program_name}.{func}"
            add_nodes_recursive(start_node)

        # Add legend
        with dot.subgraph(name='cluster_legend') as legend:
            legend.attr(label='Legend', style='filled', color='lightgrey')
            legend.node('legend_call', 'CALL Node\n(Makes calls)',
                       fillcolor='lightblue', shape='box', style='rounded,filled')
            legend.node('legend_leaf', 'LEAF Node\n(Terminal)',
                       fillcolor='lightcoral', shape='box', style='rounded,filled')
            legend.node('legend_ext', 'External\n(Other programs)',
                       fillcolor='lightgray', shape='box', style='rounded,filled')

        # Render
        if output_file is None:
            output_file = f'call_tree_{program_name}'

        print(f"Rendering to {output_file}.pdf and {output_file}.png")
        dot.render(output_file, format='pdf', cleanup=True)
        dot.render(output_file, format='png', cleanup=True)

        print(f"✅ Visualization complete: {output_file}.pdf")
        print(f"   Functions visualized: {len(visited)}")
        print(f"   - CALL nodes: {len([n for n in visited if n in self.call_nodes])}")
        print(f"   - LEAF nodes: {len([n for n in visited if n in self.leaf_nodes])}")

    def visualize_function_call_chain(self, start_program, start_function, output_file=None, max_depth=5):
        """Generate call chain starting from a specific function"""
        start_node = f"{start_program}.{start_function}"

        if start_node not in self.call_nodes and start_node not in self.leaf_nodes:
            print(f"Function {start_node} not found in dataset")
            return

        print(f"\nGenerating call chain from: {start_node}")

        # Create Graphviz graph
        dot = Digraph(comment=f'Call Chain: {start_node}')
        dot.attr(rankdir='TB')
        dot.attr('node', shape='box', style='rounded,filled')
        dot.attr(splines='ortho')

        visited = set()

        def trace_calls(node_id, depth=0):
            """Trace all calls from this node"""
            if depth > max_depth or node_id in visited:
                return

            visited.add(node_id)

            # Add node
            if node_id in self.leaf_nodes:
                dot.node(node_id, node_id, fillcolor='lightcoral',
                        tooltip='LEAF: Terminal function')
            elif node_id in self.call_nodes:
                dot.node(node_id, node_id, fillcolor='lightblue',
                        tooltip='CALL: Makes calls')
            else:
                dot.node(node_id, node_id, fillcolor='lightgray',
                        tooltip='External')

            # Trace callees
            if node_id in self.call_graph:
                for call_info in self.call_graph[node_id]:
                    target = call_info['target']
                    call_type = call_info['call_type']

                    dot.edge(node_id, target, label=call_type)
                    trace_calls(target, depth + 1)

        trace_calls(start_node)

        # Render
        if output_file is None:
            output_file = f'call_chain_{start_program}_{start_function}'

        print(f"Rendering to {output_file}.pdf")
        dot.render(output_file, format='pdf', cleanup=True)
        dot.render(output_file, format='png', cleanup=True)

        print(f"✅ Call chain complete: {output_file}.pdf")
        print(f"   Total functions in chain: {len(visited)}")

    def generate_program_summary(self, program_name):
        """Generate text summary of program structure"""
        if program_name not in self.program_functions:
            print(f"Program {program_name} not found")
            return

        print(f"\n{'='*70}")
        print(f"PROGRAM SUMMARY: {program_name}")
        print(f"{'='*70}")

        functions = sorted(self.program_functions[program_name])

        for func in functions:
            node_id = f"{program_name}.{func}"

            if node_id in self.leaf_nodes:
                print(f"\n📍 {func} [LEAF - Terminal Function]")
                print(f"   └─ Makes NO calls (end of call chain)")
            elif node_id in self.call_graph:
                calls = self.call_graph[node_id]
                print(f"\n📞 {func} [CALL - Makes {len(calls)} call(s)]")
                for call_info in calls:
                    target = call_info['target']
                    call_type = call_info['call_type']
                    print(f"   ├─ {call_type} → {target}")
            else:
                print(f"\n❓ {func} [UNKNOWN]")

        # Statistics
        total_funcs = len(functions)
        leaf_count = len([f for f in functions if f"{program_name}.{f}" in self.leaf_nodes])
        call_count = len([f for f in functions if f"{program_name}.{f}" in self.call_nodes])

        print(f"\n{'='*70}")
        print(f"STATISTICS:")
        print(f"  Total functions: {total_funcs}")
        print(f"  CALL functions:  {call_count} ({100*call_count/total_funcs:.1f}%)")
        print(f"  LEAF functions:  {leaf_count} ({100*leaf_count/total_funcs:.1f}%)")
        print(f"{'='*70}")

    def find_deepest_call_chains(self, top_n=10):
        """Find the longest call chains in the codebase"""
        print("\n🔍 Finding deepest call chains...")

        def calculate_depth(node_id, visited=None):
            """Calculate maximum depth from this node"""
            if visited is None:
                visited = set()

            if node_id in visited:  # Circular reference
                return 0

            if node_id in self.leaf_nodes:
                return 1

            if node_id not in self.call_graph:
                return 1

            visited.add(node_id)
            max_child_depth = 0

            for call_info in self.call_graph[node_id]:
                target = call_info['target']
                depth = calculate_depth(target, visited.copy())
                max_child_depth = max(max_child_depth, depth)

            return max_child_depth + 1

        # Calculate depths
        depths = {}
        for node in self.call_nodes:
            depths[node] = calculate_depth(node)

        # Sort by depth
        sorted_chains = sorted(depths.items(), key=lambda x: x[1], reverse=True)[:top_n]

        print(f"\n{'='*70}")
        print(f"TOP {top_n} DEEPEST CALL CHAINS:")
        print(f"{'='*70}")

        for rank, (node, depth) in enumerate(sorted_chains, 1):
            print(f"{rank:2d}. {node:40s} → Depth: {depth} levels")

        return sorted_chains


def main():
    parser = argparse.ArgumentParser(
        description='Visualize IBM i Call Hierarchy with LEAF node support'
    )
    parser.add_argument('csv_file', help='Path to call analysis CSV')
    parser.add_argument('--program', '-p', help='Program name to visualize')
    parser.add_argument('--function', '-f', help='Function name (requires --program)')
    parser.add_argument('--summary', '-s', help='Generate text summary for program')
    parser.add_argument('--depth', '-d', type=int, default=3, help='Maximum depth (default: 3)')
    parser.add_argument('--top-chains', '-t', type=int, help='Find N deepest call chains')
    parser.add_argument('--output', '-o', help='Output file name (without extension)')

    args = parser.parse_args()

    # Initialize visualizer
    viz = CallHierarchyVisualizer(args.csv_file)
    viz.load_data()

    # Execute requested operation
    if args.top_chains:
        viz.find_deepest_call_chains(args.top_chains)

    if args.summary:
        viz.generate_program_summary(args.summary)

    if args.program:
        if args.function:
            viz.visualize_function_call_chain(
                args.program, args.function, args.output, args.depth
            )
        else:
            viz.visualize_program(args.program, args.output, args.depth)


if __name__ == '__main__':
    main()
