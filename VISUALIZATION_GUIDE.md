# Call Hierarchy Visualization Guide

Complete guide to visualizing your IBM i repository call hierarchy with LEAF node support.

---

## 🎯 Quick Start

### **Prerequisites:**

```bash
pip install graphviz pandas networkx plotly
```

**Note**: Also install Graphviz system package from https://graphviz.org/download/

---

## 📊 Usage Examples

### **1. Visualize a Complete Program**

Shows all functions in a program, including LEAF nodes:

```bash
python visualize_call_hierarchy.py repository_call_analysis_20251006_121452.csv --program ACTBALS
```

**Output**: `call_tree_ACTBALS.pdf` and `.png`

**What you'll see**:
- 🔵 Blue boxes: Functions that make calls
- 🔴 Red boxes: LEAF functions (terminal nodes)
- ⚪ Gray boxes: External programs called
- Arrows show call relationships with call types (CALL, EXSR, CALLP, etc.)

---

### **2. Trace a Specific Function's Call Chain**

Follow the complete call chain from one function:

```bash
python visualize_call_hierarchy.py repository_call_analysis_20251006_121452.csv \
    --program ACTBALS --function MAIN --depth 5
```

**Output**: `call_chain_ACTBALS_MAIN.pdf`

**What you'll see**:
- Starting function at the top
- All functions it calls (directly and indirectly)
- Complete chain down to LEAF nodes
- Maximum 5 levels deep (configurable)

---

### **3. Generate Program Summary Report**

Text-based analysis of program structure:

```bash
python visualize_call_hierarchy.py repository_call_analysis_20251006_121452.csv --summary ACTBALS
```

**Output** (terminal):
```
======================================================================
PROGRAM SUMMARY: ACTBALS
======================================================================

📞 MAIN [CALL - Makes 3 call(s)]
   ├─ CALL → CUSTMAINT.MAIN
   ├─ EXSR → ACTBALS.INITSUBR
   └─ EXSR → ACTBALS.PROCESSREC

📞 INITSUBR [CALL - Makes 2 call(s)]
   ├─ EXSR → ACTBALS.OPENFILES
   └─ CALL → UTILITY.LOGGING

📍 OPENFILES [LEAF - Terminal Function]
   └─ Makes NO calls (end of call chain)

📍 PROCESSREC [LEAF - Terminal Function]
   └─ Makes NO calls (end of call chain)

======================================================================
STATISTICS:
  Total functions: 4
  CALL functions:  2 (50.0%)
  LEAF functions:  2 (50.0%)
======================================================================
```

---

### **4. Find Deepest Call Chains**

Identify the most complex call hierarchies:

```bash
python visualize_call_hierarchy.py repository_call_analysis_20251006_121452.csv --top-chains 20
```

**Output**:
```
======================================================================
TOP 20 DEEPEST CALL CHAINS:
======================================================================
 1. MAINPGM.MAIN                           → Depth: 12 levels
 2. ARJOPVR.VALIDATE                       → Depth: 10 levels
 3. CUSTMAINT.PROCESS                      → Depth: 9 levels
...
```

**Use case**: Find refactoring candidates (deeply nested = high complexity)

---

### **5. Custom Output Location**

```bash
python visualize_call_hierarchy.py repository_call_analysis_20251006_121452.csv \
    --program ACTBALS --output ./diagrams/actbals_complete
```

**Output**: `./diagrams/actbals_complete.pdf`

---

## 🎨 Advanced Visualizations

### **Option A: Interactive HTML with Plotly**

Create `interactive_call_graph.py`:

```python
import pandas as pd
import plotly.graph_objects as go
import networkx as nx

# Load data
df = pd.read_csv('repository_call_analysis_20251006_121452.csv')

# Filter to a specific program or subsystem
df_subset = df[df['Caller'].str.startswith('ACT')]  # All ACT* programs

# Build graph
G = nx.DiGraph()

for _, row in df_subset.iterrows():
    source = f"{row['Caller']}.{row['Caller Function']}"

    if row['Record Type'] == 'CALL':
        target = f"{row['Callee Program']}.{row['Callee Function']}"
        G.add_edge(source, target, call_type=row['Call Type'])
    else:
        # LEAF node - add as isolated node
        G.add_node(source, node_type='LEAF')

# Position nodes
pos = nx.spring_layout(G, k=0.5, iterations=50)

# Create edge trace
edge_trace = []
for edge in G.edges():
    x0, y0 = pos[edge[0]]
    x1, y1 = pos[edge[1]]
    edge_trace.append(
        go.Scatter(x=[x0, x1, None], y=[y0, y1, None],
                   mode='lines', line=dict(width=0.5, color='#888'))
    )

# Create node trace
node_x, node_y, node_color, node_text = [], [], [], []

for node in G.nodes():
    x, y = pos[node]
    node_x.append(x)
    node_y.append(y)

    # Color by type
    if G.nodes[node].get('node_type') == 'LEAF':
        node_color.append('red')
        node_text.append(f"{node}<br>LEAF")
    else:
        node_color.append('blue')
        node_text.append(f"{node}<br>CALL")

node_trace = go.Scatter(
    x=node_x, y=node_y, mode='markers+text',
    marker=dict(size=10, color=node_color),
    text=node_text, textposition="top center"
)

# Create figure
fig = go.Figure(data=edge_trace + [node_trace],
                layout=go.Layout(
                    title='Interactive Call Hierarchy',
                    showlegend=False,
                    hovermode='closest',
                    xaxis=dict(showgrid=False, zeroline=False),
                    yaxis=dict(showgrid=False, zeroline=False)
                ))

fig.write_html('interactive_call_graph.html')
print("✅ Interactive graph saved to interactive_call_graph.html")
```

**Run**:
```bash
python interactive_call_graph.py
```

**Output**: Interactive HTML file with zoom, pan, hover tooltips

---

### **Option B: Circular Hierarchy with D3.js**

Create `generate_d3_hierarchy.py`:

```python
import pandas as pd
import json

df = pd.read_csv('repository_call_analysis_20251006_121452.csv')

def build_hierarchy(program_name):
    """Build hierarchical JSON for D3.js"""
    df_program = df[df['Caller'] == program_name]

    def build_node(func_name):
        node_id = f"{program_name}.{func_name}"
        calls = df_program[df_program['Caller Function'] == func_name]

        if len(calls) == 0 or calls.iloc[0]['Record Type'] == 'LEAF':
            return {
                "name": func_name,
                "type": "LEAF",
                "children": []
            }

        children = []
        for _, call in calls.iterrows():
            if call['Record Type'] == 'CALL':
                children.append({
                    "name": f"{call['Callee Program']}.{call['Callee Function']}",
                    "type": "CALL",
                    "call_type": call['Call Type']
                })

        return {
            "name": func_name,
            "type": "CALL",
            "children": children
        }

    # Start with MAIN
    root = build_node('MAIN')

    return {
        "name": program_name,
        "children": [root]
    }

# Generate for multiple programs
programs = ['ACTBALS', 'ACTSEL', 'ACTRET']
hierarchies = {prog: build_hierarchy(prog) for prog in programs}

# Save as JSON
with open('call_hierarchy_d3.json', 'w') as f:
    json.dump(hierarchies, f, indent=2)

print("✅ D3.js hierarchy saved to call_hierarchy_d3.json")
print("   Use with D3.js tree/radial layout for interactive visualization")
```

---

## 📈 Use Case Examples

### **Use Case 1: Impact Analysis Before Refactoring**

**Scenario**: Need to refactor CUSTMAINT program

```bash
# Step 1: See complete structure
python visualize_call_hierarchy.py calls.csv --summary CUSTMAINT

# Step 2: Visualize to identify LEAF functions
python visualize_call_hierarchy.py calls.csv --program CUSTMAINT --depth 5

# Step 3: Find who calls CUSTMAINT (reverse lookup - requires custom script)
```

**Result**:
- Identify 5 LEAF functions that can be tested independently
- See 12 external programs that depend on CUSTMAINT
- Plan refactoring starting from LEAF nodes (no dependencies)

---

### **Use Case 2: Dead Code Detection**

**Scenario**: Find unused functions

```python
# Custom script: find_dead_code.py
import pandas as pd

df = pd.read_csv('repository_call_analysis_20251006_121452.csv')

# Find LEAF nodes
leaf_functions = df[df['Record Type'] == 'LEAF'][['Caller', 'Caller Function']]

# Find which functions are never called
all_callees = df[df['Record Type'] == 'CALL'][['Callee Program', 'Callee Function']]
all_callees.columns = ['Caller', 'Caller Function']

# LEAF functions that nobody calls = potential dead code
dead_code = leaf_functions.merge(
    all_callees,
    on=['Caller', 'Caller Function'],
    how='left',
    indicator=True
)

dead_code = dead_code[dead_code['_merge'] == 'left_only']

print(f"Found {len(dead_code)} potential dead code functions")
print(dead_code[['Caller', 'Caller Function']].to_string())
```

---

### **Use Case 3: Documentation Generation**

**Scenario**: Generate architecture documentation

```bash
# Visualize top 10 most complex programs
for prog in ACTBALS ACTSEL CUSTMAINT ORDPROC INVMAINT; do
    python visualize_call_hierarchy.py calls.csv --program $prog --output docs/$prog
done

# Combine into PDF documentation
# (use your favorite PDF merger)
```

---

### **Use Case 4: Modernization Planning**

**Scenario**: Identify programs for microservices conversion

```bash
# Find programs with shallow hierarchies (good microservice candidates)
python visualize_call_hierarchy.py calls.csv --top-chains 100

# Programs with depth 1-3 = good candidates
# Programs with depth 10+ = refactor first before converting
```

---

## 🎯 Visualization Types Explained

### **Call Tree (Hierarchical)**
```
MAIN [CALL]
 ├─ INITSUBR [CALL]
 │   ├─ OPENFILES [LEAF]
 │   └─ LOADCONFIG [LEAF]
 ├─ PROCESSREC [CALL]
 │   ├─ VALIDATE [CALL]
 │   │   └─ CHECKRULES [LEAF]
 │   └─ SAVE [LEAF]
 └─ CLEANUP [LEAF]
```

**Use**: Understanding execution flow, identifying bottlenecks

---

### **Call Graph (Network)**
```
      [MAIN]
      /  |  \
    /    |    \
[INIT] [PROC] [CLEAN]
  |      |
[OPEN] [VALID]
  |      |
[LEAF] [LEAF]
```

**Use**: Dependency analysis, finding circular references

---

### **Sunburst/Radial (D3.js)**
```
        MAIN
       /  |  \
    INIT PROC CLEAN
     |    |
   OPEN VALID
     |    |
   LEAF LEAF
```

**Use**: Executive presentations, architecture overviews

---

## 🔧 Tips & Best Practices

### **Tip 1: Start Small**
Begin with individual programs, not entire repository:
```bash
python visualize_call_hierarchy.py calls.csv --program ACTBALS
```

### **Tip 2: Adjust Depth**
Large programs need depth limiting:
```bash
--depth 3  # Good for overview
--depth 10 # Deep analysis (may be large)
```

### **Tip 3: Use Filters**
Focus on specific subsystems:
```python
df_filtered = df[df['Caller'].str.startswith('ACT')]  # Only ACT* programs
```

### **Tip 4: Color Coding Matters**
- 🔴 Red (LEAF) = Safe to modify (no downstream impact)
- 🔵 Blue (CALL) = Careful (affects other functions)
- ⚪ Gray (External) = Check other programs

---

## 📊 Sample Outputs

### **Before LEAF Tracking:**
```
ACTBALS visualization:
- Shows MAIN → calls 3 functions
- Shows INITSUBR → calls 2 functions
- MISSING: 5 helper functions that don't call anything
- Result: Incomplete picture (only 40% of functions visible)
```

### **After LEAF Tracking:**
```
ACTBALS visualization:
- Shows MAIN → calls 3 functions [CALL]
- Shows INITSUBR → calls 2 functions [CALL]
- Shows OPENFILES [LEAF]
- Shows LOADCONFIG [LEAF]
- Shows CLEANUP [LEAF]
- Shows VALIDATEDATA [LEAF]
- Shows FORMATOUTPUT [LEAF]
- Result: Complete picture (100% of functions visible)
```

---

## 🚀 Next Steps

1. **Install Graphviz**: https://graphviz.org/download/
2. **Run first visualization**: `python visualize_call_hierarchy.py calls.csv --program ACTBALS`
3. **Generate summary reports**: `python visualize_call_hierarchy.py calls.csv --summary ACTBALS`
4. **Find deep chains**: `python visualize_call_hierarchy.py calls.csv --top-chains 20`
5. **Create custom scripts** for specific needs

---

## 📞 Common Patterns to Look For

### **Pattern 1: Long Linear Chain**
```
A → B → C → D → E → F [LEAF]
```
**Issue**: Deep coupling, hard to maintain
**Action**: Consider refactoring to parallel structure

### **Pattern 2: Wide Shallow Tree**
```
     MAIN
    /  |  \  \  \
   A   B  C  D  E  [all LEAF]
```
**Good**: Low coupling, easy to test
**Action**: Good candidate for microservices

### **Pattern 3: Diamond Pattern**
```
    MAIN
   /    \
  A      B
   \    /
     C [LEAF]
```
**Issue**: Multiple paths to same function
**Action**: Verify consistency, potential optimization

### **Pattern 4: Circular Reference**
```
A → B → C → A [circular]
```
**Issue**: Potential infinite loop
**Action**: Review logic, may need refactoring

---

**Your repository has all the data needed for complete visualization thanks to LEAF node tracking!** 🎉
