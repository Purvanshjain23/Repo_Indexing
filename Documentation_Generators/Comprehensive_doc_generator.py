"""
IBM i Comprehensive Documentation Generator - PROFESSIONAL EDITION
=================================================================
🎯 MOST ADVANCED & DETAILED IBM i DOCUMENTATION SYSTEM

🏆 SUPERIOR FEATURES:
- Complete 8-section professional documentation framework
- Individual section builders with detailed business analysis
- Multi-file structure (Sections 1-7 + Section 8 Functions)
- Advanced AI marker system for intelligent completion
- Professional CSS with responsive design (200+ lines)
- JSON trigger system for automated AI processing
- Token-efficient workflow with 75-90% savings
- Executive-ready output with comprehensive business context

🎯 USAGE:
    python ibm_i_comprehensive_doc_generator.py <source_file> <program_name> [output_dir]

📋 EXAMPLES:
    python ibm_i_comprehensive_doc_generator.py "Shipping_modules/OMEIPVR_RPG.txt" "OMEIPVR"
    python ibm_i_comprehensive_doc_generator.py "Shipping_modules/HP4041S_AD.txt" "HP4041S"

✨ COMPREHENSIVE OUTPUT:
    - Professional multi-file structure for complex programs
    - Individual section builders for detailed analysis
    - Advanced business context and modernization insights
    - Complete executive summary and process documentation

What Claude Does (Automatically):
    - Detects JSON trigger in script output
    - Reads source code from specified path
    - Finds all AI markers in HTML templates
    - Fills each marker with analyzed content (2-4 lines, concise)
    - Generates Mermaid flowchart diagram for EVERY function
    - NO need to read 1500-line universal prompt
    - Saves completed files

Token Savings:
    - Traditional: 60K-180K tokens ($0.18-$0.54)
    - Optimized: 8K-20K tokens ($0.02-$0.06)
    - Savings: 75-90% reduction

Quality: Same executive-ready output with massive efficiency gain
"""

import os
import re
import sys
from datetime import datetime
from pathlib import Path


class Stage1StructureGenerator:
    """
    Stage 1: Python-based structure generation
    Creates complete HTML with generic content
    AI will enhance in Stage 2
    """

    def __init__(self, source_file, program_name, output_dir="HTML_Outputs"):
        self.source_file = Path(source_file)
        self.program_name = program_name
        self.output_dir = Path(output_dir)
        self.source_code = self.read_source()
        self.line_count = len(self.source_code.split('\n'))
        self.functions = self.extract_functions()
        self.function_count = len(self.functions)
        self.file_structure = 'single' if self.function_count <= 15 else 'multi'

        print("=" * 80)
        print("IBM i PROFESSIONAL TEMPLATE GENERATOR")
        print("=" * 80)
        print(f"Program: {self.program_name}")
        print(f"Source: {self.source_file.name}")
        print(f"Lines: {self.line_count:,}")
        print(f"Functions: {self.function_count}")
        print(f"Structure: {self.file_structure.upper()}-FILE")
        print("=" * 80)

    def read_source(self):
        """Read source file with proper encoding"""
        try:
            with open(self.source_file, 'r', encoding='utf-8', errors='ignore') as f:
                return f.read()
        except:
            with open(self.source_file, 'r', encoding='latin-1', errors='ignore') as f:
                return f.read()

    def extract_functions(self):
        """Extract functions from all IBM i object types (RPG, AD, CL, DDS, SQL, Synon, etc.)"""
        functions = []
        source_str = str(self.source_file).upper()

        # RPG Programs (_RPG, RPGLE, SQLRPGLE) - handle fixed & free format
        if '_RPG' in source_str or 'BEGSR' in self.source_code.upper() or 'DCL-PROC' in self.source_code.upper():
            # Procedures: dcl-proc ProcedureName;
            for match in re.finditer(r'dcl-proc\s+(\w+)\s*;', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': func_name, 'line': line_num, 'full_name': func_name,
                    'category': self.auto_categorize(func_name), 'priority': self.auto_priority(func_name), 'type': 'Procedure'
                })

            # Free-format subroutines: Begsr SubroutineName;
            for match in re.finditer(r'Begsr\s+(\w+|\*PSSR)\s*;', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                if not any(f['name'] == func_name for f in functions):
                    line_num = self.source_code[:match.start()].count('\n') + 1
                    functions.append({
                        'name': func_name, 'line': line_num, 'full_name': func_name,
                        'category': self.auto_categorize(func_name), 'priority': self.auto_priority(func_name), 'type': 'Subroutine'
                    })

            # Fixed-format subroutines: CSR FunctionName BEGSR
            for match in re.finditer(r'CSR\s+(\w+)\s+BEGSR', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                if not any(f['name'] == func_name for f in functions):
                    line_num = self.source_code[:match.start()].count('\n') + 1
                    functions.append({
                        'name': func_name, 'line': line_num, 'full_name': func_name,
                        'category': self.auto_categorize(func_name), 'priority': self.auto_priority(func_name), 'type': 'Subroutine'
                    })

        # Action Diagrams (_AD.txt) - EXECUTE FUNCTION
        elif '_AD' in source_str or 'EXECUTE FUNCTION' in self.source_code:
            for match in re.finditer(r'EXECUTE\s+FUNCTION\(([^)]+)\)', self.source_code, re.IGNORECASE):
                full_name = match.group(1).strip()
                short_name = full_name.split()[0] if full_name else full_name
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': short_name, 'line': line_num, 'full_name': full_name,
                    'category': self.auto_categorize(short_name), 'priority': self.auto_priority(short_name), 'type': 'Function'
                })

        # CL Programs (_CL, CLLE, .CLP) - Subroutines and labels
        elif '_CL' in source_str or '.CL' in source_str or 'PGM' in self.source_code[:200]:
            # CL Subroutines: SUBR SUBRNAME
            for match in re.finditer(r'SUBR\s+SUBR\s+(\w+)', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': func_name, 'line': line_num, 'full_name': func_name,
                    'category': 'CL Subroutine', 'priority': 'Medium', 'type': 'CL Subroutine'
                })

        # DDS Files (_DDS, Physical, Logical, Display Files) - Record formats
        elif '_DDS' in source_str or 'DSPF' in source_str or re.search(r'^\s*A\s+R\s+\w+', self.source_code, re.MULTILINE):
            # Record formats: A R FORMATNAME
            for match in re.finditer(r'^\s*A\s+R\s+(\w+)', self.source_code, re.MULTILINE):
                format_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1

                # Determine format type
                format_type = 'Record Format'
                if 'SFL' in format_name.upper():
                    format_type = 'Subfile Format'
                elif 'CTL' in format_name.upper():
                    format_type = 'Control Format'
                elif 'HDR' in format_name.upper() or 'HEADER' in format_name.upper():
                    format_type = 'Header Format'
                elif 'FTR' in format_name.upper() or 'FOOTER' in format_name.upper():
                    format_type = 'Footer Format'

                functions.append({
                    'name': format_name, 'line': line_num, 'full_name': format_name,
                    'category': format_type, 'priority': 'High', 'type': 'DDS Format'
                })

        # SQL Programs or Procedures
        elif '_SQL' in source_str or 'CREATE PROCEDURE' in self.source_code.upper() or 'CREATE FUNCTION' in self.source_code.upper():
            # SQL Procedures
            for match in re.finditer(r'CREATE\s+PROCEDURE\s+(\w+)', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': func_name, 'line': line_num, 'full_name': func_name,
                    'category': 'SQL Procedure', 'priority': 'High', 'type': 'SQL Procedure'
                })

            # SQL Functions
            for match in re.finditer(r'CREATE\s+FUNCTION\s+(\w+)', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': func_name, 'line': line_num, 'full_name': func_name,
                    'category': 'SQL Function', 'priority': 'High', 'type': 'SQL Function'
                })

        # Synon/2E Generated Code - Action blocks
        elif 'SYNON' in source_str or '*ACTION' in self.source_code.upper() or '2E' in source_str:
            # Look for action blocks
            for match in re.finditer(r'\*ACTION\s+(\w+)', self.source_code, re.IGNORECASE):
                func_name = match.group(1)
                line_num = self.source_code[:match.start()].count('\n') + 1
                functions.append({
                    'name': func_name, 'line': line_num, 'full_name': func_name,
                    'category': 'Synon Action', 'priority': 'Medium', 'type': 'Synon Action'
                })

        # Content-based detection for unknown/unrecognized files
        if not functions:
            # Generic function pattern detection
            for match in re.finditer(r'(FUNCTION|PROCEDURE|SUBROUTINE|SUB|PROC)\s+(\w+)', self.source_code, re.IGNORECASE):
                func_name = match.group(2)
                if not any(f['name'] == func_name for f in functions):
                    line_num = self.source_code[:match.start()].count('\n') + 1
                    functions.append({
                        'name': func_name, 'line': line_num, 'full_name': func_name,
                        'category': self.auto_categorize(func_name), 'priority': self.auto_priority(func_name), 'type': 'Function'
                    })

        # Last resort: Create MAIN placeholder for files with no detectable functions
        if not functions:
            functions = [{
                'name': 'MAIN', 'line': 1, 'full_name': 'MAIN',
                'category': 'Core', 'priority': 'Critical', 'type': 'Program'
            }]

        return functions
    def find_line_number(self, func_name):
        """Find line number of function (fixed format)"""
        lines = self.source_code.split('\n')
        for i, line in enumerate(lines, 1):
            if func_name in line and 'BEGSR' in line.upper():
                return i
        return 0

    def find_line_number_free(self, func_name):
        """Find line number of function (free format)"""
        lines = self.source_code.split('\n')
        for i, line in enumerate(lines, 1):
            if 'BegSr' in line and func_name in line:
                return i
        return 0

    def auto_categorize(self, func_name):
        """Auto-categorize by naming pattern (for generic templates)"""
        name = func_name.upper()
        if 'INIT' in name or name.startswith('ZZ'): return 'Initialization'
        if 'EXIT' in name or name.startswith(('ZX', 'ZY')) or '*PSSR' in name: return 'Program Control'
        if any(x in name for x in ['DSP', 'EXF', 'KEY', 'DTL', 'DISPLAY', 'DISP']): return 'User Interface'
        if any(x in name for x in ['VLD', 'VAL', 'CHK', 'VALDAT', 'VALIDATE']): return 'Validation'
        if any(x in name for x in ['ADD', 'CRT', 'DLT', 'UPD', 'CHG']): return 'Database Operations'
        if any(x in name for x in ['RTV', 'GET', 'RV']): return 'Data Retrieval'
        if any(x in name for x in ['SET', 'TST', 'CLR', 'PMT', 'RESET', 'REFRESH', 'CLEAR']): return 'Screen Control'
        if any(x in name for x in ['MSG', 'SNM', 'HLP']): return 'Messaging'
        return 'Business Logic'

    def auto_priority(self, func_name):
        """Auto-assign priority (for generic templates)"""
        name = func_name.upper()
        if any(x in name for x in ['INIT', 'EXIT', 'MAIN', 'VLD', 'VALIDATE', '*PSSR']): return 'Critical'
        if any(x in name for x in ['PRO', 'ADD', 'UPD', 'CHG', 'DLT', 'RTV', 'GET', 'DISPLAY']): return 'High'
        if any(x in name for x in ['SET', 'CLR', 'TST', 'RESET', 'REFRESH', 'CLEAR']): return 'Low'
        return 'Medium'

    @staticmethod
    def validate_mermaid_syntax(html_content):
        """
        Validate Mermaid diagram syntax to prevent rendering errors
        
        Checks for common syntax errors that cause Mermaid diagrams to fail:
        1. = character in node labels (e.g., [Field = Value])
        2. Quotes in node labels (e.g., [Error: "Message"])
        3. Pipe character in node labels (e.g., [A | B])
        4. Equals in edge labels (e.g., -->|2=Edit|)
        
        Returns: (is_valid, error_list)
        """
        errors = []
        
        # Extract all Mermaid blocks
        mermaid_blocks = re.findall(r'<pre class="mermaid">(.*?)</pre>', html_content, re.DOTALL)
        
        for i, block in enumerate(mermaid_blocks, 1):
            # Skip subgraph lines (they can have quotes)
            lines = [l for l in block.split('\n') if 'subgraph' not in l]
            
            for line_num, line in enumerate(lines, 1):
                # Check for = in node labels []
                node_labels = re.findall(r'\[([^\]]+)\]', line)
                for label in node_labels:
                    if '=' in label and 'subgraph' not in line:
                        errors.append(f"Diagram {i}, Line {line_num}: Equals sign in node label: [{label}]")
                
                # Check for quotes in node labels
                if re.search(r'\[[^\]]*"[^\]]*\]', line) and 'subgraph' not in line:
                    errors.append(f"Diagram {i}, Line {line_num}: Quote in node label: {line.strip()}")
                
                # Check for pipe in node labels
                if re.search(r'\[[^\]]*\|[^\]]*\]', line) and 'subgraph' not in line:
                    errors.append(f"Diagram {i}, Line {line_num}: Pipe character in node label: {line.strip()}")
                
                # Check for = in edge labels |text|
                edge_labels = re.findall(r'\|([^|]+)\|', line)
                for label in edge_labels:
                    if '=' in label:
                        errors.append(f"Diagram {i}, Line {line_num}: Equals in edge label: |{label}|")
        
        return (len(errors) == 0, errors)

    def generate(self):
        """Generate complete documentation structure"""
        print(f"\nGenerating {self.file_structure}-file structure...")

        if self.file_structure == 'single':
            self.generate_single_file()
        else:
            self.generate_multi_file()

        print("\n[OK] Stage 1 complete: Generic HTML structure generated")
        print(">> Next: Claude will automatically fill AI markers with accurate content")

    def generate_multi_file(self):
        """Generate multi-file structure"""
        prog_folder = self.output_dir / self.program_name
        prog_folder.mkdir(parents=True, exist_ok=True)

        # Generate Sections 1-7
        print("  Generating Sections 1-7...")
        sections_17 = self.build_sections_1_7_html()
        with open(prog_folder / f"{self.program_name}_Sections_1-7.html", 'w', encoding='utf-8') as f:
            f.write(sections_17)

        # Generate Section 8
        print("  Generating Section 8...")
        section_8 = self.build_section_8_html()
        with open(prog_folder / f"{self.program_name}_Section_8_Functions.html", 'w', encoding='utf-8') as f:
            f.write(section_8)

        print(f"\n  Output: {prog_folder}/")
        print(f"    - {self.program_name}_Sections_1-7.html")
        print(f"    - {self.program_name}_Section_8_Functions.html")

    def build_sections_1_7_html(self):
        """Build complete Sections 1-7 HTML with generic content"""

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.program_name} - Business Documentation (Sections 1-7)</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
    <script>mermaid.initialize({{ startOnLoad: true, theme: 'default' }});</script>
    {self.get_css()}
</head>
<body>

<!-- AI_VALIDATION_MARKER: This file was generated by Stage 1 Python generator -->
<!-- AI_TODO: Review all sections for accuracy and enhance generic content -->

{self.build_header()}
{self.build_executive_summary()}
{self.build_toc()}
{self.build_section_1()}
{self.build_section_2()}
{self.build_section_3()}
{self.build_section_4()}
{self.build_section_5()}
{self.build_section_6()}
{self.build_section_7()}

<div style="text-align: center; margin: 50px 0;">
    <a href="{self.program_name}_Section_8_Functions.html" class="nav-link" style="font-size: 1.1em; padding: 15px 30px;">
        Continue to Section 8: Detailed Functions Analysis ({self.function_count} Functions) →
    </a>
</div>

</body>
</html>
"""
        return html

    def build_section_8_html(self):
        """Build Section 8 HTML with ALL functions (generic placeholders)"""

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.program_name} - Section 8: Business Functions</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
    <script>mermaid.initialize({{ startOnLoad: true, theme: 'default' }});</script>
    {self.get_css()}
</head>
<body>

<!-- AI_VALIDATION_MARKER: This file was generated by Stage 1 Python generator -->
<!-- AI_TODO: Analyze EACH function's source code and replace generic content with accurate analysis -->

<div class="header">
    <h1>{self.program_name} - Section 8</h1>
    <div class="subtitle">Detailed Business Functions Analysis</div>
    <div class="meta-info">
        <div class="meta-item"><strong>Total Functions</strong>{self.function_count}</div>
        <div class="meta-item"><strong>Documentation</strong>Generic - Needs AI Enhancement</div>
        <div class="meta-item"><strong>Generated</strong>{datetime.now().strftime('%B %d, %Y')}</div>
    </div>
</div>

<div style="text-align: center; margin: 30px 0;">
    <a href="{self.program_name}_Sections_1-7.html" class="nav-link">← Back to Sections 1-7</a>
</div>

<div class="section">
    <h2>8. Detailed Business Functions Analysis</h2>

    <div class="highlight">
        <strong>Stage 1 (Python):</strong> Generic structure generated.
        <strong>Stage 2 Required:</strong> AI validation needed to replace generic content with accurate, function-specific analysis.
    </div>
</div>

"""

        # Generate function documentation for ALL functions
        for idx, func in enumerate(self.functions, 1):
            html += self.build_function_template(idx, func)

        # Function catalog
        html += self.build_function_catalog()

        html += f"""
<div style="text-align: center; margin: 50px 0;">
    <a href="{self.program_name}_Sections_1-7.html" class="nav-link">← Back to Sections 1-7</a>
</div>

</body>
</html>
"""
        return html

    def build_function_template(self, idx, func):
        """Build generic function template with AI markers"""

        return f"""
<div class="function-analysis" id="func-{idx}">
    <!-- AI_ENHANCE_FUNCTION: {func['name']} at line {func['line']} -->
    <div class="function-header">
        <h3>F{idx:03d}: {func['name']} - {func['category']} Function</h3>
        <div style="margin-top: 10px;">
            <span style="background: {'#e74c3c' if func['priority'] == 'Critical' else '#f39c12' if func['priority'] == 'High' else '#3498db' if func['priority'] == 'Medium' else '#95a5a6'}; color: white; padding: 5px 12px; border-radius: 4px; font-size: 0.9em;">
                Priority: {func['priority']}
            </span>
            <span style="background: #ffc107; color: #000; padding: 5px 12px; border-radius: 4px; font-size: 0.9em; margin-left: 10px;">
                GENERIC - AI Enhancement Needed
            </span>
        </div>
    </div>

    <!-- AI_TODO: Read source code at line {func['line']} and analyze actual logic -->

    <h4>1. Parameters and Business Data Elements</h4>
    <!-- AI_PLACEHOLDER: Replace with actual parameters from source code -->
    <p>The {func['name']} function operates with business data elements (AI analysis required for specifics).</p>
    <ul>
        <li><strong>Input Parameters:</strong> [AI: Extract from actual source code]</li>
        <li><strong>Processing Context:</strong> [AI: Identify from code analysis]</li>
        <li><strong>Output Results:</strong> [AI: Determine from function logic]</li>
    </ul>

    <h4>2. Business Logic Summary</h4>
    <!-- AI_PLACEHOLDER: Replace with actual logic from source analysis -->
    <ul>
        <li><strong>Primary Business Workflow:</strong> [AI: Analyze actual code flow]</li>
        <li><strong>Key Decision Points:</strong> [AI: Identify IF/CASE/WHEN conditions]</li>
        <li><strong>Exception Management:</strong> [AI: Extract error handling logic]</li>
    </ul>

    <h4>3. Detailed Logic Explanation and Business Process Flow</h4>
    <!-- AI_PLACEHOLDER: Replace with actual logic narrative -->
    <p><strong>Generic Placeholder:</strong> This function performs {func['category'].lower()} operations. AI analysis required to document actual business logic.</p>

    <h4>Business Process Flow Diagram (Mermaid):</h4>
    <!-- AI_GENERATE_DIAGRAM: MUST create proper flowchart with decision diamonds based on source at line {func['line']} -->
    <!-- AI_CRITICAL: NO simple linear sequences allowed. MUST include: pre-check diamond, mid-process diamonds, post-verification diamond -->
    <!-- AI_PATTERN: Start → {{Pre-condition check?}} → Operation1 → {{Mid-process check?}} → Operation2 → {{Verification check?}} → Effect → End -->
    <div style="background: #fff3cd; padding: 20px; border-radius: 8px; margin: 20px 0; border: 2px dashed #ffc107;">
        <pre class="mermaid">
flowchart TD
    Start([{func['name']}<br/>Subroutine Entry]) --> PreCheck{{AI: Add pre-condition check<br/>e.g. Called from where?<br/>Buffer loaded? Error state?}}

    PreCheck -->|AI: Yes condition| Operation1[AI: First operation group<br/>Analyze actual code logic]

    Operation1 --> MidCheck{{AI: Add mid-process check<br/>e.g. Field present?<br/>Data valid? More to process?}}

    MidCheck -->|AI: Yes| Operation2[AI: Second operation group<br/>Map actual processing steps]
    MidCheck -->|AI: No/Skip| PostVerify

    Operation2 --> PostVerify{{AI: Add verification check<br/>e.g. All fields processed?<br/>Operation complete?}}

    PostVerify -->|AI: Yes| Effect[AI: Show effect/outcome<br/>e.g. Buffer ready<br/>Screen clean, Indicators reset]

    Effect --> End([Return to Caller<br/>AI: State after function])

    style Start fill:#d4edda,stroke:#28a745
    style End fill:#d1ecf1,stroke:#17a2b8
    style PreCheck fill:#fff3cd,stroke:#ffc107
    style MidCheck fill:#fff3cd,stroke:#ffc107
    style PostVerify fill:#fff3cd,stroke:#ffc107
    style Operation1 fill:#3498db,stroke:#2980b9,color:#fff
    style Operation2 fill:#e67e22,stroke:#d35400,color:#fff
    style Effect fill:#d4edda,stroke:#28a745
        </pre>
    </div>

    <h4>4. Data Interaction and Business Information Management</h4>
    <!-- AI_PLACEHOLDER: Replace with actual data operations -->
    <p>[AI: Analyze database operations, file I/O, and data transformations from source code]</p>
</div>
"""

    def build_function_catalog(self):
        """Build function catalog table"""
        html = """
<div class="section">
    <h2>Complete Function Catalog</h2>
    <p>All functions in this program (generic categorization - AI validation recommended):</p>

    <table>
        <thead>
            <tr>
                <th>ID</th>
                <th>Function</th>
                <th>Category</th>
                <th>Priority</th>
                <th>Line</th>
                <th>Status</th>
            </tr>
        </thead>
        <tbody>
"""

        for idx, func in enumerate(self.functions, 1):
            priority_class = f"priority-{func['priority'].lower()}"
            html += f"""            <tr>
                <td>F{idx:03d}</td>
                <td><a href="#func-{idx}">{func['name']}</a></td>
                <td>{func['category']}</td>
                <td class="{priority_class}">{func['priority']}</td>
                <td>Line {func['line']}</td>
                <td style="background: #fff3cd;">Generic - AI Enhancement Needed</td>
            </tr>
"""

        html += """        </tbody>
    </table>
</div>
"""
        return html

    def build_header(self):
        """Build HTML header section"""
        return f"""
<div class="header">
    <h1>{self.program_name}</h1>
    <div class="subtitle">IBM i Program Business Documentation</div>
    <div class="meta-info">
        <div class="meta-item"><strong>Object Type</strong>RPG Program</div>
        <div class="meta-item"><strong>Lines of Code</strong>{self.line_count:,}</div>
        <div class="meta-item"><strong>Functions</strong>{self.function_count}</div>
        <div class="meta-item"><strong>Generated</strong>{datetime.now().strftime('%B %d, %Y')}</div>
    </div>
</div>

<div class="alert-banner">
    STAGE 1 GENERIC STRUCTURE - AI Enhancement Required for Accurate Content
</div>
"""

    def build_executive_summary(self):
        """Build executive summary with generic content"""
        return f"""
<div class="executive-summary">
    <h2>Executive Summary</h2>
    <!-- AI_PLACEHOLDER: Replace with program-specific strategic overview -->
    <p><strong>Generic Overview:</strong> The {self.program_name} is an IBM i business application with {self.function_count} functions spanning {self.line_count:,} lines of code. AI analysis required for accurate business context.</p>

    <div class="summary-grid">
        <div class="summary-card">
            <h3>Business Value</h3>
            <!-- AI_TODO: Analyze business purpose from source -->
            <ul>
                <li>[AI: Extract actual business value from code analysis]</li>
            </ul>
        </div>

        <div class="summary-card">
            <h3>System Scale</h3>
            <ul>
                <li><strong>Functions:</strong> {self.function_count}</li>
                <li><strong>Code Lines:</strong> {self.line_count:,}</li>
                <li>[AI: Add integration points count]</li>
            </ul>
        </div>

        <div class="summary-card">
            <h3>Operational Impact</h3>
            <!-- AI_TODO: Determine from code analysis -->
            <ul>
                <li>[AI: Identify user community from file operations]</li>
                <li>[AI: Extract business processes from logic flow]</li>
            </ul>
        </div>
    </div>

    <h3>Key Performance Indicators</h3>
    <div class="kpi-grid">
        <div class="kpi-item"><div class="kpi-value">{self.line_count:,}</div><div class="kpi-label">Lines of Code</div></div>
        <div class="kpi-item"><div class="kpi-value">{self.function_count}</div><div class="kpi-label">Functions</div></div>
        <div class="kpi-item"><div class="kpi-value">GENERIC</div><div class="kpi-label">Content Type</div></div>
    </div>
</div>
"""

    def build_toc(self):
        """Build table of contents"""
        # Generate appropriate Section 8 link based on file structure
        if self.file_structure == 'single':
            # Single-file: use anchor link to #functions in same document
            section8_link = f'<li><a href="#functions">8. Detailed Business Functions Analysis ({self.function_count} Functions)</a></li>'
        else:
            # Multi-file: use external link to separate HTML file with clear labeling
            section8_link = f'<li><strong><a href="{self.program_name}_Section_8_Functions.html" class="nav-link" style="display:inline; padding:5px 10px;">8. Section 8: Detailed Business Functions Analysis ({self.function_count} Functions) - Click Here →</a></strong></li>'

        return f"""
<div class="toc">
    <h3>Table of Contents</h3>
    <ul>
        <li><a href="#context">1. Business Context and Overview</a></li>
        <li><a href="#inputs">2. Inputs (Primary and Optional)</a></li>
        <li><a href="#structure">3. Structure Overview and Components</a></li>
        <li><a href="#logic-summary">4. Business Logic Summary</a></li>
        <li><a href="#logic-flow">5. Detailed Logic Flow and Process Diagrams</a></li>
        <li><a href="#data-interaction">6. Data Operations</a></li>
        <li><a href="#dependencies">7. System Dependencies</a></li>
        {section8_link}
    </ul>
</div>
"""

    def build_section_1(self):
        """Section 1: Business Context (generic)"""
        return """
<div class="section" id="context">
    <h2>1. Business Context and Overview</h2>
    <!-- AI_VALIDATION_REQUIRED: Entire section needs source code analysis -->

    <div class="highlight">
        <strong>GENERIC PLACEHOLDER:</strong> AI analysis required to extract accurate business context from source code.
    </div>

    <h3>Executive Summary</h3>
    <!-- AI_TODO: Analyze file headers, comments, and overall program structure -->
    <p>[AI: Extract business purpose from program header comments and file operations]</p>

    <h3>Business Purpose</h3>
    <!-- AI_TODO: Determine from CALL statements and file I/O patterns -->
    <p>[AI: Analyze file operations to determine business function]</p>

    <h3>Key Business Functions</h3>
    <!-- AI_TODO: Group functions by business capability -->
    <p>[AI: Analyze function names and dependencies to identify key business processes]</p>
</div>
"""

    def build_section_2(self):
        """Section 2: Inputs (generic)"""
        return """
<div class="section" id="inputs">
    <h2>2. Inputs (Primary and Optional)</h2>
    <!-- AI_VALIDATION_REQUIRED: Extract actual inputs from F-spec and parameter lists -->

    <h3>2.1 Primary Inputs</h3>
    <!-- AI_TODO: Parse F-specs for database files -->
    <p>[AI: Extract from F-specifications in source code header]</p>

    <h3>2.2 Optional Inputs</h3>
    <!-- AI_TODO: Identify from parameter lists and conditional logic -->
    <p>[AI: Analyze parameter PLISTs and conditional field handling]</p>
</div>
"""

    def build_section_3(self):
        """Section 3: Structure with call tree"""
        categories = {}
        for func in self.functions:
            cat = func['category']
            categories[cat] = categories.get(cat, 0) + 1

        table_rows = ""
        for cat, count in sorted(categories.items()):
            table_rows += f"""            <tr>
                <td>{cat}</td>
                <td>{count}</td>
                <td>[AI: Analyze actual purpose]</td>
            </tr>
"""

        return f"""
<div class="section" id="structure">
    <h2>3. Structure Overview and Components</h2>
    <!-- AI_VALIDATION_REQUIRED: Validate component organization -->

    <h3>Component Organization</h3>
    <table>
        <thead>
            <tr><th>Category</th><th>Functions</th><th>Purpose</th></tr>
        </thead>
        <tbody>
{table_rows}        </tbody>
    </table>

    <h3>Program Call Tree Structure</h3>
    <!-- AI_GENERATE_DIAGRAM: Create accurate call tree by analyzing EXSR calls -->
    <div style="background: white; padding: 20px; border-radius: 8px; margin: 20px 0; border: 1px solid #e9ecef;">
        <pre class="mermaid">
flowchart TD
    Main[{self.program_name}<br/>Main Entry] --> Init[AI: Identify init functions]
    Main --> KeyScreen[AI: Identify key screen functions]
    KeyScreen --> DetailScreen[AI: Identify detail screen functions]
    DetailScreen --> Process[AI: Identify processing functions]
    Process --> Database[AI: Identify DB operations]

    style Main fill:#2e8b57,stroke:#1e5d3a,color:#fff
        </pre>
    </div>
    <p><em>AI Enhancement Required: Analyze EXSR calls to build accurate call tree</em></p>
</div>
"""

    def build_section_4(self):
        """Section 4: Business Logic"""
        return """
<div class="section" id="logic-summary">
    <h2>4. Business Logic Summary</h2>
    <!-- AI_VALIDATION_REQUIRED: Extract actual business workflow -->

    <h3>Primary Business Workflow</h3>
    <!-- AI_TODO: Trace execution flow from entry point through main logic -->
    <p>[AI: Analyze main program flow and key decision points]</p>
</div>
"""

    def build_section_5(self):
        """Section 5: Logic Flow with DETAILED explanation and diagram"""
        return """
<div class="section" id="logic-flow">
    <h2>5. Detailed Logic Flow and Process Diagrams</h2>
    <!-- AI_VALIDATION_REQUIRED: Create accurate process flow with DETAILED MULTI-PARAGRAPH EXPLANATION -->

    <h3>Main Program Flow Explanation</h3>
    <!-- AI_CRITICAL: This section MUST have detailed explanation BEFORE the diagram -->
    <!-- AI_MINIMUM: Opening paragraph + Key Processing Phases (5-7 items) + State Management section + THEN diagram -->

    <p><strong>[AI: Write opening paragraph explaining overall program flow pattern - describe DoU/DoW loop structure, main control flow, state management approach. 3-5 sentences.]</strong></p>

    <h4>Key Processing Phases:</h4>
    <p><strong>[AI: Identify 5-7 distinct phases from analyzing mainline code. Each phase needs 2-4 sentences explaining WHAT it does, WHEN it executes, WHY it's important.]</strong></p>

    <ol>
        <li><strong>[Phase 1 - Initialization]:</strong> [AI: Describe program startup, variable initialization, file opens, PSDS data capture. 2-3 sentences.]</li>
        <li><strong>[Phase 2 - Screen Refresh/Display]:</strong> [AI: Explain error-aware refresh logic, screen preparation, field clearing. 2-3 sentences.]</li>
        <li><strong>[Phase 3 - Function Key Handling]:</strong> [AI: Detail F3/F5/F12 key processing, exit logic, refresh logic. 2-3 sentences.]</li>
        <li><strong>[Phase 4 - Validation/Decision Logic]:</strong> [AI: Explain option validation, error checking, conditional routing. 2-3 sentences.]</li>
        <li><strong>[Phase 5 - Processing Workflows]:</strong> [AI: Describe main business operations (Display, Add, Update, etc.). 2-3 sentences.]</li>
        <li><strong>[Phase 6+]:</strong> [AI: Add more phases as needed from actual code - database operations, reporting, etc.]</li>
    </ol>

    <h4>State Management Design Pattern:</h4>
    <p><strong>[AI: Explain how program manages state across loop iterations - ERROR flags, indicator usage, loop control variables, how validation errors preserve vs. success clears fields. 3-5 sentences.]</strong></p>

    <h3>Business Process Flow Diagram</h3>
    <!-- AI_GENERATE_DIAGRAM: Create comprehensive mainline flowchart with ALL decision points -->
    <div style="background: white; padding: 20px; border-radius: 8px; margin: 20px 0; border: 1px solid #e9ecef;">
        <pre class="mermaid">
flowchart TD
    Start([Program Start]) --> Init[AI: Initialization logic]
    Init --> MainLoop{{AI: Main loop condition<br/>DoU/DoW check}}

    MainLoop -->|Continue| StateCheck{{AI: Error/State check<br/>Preserve or Refresh?}}
    StateCheck -->|Refresh| RefreshOp[AI: Refresh operation]
    StateCheck -->|Preserve| ShowScreen[AI: Display screen]

    RefreshOp --> ShowScreen
    ShowScreen --> KeyCheck{{AI: Function key check<br/>F3 F5 F12?}}

    KeyCheck -->|F3 Exit| End([Program End])
    KeyCheck -->|F5 Refresh| RefreshOp
    KeyCheck -->|Continue| ValidateOpt{{AI: Option validation<br/>Menu choice valid?}}

    ValidateOpt -->|Invalid| ErrorMsg[AI: Error message<br/>Set error indicators]
    ErrorMsg --> MainLoop

    ValidateOpt -->|Valid| OptionRoute{{AI: Route by option<br/>Which workflow?}}

    OptionRoute -->|Option 1| Workflow1[AI: First workflow<br/>e.g. Display/Inquiry]
    OptionRoute -->|Option 2| Workflow2[AI: Second workflow<br/>e.g. Add/Update]
    OptionRoute -->|Option 3+| Workflow3[AI: Additional workflows<br/>if present in code]

    Workflow1 --> MainLoop
    Workflow2 --> MainLoop
    Workflow3 --> MainLoop

    style Start fill:#d4edda,stroke:#28a745
    style End fill:#d1ecf1,stroke:#17a2b8
    style MainLoop fill:#fff3cd,stroke:#ffc107
    style StateCheck fill:#fff3cd,stroke:#ffc107
    style KeyCheck fill:#fff3cd,stroke:#ffc107
    style ValidateOpt fill:#fff3cd,stroke:#ffc107
    style OptionRoute fill:#fff3cd,stroke:#ffc107
    style Workflow1 fill:#3498db,stroke:#2980b9,color:#fff
    style Workflow2 fill:#e67e22,stroke:#d35400,color:#fff
        </pre>
    </div>
    <p><em>[AI: Add detailed explanation of the flow diagram - explain the decision points, branch logic, loop control, workflow routing, state management. 3-5 sentences showing how ERROR flags, indicators, and function keys control the flow.]</em></p>
</div>
"""

    def build_section_6(self):
        """Section 6: Data Operations"""
        return """
<div class="section" id="data-interaction">
    <h2>6. Data Operations and Business Information Flow</h2>
    <!-- AI_VALIDATION_REQUIRED: Extract actual database operations -->

    <h3>Database Access Patterns</h3>
    <!-- AI_TODO: Parse F-specs and identify CHAIN/READ/WRITE/UPDATE operations -->
    <p>[AI: Extract from file specifications and database operation statements]</p>
</div>
"""

    def build_section_7(self):
        """Section 7: Dependencies"""
        return """
<div class="section" id="dependencies">
    <h2>7. System Dependencies and Business Relationships</h2>
    <!-- AI_VALIDATION_REQUIRED: Extract CALL statements and external dependencies -->

    <h3>External Program Dependencies</h3>
    <!-- AI_TODO: Search for CALL statements in source code -->
    <p>[AI: Extract from CALL operations throughout the source]</p>

    <h3>Modernization Considerations</h3>
    <!-- AI_TODO: Analyze complexity and suggest migration approach -->
    <p>[AI: Assess modernization complexity based on code analysis]</p>
</div>
"""

    def get_css(self):
        """Professional CSS styles"""
        return """    <style>
body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
    background-color: #f8f9fa;
}

.header {
    background: linear-gradient(135deg, #0066cc, #007bff);
    color: white;
    padding: 2rem;
    margin: -20px -20px 30px -20px;
    border-radius: 0 0 15px 15px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
}

.header h1 { margin: 0; font-size: 2.5em; font-weight: 300; }
.header .subtitle { font-size: 1.2em; opacity: 0.9; margin-top: 10px; }

.meta-info {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 15px;
    margin-top: 20px;
}

.meta-item {
    background: rgba(255,255,255,0.2);
    padding: 10px 15px;
    border-radius: 8px;
}

.meta-item strong {
    display: block;
    font-size: 0.9em;
    opacity: 0.8;
}

.alert-banner {
    background: linear-gradient(45deg, #ffc107, #f39c12);
    color: #000;
    padding: 15px;
    margin: 20px 0;
    border-radius: 8px;
    text-align: center;
    font-weight: bold;
    box-shadow: 0 4px 15px rgba(255, 193, 7, 0.3);
}

.executive-summary {
    background: white;
    padding: 30px;
    border-radius: 10px;
    box-shadow: 0 5px 15px rgba(0,0,0,0.1);
    margin: 30px 0;
}

.summary-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 20px;
    margin: 20px 0;
}

.summary-card {
    background: white;
    padding: 25px;
    border-radius: 10px;
    box-shadow: 0 5px 15px rgba(0,0,0,0.1);
    border-left: 5px solid #0066cc;
}

.summary-card h3 { color: #2c3e50; margin-top: 0; }

.kpi-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 15px;
    margin: 20px 0;
}

.kpi-item {
    text-align: center;
    padding: 15px;
    background: #f8f9fa;
    border-radius: 8px;
}

.kpi-value {
    font-size: 2em;
    font-weight: bold;
    color: #0066cc;
}

.kpi-label {
    font-size: 0.9em;
    color: #666;
    margin-top: 5px;
}

.toc {
    background: white;
    padding: 25px;
    border-radius: 10px;
    box-shadow: 0 5px 15px rgba(0,0,0,0.1);
    margin: 30px 0;
}

.toc ul { list-style: none; padding: 0; }
.toc li { padding: 8px 0; border-bottom: 1px solid #ecf0f1; }
.toc a { color: #0066cc; text-decoration: none; font-weight: 500; }
.toc a:hover { color: #007bff; }

.section {
    background: white;
    margin: 30px 0;
    padding: 30px;
    border-radius: 10px;
    box-shadow: 0 5px 15px rgba(0,0,0,0.1);
}

.section h2 {
    color: #2c3e50;
    border-bottom: 3px solid #0066cc;
    padding-bottom: 10px;
    margin-bottom: 25px;
}

.section h3 { color: #34495e; margin-top: 25px; }

.function-analysis {
    background: #f8f9fa;
    border: 2px solid #ffc107;
    border-radius: 8px;
    padding: 20px;
    margin: 30px 0;
}

.function-header {
    background: #0066cc;
    color: white;
    padding: 20px;
    margin: -20px -20px 20px -20px;
    border-radius: 8px 8px 0 0;
}

.function-header h3 { margin: 0; color: white; }

table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
    background: white;
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 3px 10px rgba(0,0,0,0.1);
}

th, td {
    padding: 12px 15px;
    text-align: left;
    border-bottom: 1px solid #e9ecef;
}

th {
    background: #0066cc;
    color: white;
    font-weight: 600;
}

tr:hover { background-color: #f8f9fa; }

.nav-link {
    display: inline-block;
    background: #0066cc;
    color: white;
    padding: 12px 24px;
    border-radius: 8px;
    text-decoration: none;
    margin: 10px 5px;
    font-weight: 500;
    transition: all 0.3s;
}

.nav-link:hover {
    background: #007bff;
    transform: translateY(-2px);
    box-shadow: 0 5px 15px rgba(0,102,204,0.3);
}

.highlight {
    background: #fff3cd;
    padding: 15px;
    border-radius: 8px;
    border-left: 5px solid #ffc107;
    margin: 15px 0;
}

.priority-critical { color: #e74c3c; font-weight: bold; }
.priority-high { color: #f39c12; font-weight: bold; }
.priority-medium { color: #3498db; }
.priority-low { color: #95a5a6; }

@media (max-width: 768px) {
    .summary-grid, .meta-info, .kpi-grid { grid-template-columns: 1fr; }
    .header h1 { font-size: 2em; }
    body { padding: 10px; }
}
    </style>"""

    def fill_ai_markers(self, html_file):
        """
        Fill all remaining AI markers in HTML file with actual content from source code
        This is a BASIC cleanup that removes obvious placeholder text.

        NOTE: This does NOT perform actual source code analysis.
        For COMPLETE documentation with function-specific analysis:
        - Use Claude Code's /fill-docs command after stage1 generation
        - Claude will read source code and fill each function with accurate logic

        This method only:
        - Removes [AI: ...] marker text
        - Removes "Generic Placeholder" messages
        - Removes AI comment markers
        - Makes HTML viewable (not complete analysis)
        """
        print(f"\n{'='*80}")
        print("STAGE 2: Basic AI Marker Cleanup (NOT full analysis)")
        print(f"{'='*80}")
        print("NOTE: This removes placeholder text only.")
        print("For complete function analysis, use Claude's /fill-docs command")
        print(f"{'='*80}\n")

        # Read HTML file
        with open(html_file, 'r', encoding='utf-8') as f:
            html = f.read()

        # Count markers before
        ai_markers_before = html.count('[AI:')
        generic_before = html.count('Generic Placeholder')

        print(f"Found: {ai_markers_before} [AI:] markers, {generic_before} Generic Placeholders")
        print("Removing placeholder text...")

        # Replace all remaining AI markers and Generic Placeholders
        # Remove placeholder paragraphs
        html = re.sub(r'<p>The \w+ function operates with business data elements \(AI analysis required for specifics\)\.</p>', '', html)
        html = re.sub(r'<p><strong>Function performs</strong> This function performs \w+ operations\. AI analysis required to document actual business logic\.</p>', '<p>Review source code at function line number for detailed logic implementation.</p>', html)

        # Replace inline AI markers
        html = html.replace('[AI: Extract from actual source code]', 'Analyze source code at function line number')
        html = html.replace('[AI: Identify from code analysis]', 'Review function implementation')
        html = html.replace('[AI: Determine from function logic]', 'Extract from function body')
        html = html.replace('[AI: Analyze actual code flow]', 'Trace execution from function entry to exit')
        html = html.replace('[AI: Identify IF/CASE/WHEN conditions]', 'Review conditional logic branches')
        html = html.replace('[AI: Extract error handling logic]', 'Check error indicators and message sending')
        html = html.replace('Generic Placeholder:', 'Function performs')
        html = html.replace('[AI: Analyze database operations, file I/O, and data transformations from source code]', 'Review file operations, CHAIN/READ/WRITE statements, and data movements in source code')
        html = html.replace('AI analysis required to document actual business logic.', 'Refer to source code for implementation details.')

        # Remove leftover AI comment markers
        html = re.sub(r'<!-- AI_TODO:.*?-->', '', html)
        html = re.sub(r'<!-- AI_PLACEHOLDER:.*?-->', '', html)
        html = re.sub(r'<!-- AI_VALIDATION_REQUIRED:.*?-->', '', html)
        html = re.sub(r'<!-- AI_GENERATE_DIAGRAM:.*?-->', '', html)
        html = re.sub(r'<!-- AI_ENHANCE_FUNCTION:.*?-->', '', html)

        # Write back
        with open(html_file, 'w', encoding='utf-8') as f:
            f.write(html)

        # Count after
        ai_markers_after = html.count('[AI:')
        generic_after = html.count('Generic Placeholder')

        print(f"After: {ai_markers_after} [AI:] markers, {generic_after} Generic Placeholders")
        print(f"Removed: {ai_markers_before - ai_markers_after} AI markers, {generic_before - generic_after} placeholders")
        print(f"{'='*80}\n")

        return html

    def generate_single_file(self):
        """Generate single HTML file for ≤15 functions"""
        print("  Generating single-file documentation...")

        prog_folder = self.output_dir / self.program_name
        prog_folder.mkdir(parents=True, exist_ok=True)

        single_file = prog_folder / f"{self.program_name}_Documentation.html"

        # Build complete single-file HTML
        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.program_name} - Complete Business Documentation</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
    <script>mermaid.initialize({{ startOnLoad: true, theme: 'default' }});</script>
    {self.get_css()}
</head>
<body>

{self.build_header()}
{self.build_executive_summary()}
{self.build_toc()}
{self.build_section_1()}
{self.build_section_2()}
{self.build_section_3()}
{self.build_section_4()}
{self.build_section_5()}
{self.build_section_6()}
{self.build_section_7()}

<!-- SECTION 8: FUNCTIONS -->
<div class="section" id="functions">
    <h2>8. Detailed Business Functions Analysis</h2>
    <p>Complete 4-part analysis for all {self.function_count} functions with Mermaid diagrams.</p>
"""

        # Add all functions to single file
        for idx, func in enumerate(self.functions, 1):
            html += self.build_function_template(idx, func)

        # Add function catalog
        html += self.build_function_catalog()

        html += """
</div>

<footer style="text-align: center; padding: 20px; color: #666; border-top: 1px solid #ddd; margin-top: 40px;">
    <p>Complete IBM i Business Documentation - Generated with AI Marker Approach</p>
</footer>

</body>
</html>
"""

        with open(single_file, 'w', encoding='utf-8') as f:
            f.write(html)

        print(f"  Output: {single_file}")
        print(f"  [OK] Single file with {self.function_count} functions created")


def main():
    # Check for --fill-markers command
    if len(sys.argv) >= 2 and sys.argv[1] == '--fill-markers':
        if len(sys.argv) < 3:
            print("Usage: python stage1_generate_structure.py --fill-markers <html_file>")
            print("Example: python stage1_generate_structure.py --fill-markers Documentation/PBADXFR/PBADXFR_Documentation.html")
            sys.exit(1)

        html_file = Path(sys.argv[2])
        if not html_file.exists():
            print(f"Error: File not found: {html_file}")
            sys.exit(1)

        # Create a temporary generator just to use the fill_ai_markers method
        # We need a dummy source file
        dummy_gen = Stage1StructureGenerator.__new__(Stage1StructureGenerator)
        dummy_gen.fill_ai_markers(html_file)
        print(f"[OK] Completed filling markers in {html_file}")
        return

    if len(sys.argv) < 3:
        print("=" * 80)
        print("OPTIMIZED IBM i Documentation Generator - AI Marker Approach")
        print("=" * 80)
        print("\nUsage:")
        print("  python stage1_generate_structure.py <source_file> <program_name> [output_dir]")
        print("  python stage1_generate_structure.py --fill-markers <html_file>")
        print("\nExamples:")
        print('  python stage1_generate_structure.py "shipping_modules/OMEIPVR_RPG.txt" "OMEIPVR"')
        print('  python stage1_generate_structure.py "shipping_modules/HP4041S_AD.txt" "HP4041S"')
        print('  python stage1_generate_structure.py --fill-markers Documentation/PBADXFR/PBADXFR_Documentation.html')
        print("\nWhat Happens:")
        print("  1. Python extracts functions and creates professional HTML templates")
        print("  2. Templates include AI markers (<!-- AI_TODO -->, <!-- AI_PLACEHOLDER -->)")
        print("  3. Claude auto-detects JSON trigger and fills all markers")
        print("  4. Complete documentation with 80-90% token savings vs traditional approach")
        print("\nToken Efficiency:")
        print("  - Traditional: Read 1500-line prompt + analyze = 60K-180K tokens")
        print("  - Optimized: Read source + fill markers = 8K-20K tokens")
        print("  - Savings: 75-90% reduction in token usage")
        print("=" * 80)
        sys.exit(1)

    source_file = sys.argv[1]
    program_name = sys.argv[2]
    output_dir = sys.argv[3] if len(sys.argv) > 3 else "Documentation"

    generator = Stage1StructureGenerator(source_file, program_name, output_dir)
    generator.generate()

    # Count AI markers for efficiency tracking
    import json
    section_1_7_markers = 7  # One per section
    section_8_markers = generator.function_count * 4  # 4 parts per function
    total_markers = section_1_7_markers + section_8_markers

    # Calculate token efficiency
    traditional_tokens = generator.function_count * 1000 + 60000
    optimized_tokens = generator.function_count * 400 + 8000
    savings_pct = int((1 - (optimized_tokens / traditional_tokens)) * 100)

    # Output JSON trigger for Claude AUTO-PROCESSING
    trigger_data = {
        "action": "FILL_AI_MARKERS",
        "program_name": program_name,
        "file_type": "RPG Program" if "_RPG" in source_file else "Action Diagram",
        "source_file": source_file,
        "line_count": generator.line_count,
        "function_count": generator.function_count,
        "output_files": {
            "sections_1_7": str(generator.output_dir / program_name / f"{program_name}_Sections_1-7.html"),
            "section_8": str(generator.output_dir / program_name / f"{program_name}_Section_8_Functions.html")
        },
        "total_markers": total_markers,
        "marker_breakdown": {
            "sections_1_7": section_1_7_markers,
            "section_8_functions": section_8_markers
        },
        "functions": [
            {"name": f['name'], "line": f['line'], "category": f['category'], "priority": f['priority']}
            for f in generator.functions
        ],
        "ai_instructions": {
            "workflow": "MARKER_FILL_ONLY",
            "read_source_code": source_file,
            "read_universal_prompt": False,
            "find_markers": "Search for <!-- AI_TODO -->, <!-- AI_PLACEHOLDER -->, <!-- AI_VALIDATION_REQUIRED -->",
            "fill_each_marker": "Replace placeholder text with analyzed content from source",

            "SECTION_5_REQUIREMENTS": {
                "CRITICAL": "Section 5 MUST have detailed multi-paragraph explanation BEFORE the diagram",
                "minimum_content": [
                    "Opening paragraph: Describe overall program flow pattern (loop structure, state management, etc.)",
                    "Key Processing Phases: Number and explain 5-7 distinct phases (initialization, screen refresh, menu display, function key handling, validation, processing workflows, etc.)",
                    "Each phase needs 2-4 sentences explaining WHAT it does, WHEN it executes, WHY it matters",
                    "State Management/Design Pattern section: Explain error flags, indicators, loop control variables",
                    "THEN show the diagram with detailed explanation after"
                ],
                "BAD_EXAMPLE": "Brief one-line explanation before diagram",
                "GOOD_EXAMPLE": "Multi-paragraph explanation with <h4>Key Processing Phases:</h4> and numbered/bulleted phases, then <h4>State Management Design Pattern:</h4>, THEN diagram"
            },

            "MERMAID_FLOWCHART_REQUIREMENTS": {
                "CRITICAL": "EVERY function MUST have proper Mermaid flowchart with decision diamonds - NO EXCEPTIONS",
                "NEVER_ALLOWED": [
                    "Simple linear sequences without decision diamonds (Start → Step1 → Step2 → End)",
                    "Horizontal flows without conditional logic (flowchart LR with just boxes)",
                    "Sequential lists of operations without branches"
                ],
                "REQUIRED_ELEMENTS": [
                    "Decision diamonds {}: Show pre-conditions, state checks, field validation",
                    "Conditional branches: Use -->|Yes| and -->|No| or -->|Condition| labels",
                    "Business context: Show WHY function is called, WHAT triggers it",
                    "Verification checks: Show post-conditions, validation of completion",
                    "Effect/outcome nodes: Show WHAT changes after function executes"
                ],
                "EVEN_SIMPLE_FUNCTIONS": "Even utility functions with no IF statements in code must show: calling context decision → operation → verification check → effect",
                "EXAMPLES": {
                    "BAD_Simple_Utility": "Start → Clear Field1 → Clear Field2 → Clear Field3 → End (NO DECISION DIAMONDS)",
                    "GOOD_Simple_Utility": "Start → {Called after success or F5?} → Clear Names → {Address needs clearing?} → Clear Address → {All fields blank?} → Screen Ready → End",
                    "BAD_Data_Transfer": "Start → Copy Field1 → Copy Field2 → ... → Copy Field12 → End",
                    "GOOD_Data_Transfer": "Start → {Record buffer loaded?} → Copy ID → Copy Names → {Address present?} → Copy Address → {Contact available?} → Copy Contact → {All transferred?} → Ready → End"
                },
                "MINIMUM_DIAMONDS": "At least 3 decision diamonds per flowchart (pre-check, mid-process check, post-verification)",
                "USE_COLORS": "Apply different colors with style statements for visual variety"
            },

            "mermaid_syntax_rules": {
                "CRITICAL": "Follow these rules to prevent syntax errors",
                "no_equals_in_labels": "NEVER use = inside node labels []. Use from, to, or : instead. BAD: [Field = Value], GOOD: [Field from Value]",
                "no_quotes_in_labels": "NEVER use quotes inside node labels []. BAD: [Error: \"Message\"], GOOD: [Error: Message]",
                "no_pipes_in_labels": "NEVER use | inside node labels []. Use comma instead. BAD: [A | B], GOOD: [A, B]",
                "edge_labels_safe": "Edge labels |text| should not contain = or quotes. Use short descriptive text only",
                "examples": {
                    "WRONG": "[SUPID = blank], [Error: \"Not found\"], [A | B], {Field = Value?}",
                    "CORRECT": "[SUPID blank], [Error: Not found], [A, B], {Field Value?}"
                }
            },

            "validation_checklist": [
                "BEFORE saving any Mermaid diagram, verify:",
                "1. No = character in any node label []",
                "2. No quotes in any node label []",
                "3. No | pipe character in any node label []",
                "4. Edge labels |text| contain no = or quotes",
                "5. All brackets balanced: [], {}, ()",
                "6. At least 3 decision diamonds {} present",
                "7. Conditional branches with labels (-->|Yes|, -->|No|)",
                "8. NOT a simple linear sequence",
                "9. Section 5 has multi-paragraph explanation BEFORE diagram"
            ],

            "keep_concise": "2-4 lines per marker in Section 8 function 4-part analysis (NOT Section 5 - Section 5 needs detailed explanation)",
            "token_savings": f"{savings_pct}% vs traditional approach"
        },
        "efficiency_metrics": {
            "traditional_tokens": traditional_tokens,
            "optimized_tokens": optimized_tokens,
            "savings_percent": savings_pct,
            "traditional_cost_usd": round(traditional_tokens * 0.003 / 1000, 2),
            "optimized_cost_usd": round(optimized_tokens * 0.003 / 1000, 2)
        }
    }

    print("\n" + "=" * 80)
    print("AI_AUTO_TRIGGER_JSON_START")
    print(json.dumps(trigger_data, indent=2))
    print("AI_AUTO_TRIGGER_JSON_END")
    print("=" * 80)

    print(f"\nEfficiency Summary:")
    print(f"  AI Markers: {total_markers} fill points")
    print(f"  Traditional Tokens: ~{traditional_tokens:,} (${trigger_data['efficiency_metrics']['traditional_cost_usd']})")
    print(f"  Optimized Tokens: ~{optimized_tokens:,} (${trigger_data['efficiency_metrics']['optimized_cost_usd']})")
    print(f"  Token Savings: {savings_pct}%")
    print("\n" + "=" * 80)
    print("Claude will now automatically fill all AI markers")
    print("=" * 80)


if __name__ == "__main__":
    main()
