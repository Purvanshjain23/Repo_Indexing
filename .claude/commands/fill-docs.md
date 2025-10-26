---
description: Auto-fill IBM i documentation markers after stage1_generate_structure.py runs
---

You just detected AI_AUTO_TRIGGER_JSON in the output. This means stage1_generate_structure.py has generated HTML templates with AI markers that need to be filled.

**DO NOT create extra MD files. DO NOT generate summaries. Just fill the HTML files.**

**CRITICAL RULES - NO SHORTCUTS ALLOWED:**

1. **NEVER create Python scripts** to fill markers with templates
2. **NEVER use generic/template flowcharts**
3. **ALWAYS read actual source code** for each function at its line number
4. **EVERY Mermaid diagram must be unique** and reflect actual code logic

**INSTRUCTIONS:**

1. **Read the source file** specified in the JSON trigger (in sections if too large)
2. **Read the HTML file** (in sections if too large)
3. **For EACH function**, you MUST:
   - **Read the actual source code** starting at the function's line number (read 50-150 lines)
   - **Analyze the REAL logic**: IF statements, EXSR calls, CALL statements, READ/WRITE operations, DOWEQ loops, CHAIN operations, GOTOs
   - **Identify actual decision points**: What conditions does the code check?
   - **Map the actual flow**: What's the real sequence of operations?
   - **Create a UNIQUE Mermaid flowchart** showing the actual logic flow from the source code

4. **Fill ALL AI markers directly in the HTML file** using Edit tool:
   - Replace all `[AI: Extract...]` with actual content from source
   - Replace all `<!-- AI_PLACEHOLDER -->` sections with real analysis
   - Replace all `<!-- AI_TODO -->` sections with actual data
   - Replace ALL generic Mermaid diagrams with REAL flowcharts based on actual source code
   - Replace "Generic Placeholder" text with actual logic explanations
   - Replace "GENERIC - AI Enhancement Needed" badges with "COMPLETE"

5. **Mermaid Flowchart Requirements:**
   - Show actual IF conditions from the code (e.g., "YAFSCH Not Zero?", "IN92=0?")
   - Show actual operations (e.g., "CHAIN #SFLRCD", "READC", "EXSR DCPRSR")
   - Show actual field names and values from the source
   - Show actual error paths and GOTOs
   - NO generic nodes like "Initialize Variables", "Process Record", "Validate Condition 1"
   - Each flowchart must be visually different because each function has different logic

6. **Update banners**:
   - Change "STAGE 1 GENERIC STRUCTURE" to "COMPLETE DOCUMENTATION"
   - Change "Generic - Needs AI Enhancement" to "Complete Analysis"

7. **NO extra files** - Only edit the HTML files that Python generated

8. **When complete**, verify:
   - `grep -c "Generic Placeholder"` = 0
   - `grep -c "\[AI:"` = 0
   - `grep -c "Initialize Variables"` = 0
   - Each flowchart has unique logic (no two flowcharts should look the same)
   - HTML files ready to open in browser

**Work Process:**
- Work function by function (F01, F02, F03, ...)
- For each function: Read source → Analyze logic → Create unique flowchart → Edit HTML
- Use Edit tool directly on HTML - NO scripts
- If file is too large, read/edit in sections but ALWAYS read actual source code first
