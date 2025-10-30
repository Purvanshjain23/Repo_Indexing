/**
 * JSDoc Annotation Extractor
 * Extracts JSDoc comments from TypeScript files and creates searchable documentation
 * 
 * Features:
 * - Extracts all JSDoc comments with @legacySource tags
 * - Generates JSON output for AI tool consumption
 * - Creates markdown documentation for developers
 * - Builds searchable index of legacy-to-modern mappings
 */

import * as fs from 'fs';
import * as path from 'path';

interface LegacySourceMapping {
  modernFile: string;
  modernLocation: string; // Class/Method/Function name
  legacySource: string;   // e.g., "custmntr.txt:170-212 (AddCustomer subroutine)"
  description: string;
  businessLogicFlow?: string[]; // Numbered steps
  codeSnippet: string;
  category: 'Controller' | 'Service' | 'Repository' | 'Model' | 'Unknown';
  lineNumber: number;
}

interface DocumentationOutput {
  projectName: string;
  generatedDate: string;
  totalMappings: number;
  mappings: LegacySourceMapping[];
  legacyFileIndex: { [legacyFile: string]: LegacySourceMapping[] };
  modernFileIndex: { [modernFile: string]: LegacySourceMapping[] };
  categoryIndex: { [category: string]: LegacySourceMapping[] };
}

export class JSDocExtractor {
  private mappings: LegacySourceMapping[] = [];

  /**
   * Extract JSDoc comments from a TypeScript file
   */
  public extractFromFile(filePath: string): void {
    const content = fs.readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');
    
    let currentJSDoc: string[] = [];
    let insideJSDoc = false;
    let jsdocStartLine = 0;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Detect JSDoc start
      if (line.trim().startsWith('/**')) {
        insideJSDoc = true;
        currentJSDoc = [line];
        jsdocStartLine = i + 1;
        continue;
      }

      // Collect JSDoc lines
      if (insideJSDoc) {
        currentJSDoc.push(line);
        
        // Detect JSDoc end
        if (line.trim().includes('*/')) {
          insideJSDoc = false;
          
          // Process the JSDoc block
          this.processJSDocBlock(currentJSDoc, filePath, jsdocStartLine, lines, i);
          currentJSDoc = [];
        }
      }
    }
  }

  /**
   * Process a single JSDoc block and extract legacy source information
   */
  private processJSDocBlock(
    jsdocLines: string[],
    filePath: string,
    startLine: number,
    allLines: string[],
    endLine: number
  ): void {
    const jsdocText = jsdocLines.join('\n');
    
    // Check if this JSDoc contains @legacySource tag
    if (!jsdocText.includes('@legacySource')) {
      return;
    }

    // Extract @legacySource tags
    const legacySourceMatches = jsdocText.matchAll(/@legacySource\s+([^\n]+)/g);
    const legacySources = Array.from(legacySourceMatches).map(m => m[1].trim());

    if (legacySources.length === 0) {
      return;
    }

    // Extract description
    const descMatch = jsdocText.match(/@description\s+([^\n]+)/);
    const description = descMatch ? descMatch[1].trim() : '';

    // Extract Business Logic Flow
    const businessLogicFlow = this.extractBusinessLogicFlow(jsdocText);

    // Find the code element this JSDoc is documenting
    const modernLocation = this.findModernLocation(allLines, endLine);

    // Extract code snippet (next 20 lines or until next JSDoc/empty line)
    const codeSnippet = this.extractCodeSnippet(allLines, endLine + 1);

    // Determine category based on file path
    const category = this.determineCategory(filePath);

    // Create mapping for each legacy source reference
    legacySources.forEach(legacySource => {
      this.mappings.push({
        modernFile: path.basename(filePath),
        modernLocation,
        legacySource,
        description,
        businessLogicFlow,
        codeSnippet,
        category,
        lineNumber: startLine
      });
    });
  }

  /**
   * Extract Business Logic Flow from JSDoc
   */
  private extractBusinessLogicFlow(jsdocText: string): string[] | undefined {
    const flowMatch = jsdocText.match(/Business Logic Flow[^:]*:([^@]*)/s);
    if (!flowMatch) {
      return undefined;
    }

    const flowText = flowMatch[1];
    const steps = flowText
      .split('\n')
      .map(line => line.trim())
      .filter(line => /^\d+\./.test(line)) // Lines starting with "1.", "2.", etc.
      .map(line => line.replace(/^\*\s*/, '')); // Remove leading asterisks

    return steps.length > 0 ? steps : undefined;
  }

  /**
   * Find the modern code location (class, method, function name)
   */
  private findModernLocation(lines: string[], startIndex: number): string {
    for (let i = startIndex + 1; i < Math.min(startIndex + 5, lines.length); i++) {
      const line = lines[i].trim();
      
      // Class declaration
      const classMatch = line.match(/export\s+class\s+(\w+)/);
      if (classMatch) {
        return `class ${classMatch[1]}`;
      }

      // Method/Function declaration
      const methodMatch = line.match(/(?:async\s+)?(?:public\s+|private\s+|static\s+)*(\w+)\s*\(/);
      if (methodMatch) {
        return `method ${methodMatch[1]}`;
      }

      // Arrow function
      const arrowMatch = line.match(/(?:export\s+)?(?:const|let|var)\s+(\w+)\s*=/);
      if (arrowMatch) {
        return `function ${arrowMatch[1]}`;
      }
    }

    return 'Unknown';
  }

  /**
   * Extract code snippet following the JSDoc
   */
  private extractCodeSnippet(lines: string[], startIndex: number): string {
    const snippetLines: string[] = [];
    let braceCount = 0;
    let foundStart = false;

    for (let i = startIndex; i < Math.min(startIndex + 30, lines.length); i++) {
      const line = lines[i];
      
      // Skip empty lines before code starts
      if (!foundStart && line.trim() === '') {
        continue;
      }

      foundStart = true;
      snippetLines.push(line);

      // Count braces to find end of code block
      braceCount += (line.match(/{/g) || []).length;
      braceCount -= (line.match(/}/g) || []).length;

      // Stop at end of function/method (when braces balance and we have content)
      if (braceCount === 0 && snippetLines.length > 3) {
        break;
      }

      // Also stop at next JSDoc or after 20 lines
      if (line.trim().startsWith('/**') || snippetLines.length >= 20) {
        break;
      }
    }

    return snippetLines.join('\n');
  }

  /**
   * Determine category based on file path
   */
  private determineCategory(filePath: string): LegacySourceMapping['category'] {
    const fileName = path.basename(filePath).toLowerCase();
    
    if (fileName.includes('controller')) return 'Controller';
    if (fileName.includes('service')) return 'Service';
    if (fileName.includes('repository')) return 'Repository';
    if (fileName.includes('model')) return 'Model';
    
    return 'Unknown';
  }

  /**
   * Process entire directory recursively
   */
  public extractFromDirectory(dirPath: string): void {
    const files = fs.readdirSync(dirPath);

    files.forEach(file => {
      const fullPath = path.join(dirPath, file);
      const stat = fs.statSync(fullPath);

      if (stat.isDirectory()) {
        // Skip node_modules, dist, etc.
        if (!['node_modules', 'dist', 'build', '.git'].includes(file)) {
          this.extractFromDirectory(fullPath);
        }
      } else if (file.endsWith('.ts') || file.endsWith('.tsx')) {
        console.log(`Processing: ${fullPath}`);
        this.extractFromFile(fullPath);
      }
    });
  }

  /**
   * Generate documentation output
   */
  public generateDocumentation(projectName: string): DocumentationOutput {
    // Build indexes
    const legacyFileIndex: { [key: string]: LegacySourceMapping[] } = {};
    const modernFileIndex: { [key: string]: LegacySourceMapping[] } = {};
    const categoryIndex: { [key: string]: LegacySourceMapping[] } = {};

    this.mappings.forEach(mapping => {
      // Index by legacy file
      const legacyFile = mapping.legacySource.split(':')[0];
      if (!legacyFileIndex[legacyFile]) {
        legacyFileIndex[legacyFile] = [];
      }
      legacyFileIndex[legacyFile].push(mapping);

      // Index by modern file
      if (!modernFileIndex[mapping.modernFile]) {
        modernFileIndex[mapping.modernFile] = [];
      }
      modernFileIndex[mapping.modernFile].push(mapping);

      // Index by category
      if (!categoryIndex[mapping.category]) {
        categoryIndex[mapping.category] = [];
      }
      categoryIndex[mapping.category].push(mapping);
    });

    return {
      projectName,
      generatedDate: new Date().toISOString(),
      totalMappings: this.mappings.length,
      mappings: this.mappings,
      legacyFileIndex,
      modernFileIndex,
      categoryIndex
    };
  }

  /**
   * Export to JSON (for AI tools)
   */
  public exportToJSON(outputPath: string, projectName: string): void {
    const documentation = this.generateDocumentation(projectName);
    fs.writeFileSync(
      outputPath,
      JSON.stringify(documentation, null, 2),
      'utf-8'
    );
    console.log(`✅ JSON documentation exported to: ${outputPath}`);
    console.log(`📊 Total mappings: ${documentation.totalMappings}`);
  }

  /**
   * Export to Markdown (for developers)
   */
  public exportToMarkdown(outputPath: string, projectName: string): void {
    const documentation = this.generateDocumentation(projectName);
    let markdown = `# ${projectName} - Legacy to Modern Code Mapping\n\n`;
    markdown += `**Generated:** ${new Date().toLocaleDateString()}\n`;
    markdown += `**Total Mappings:** ${documentation.totalMappings}\n\n`;
    markdown += `---\n\n`;

    // Table of Contents
    markdown += `## 📚 Table of Contents\n\n`;
    markdown += `- [By Category](#by-category)\n`;
    markdown += `- [By Legacy File](#by-legacy-file)\n`;
    markdown += `- [By Modern File](#by-modern-file)\n`;
    markdown += `- [Complete Mapping List](#complete-mapping-list)\n\n`;
    markdown += `---\n\n`;

    // By Category
    markdown += `## By Category\n\n`;
    Object.keys(documentation.categoryIndex).sort().forEach(category => {
      const mappings = documentation.categoryIndex[category];
      markdown += `### ${category} (${mappings.length} mappings)\n\n`;
      
      mappings.slice(0, 10).forEach(mapping => {
        markdown += `- **${mapping.modernFile}** → \`${mapping.modernLocation}\`\n`;
        markdown += `  - Legacy: \`${mapping.legacySource}\`\n`;
        if (mapping.description) {
          markdown += `  - ${mapping.description}\n`;
        }
      });
      
      if (mappings.length > 10) {
        markdown += `\n_...and ${mappings.length - 10} more mappings_\n`;
      }
      markdown += `\n`;
    });

    // By Legacy File
    markdown += `---\n\n## By Legacy File\n\n`;
    Object.keys(documentation.legacyFileIndex).sort().forEach(legacyFile => {
      const mappings = documentation.legacyFileIndex[legacyFile];
      markdown += `### ${legacyFile} (${mappings.length} references)\n\n`;
      
      mappings.forEach(mapping => {
        markdown += `- **${mapping.modernFile}** → \`${mapping.modernLocation}\`\n`;
        markdown += `  - \`${mapping.legacySource}\`\n`;
      });
      markdown += `\n`;
    });

    // By Modern File
    markdown += `---\n\n## By Modern File\n\n`;
    Object.keys(documentation.modernFileIndex).sort().forEach(modernFile => {
      const mappings = documentation.modernFileIndex[modernFile];
      markdown += `### ${modernFile} (${mappings.length} legacy references)\n\n`;
      
      mappings.forEach(mapping => {
        markdown += `#### ${mapping.modernLocation}\n\n`;
        markdown += `- **Legacy Source:** \`${mapping.legacySource}\`\n`;
        if (mapping.description) {
          markdown += `- **Description:** ${mapping.description}\n`;
        }
        
        if (mapping.businessLogicFlow && mapping.businessLogicFlow.length > 0) {
          markdown += `- **Business Logic Flow:**\n`;
          mapping.businessLogicFlow.forEach(step => {
            markdown += `  - ${step}\n`;
          });
        }
        
        markdown += `\n**Code:**\n\`\`\`typescript\n${mapping.codeSnippet}\n\`\`\`\n\n`;
      });
    });

    // Complete Mapping List
    markdown += `---\n\n## Complete Mapping List\n\n`;
    documentation.mappings.forEach((mapping, index) => {
      markdown += `### ${index + 1}. ${mapping.modernFile} - ${mapping.modernLocation}\n\n`;
      markdown += `- **Category:** ${mapping.category}\n`;
      markdown += `- **Legacy Source:** \`${mapping.legacySource}\`\n`;
      markdown += `- **Description:** ${mapping.description || 'N/A'}\n`;
      markdown += `- **Line Number:** ${mapping.lineNumber}\n`;
      
      if (mapping.businessLogicFlow && mapping.businessLogicFlow.length > 0) {
        markdown += `\n**Business Logic Flow:**\n`;
        mapping.businessLogicFlow.forEach(step => {
          markdown += `${step}\n`;
        });
      }
      
      markdown += `\n**Code Snippet:**\n\`\`\`typescript\n${mapping.codeSnippet}\n\`\`\`\n\n`;
      markdown += `---\n\n`;
    });

    fs.writeFileSync(outputPath, markdown, 'utf-8');
    console.log(`✅ Markdown documentation exported to: ${outputPath}`);
  }

  /**
   * Export to AI-searchable format (enhanced JSON with embeddings-ready structure)
   */
  public exportToAIFormat(outputPath: string, projectName: string): void {
    const documentation = this.generateDocumentation(projectName);
    
    // Create AI-friendly format with searchable chunks
    const aiFormat = {
      metadata: {
        projectName,
        generatedDate: documentation.generatedDate,
        totalMappings: documentation.totalMappings,
        version: '1.0'
      },
      searchableChunks: documentation.mappings.map((mapping, index) => ({
        id: `mapping-${index}`,
        type: 'legacy-modern-mapping',
        category: mapping.category,
        
        // Searchable text (for AI embedding)
        searchText: `
          Modern File: ${mapping.modernFile}
          Modern Location: ${mapping.modernLocation}
          Legacy Source: ${mapping.legacySource}
          Description: ${mapping.description}
          ${mapping.businessLogicFlow ? 'Business Logic: ' + mapping.businessLogicFlow.join(' ') : ''}
        `.trim(),
        
        // Structured data
        data: mapping,
        
        // Keywords for traditional search
        keywords: [
          mapping.modernFile,
          mapping.modernLocation,
          mapping.legacySource.split(':')[0], // Legacy file name
          mapping.category,
          ...mapping.description.split(' ').filter(w => w.length > 3)
        ]
      })),
      
      // Pre-built indexes for fast lookups
      indexes: {
        byLegacyFile: documentation.legacyFileIndex,
        byModernFile: documentation.modernFileIndex,
        byCategory: documentation.categoryIndex
      }
    };

    fs.writeFileSync(
      outputPath,
      JSON.stringify(aiFormat, null, 2),
      'utf-8'
    );
    console.log(`✅ AI-searchable format exported to: ${outputPath}`);
  }
}

// CLI Usage
if (require.main === module) {
  const args = process.argv.slice(2);
  
  if (args.length < 2) {
    console.log('Usage: tsx jsdoc-extractor.ts <source-directory> <output-name>');
    console.log('Example: tsx jsdoc-extractor.ts ./src "Enterprise Platform System"');
    process.exit(1);
  }

  const sourceDir = args[0];
  const projectName = args[1];

  console.log(`🔍 Extracting JSDoc annotations from: ${sourceDir}`);
  console.log(`📦 Project: ${projectName}\n`);

  const extractor = new JSDocExtractor();
  extractor.extractFromDirectory(sourceDir);

  const outputDir = path.join(process.cwd(), 'Documentation_Outputs');
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  const timestamp = new Date().toISOString().split('T')[0];
  
  // Export to all formats
  extractor.exportToJSON(
    path.join(outputDir, `${projectName.replace(/\s+/g, '_')}_mappings_${timestamp}.json`),
    projectName
  );
  
  extractor.exportToMarkdown(
    path.join(outputDir, `${projectName.replace(/\s+/g, '_')}_documentation_${timestamp}.md`),
    projectName
  );
  
  extractor.exportToAIFormat(
    path.join(outputDir, `${projectName.replace(/\s+/g, '_')}_ai_searchable_${timestamp}.json`),
    projectName
  );

  console.log('\n✅ All documentation formats generated successfully!');
}
