/**
 * AI-Powered Search Interface for Legacy-Modern Code Mappings
 * 
 * This tool enables AI assistants to search and query the documentation
 * using natural language queries.
 */

import * as fs from 'fs';
import * as path from 'path';

interface SearchResult {
  relevanceScore: number;
  mapping: any;
  matchedFields: string[];
}

export class AISearchInterface {
  private documentationData: any;

  constructor(jsonFilePath: string) {
    const content = fs.readFileSync(jsonFilePath, 'utf-8');
    this.documentationData = JSON.parse(content);
  }

  /**
   * Natural language search across all mappings
   */
  public search(query: string): SearchResult[] {
    const queryLower = query.toLowerCase();
    const queryWords = queryLower.split(/\s+/).filter(w => w.length > 2);
    
    const results: SearchResult[] = [];

    this.documentationData.searchableChunks.forEach((chunk: any) => {
      const searchText = chunk.searchText.toLowerCase();
      const keywords = chunk.keywords.map((k: string) => k.toLowerCase());
      
      let score = 0;
      const matchedFields: string[] = [];

      // Exact phrase match (highest score)
      if (searchText.includes(queryLower)) {
        score += 10;
        matchedFields.push('exact-phrase');
      }

      // Individual word matches
      queryWords.forEach(word => {
        // Count occurrences in search text
        const occurrences = (searchText.match(new RegExp(word, 'g')) || []).length;
        score += occurrences * 2;

        // Keyword match
        if (keywords.includes(word)) {
          score += 3;
          matchedFields.push(`keyword:${word}`);
        }

        // Legacy file name match
        if (chunk.data.legacySource.toLowerCase().includes(word)) {
          score += 5;
          matchedFields.push('legacy-source');
        }

        // Modern file name match
        if (chunk.data.modernFile.toLowerCase().includes(word)) {
          score += 4;
          matchedFields.push('modern-file');
        }

        // Category match
        if (chunk.category.toLowerCase().includes(word)) {
          score += 3;
          matchedFields.push('category');
        }
      });

      if (score > 0) {
        results.push({
          relevanceScore: score,
          mapping: chunk.data,
          matchedFields
        });
      }
    });

    // Sort by relevance score
    return results.sort((a, b) => b.relevanceScore - a.relevanceScore);
  }

  /**
   * Find all modern implementations of a legacy file
   */
  public findModernImplementations(legacyFileName: string): any[] {
    const index = this.documentationData.indexes.byLegacyFile;
    return index[legacyFileName] || [];
  }

  /**
   * Find all legacy sources for a modern file
   */
  public findLegacySources(modernFileName: string): any[] {
    const index = this.documentationData.indexes.byModernFile;
    return index[modernFileName] || [];
  }

  /**
   * Get mappings by category
   */
  public findByCategory(category: 'Controller' | 'Service' | 'Repository' | 'Model'): any[] {
    const index = this.documentationData.indexes.byCategory;
    return index[category] || [];
  }

  /**
   * Find business logic flow for a specific operation
   */
  public findBusinessLogicFlow(searchTerm: string): any[] {
    return this.documentationData.searchableChunks
      .filter((chunk: any) => 
        chunk.data.businessLogicFlow && 
        chunk.data.businessLogicFlow.some((step: string) => 
          step.toLowerCase().includes(searchTerm.toLowerCase())
        )
      )
      .map((chunk: any) => chunk.data);
  }

  /**
   * Export search results to markdown
   */
  public exportSearchResults(results: SearchResult[], outputPath: string): void {
    let markdown = `# Search Results\n\n`;
    markdown += `**Total Results:** ${results.length}\n\n`;
    markdown += `---\n\n`;

    results.forEach((result, index) => {
      markdown += `## ${index + 1}. ${result.mapping.modernFile} - ${result.mapping.modernLocation}\n\n`;
      markdown += `**Relevance Score:** ${result.relevanceScore}\n`;
      markdown += `**Matched Fields:** ${result.matchedFields.join(', ')}\n\n`;
      markdown += `- **Legacy Source:** \`${result.mapping.legacySource}\`\n`;
      markdown += `- **Description:** ${result.mapping.description}\n`;
      markdown += `- **Category:** ${result.mapping.category}\n\n`;

      if (result.mapping.businessLogicFlow) {
        markdown += `**Business Logic Flow:**\n`;
        result.mapping.businessLogicFlow.forEach((step: string) => {
          markdown += `- ${step}\n`;
        });
        markdown += `\n`;
      }

      markdown += `**Code:**\n\`\`\`typescript\n${result.mapping.codeSnippet}\n\`\`\`\n\n`;
      markdown += `---\n\n`;
    });

    fs.writeFileSync(outputPath, markdown, 'utf-8');
    console.log(`✅ Search results exported to: ${outputPath}`);
  }
}

// CLI Usage
if (require.main === module) {
  const args = process.argv.slice(2);
  
  if (args.length < 2) {
    console.log('Usage: tsx ai-search-interface.ts <json-file> <search-query>');
    console.log('Example: tsx ai-search-interface.ts documentation.json "customer validation"');
    process.exit(1);
  }

  const jsonFile = args[0];
  const query = args.slice(1).join(' ');

  console.log(`🔍 Searching: "${query}"\n`);

  const search = new AISearchInterface(jsonFile);
  const results = search.search(query);

  console.log(`📊 Found ${results.length} results:\n`);

  results.slice(0, 5).forEach((result, index) => {
    console.log(`${index + 1}. ${result.mapping.modernFile} → ${result.mapping.modernLocation}`);
    console.log(`   Score: ${result.relevanceScore}`);
    console.log(`   Legacy: ${result.mapping.legacySource}`);
    console.log(`   Description: ${result.mapping.description}`);
    console.log('');
  });

  if (results.length > 5) {
    console.log(`...and ${results.length - 5} more results\n`);
  }

  // Export top 10 results
  const outputPath = path.join(process.cwd(), 'search_results.md');
  search.exportSearchResults(results.slice(0, 10), outputPath);
}
