# 📚 JSDoc Documentation Generator & AI Search System

## Overview

This system extracts JSDoc annotations from your TypeScript codebase and generates multiple documentation formats:

1. **JSON Format** - Machine-readable data for processing
2. **Markdown Format** - Human-readable developer documentation  
3. **AI-Searchable Format** - Optimized for AI tool consumption with embeddings-ready structure

## 🎯 Key Features

✅ **Automatic Extraction** - Scans all TypeScript files for `@legacySource` annotations  
✅ **Multi-Format Output** - JSON, Markdown, and AI-optimized formats  
✅ **Searchable Indexes** - By legacy file, modern file, and category  
✅ **Business Logic Flow** - Extracts numbered step documentation  
✅ **AI-Powered Search** - Natural language queries across all mappings  
✅ **Code Snippet Inclusion** - Captures relevant code for context

---

## 🚀 Quick Start

### 1. Installation

```bash
cd Documentation_Generators
npm install
```

### 2. Extract Documentation from Your Project

```bash
# Extract from Enterprise Platform Conversion
npm run extract:enterprise

# Extract from Seaboard POC
npm run extract:seaboard

# Or specify custom directory
tsx jsdoc-extractor.ts <source-directory> <project-name>
```

**Example:**
```bash
tsx jsdoc-extractor.ts ../Enterprise_Platform_Conversion/src "Enterprise Platform System"
```

### 3. Search the Documentation

```bash
# Search for specific topics
tsx ai-search-interface.ts ./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json "customer validation"

# Search for legacy file implementations
tsx ai-search-interface.ts ./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json "custmntr.txt"

# Search by category
tsx ai-search-interface.ts ./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json "Service create method"
```

---

## 📦 Output Files

After running the extractor, you'll get three files in `Documentation_Outputs/`:

### 1. JSON Format (`*_mappings_*.json`)
**Purpose:** Machine-readable data for programmatic access

**Structure:**
```json
{
  "projectName": "Enterprise Platform System",
  "generatedDate": "2025-10-29T...",
  "totalMappings": 150,
  "mappings": [...],
  "legacyFileIndex": { "custmntr.txt": [...] },
  "modernFileIndex": { "CustomerService.ts": [...] },
  "categoryIndex": { "Service": [...] }
}
```

### 2. Markdown Format (`*_documentation_*.md`)
**Purpose:** Human-readable developer documentation

**Contains:**
- Table of contents
- Mappings organized by category
- Mappings organized by legacy file
- Mappings organized by modern file
- Complete mapping list with code snippets

**Great for:**
- Onboarding new developers
- Code review reference
- Migration tracking

### 3. AI-Searchable Format (`*_ai_searchable_*.json`)
**Purpose:** Optimized for AI tools and embedding systems

**Structure:**
```json
{
  "metadata": { "projectName": "...", "version": "1.0" },
  "searchableChunks": [
    {
      "id": "mapping-0",
      "type": "legacy-modern-mapping",
      "category": "Service",
      "searchText": "Modern File: CustomerService.ts...",
      "data": { /* Full mapping data */ },
      "keywords": ["CustomerService", "custmntr.txt", "validation", ...]
    }
  ],
  "indexes": { /* Pre-built lookup indexes */ }
}
```

---

## 🔍 Using the AI Search Interface

### Example 1: Find Customer Validation Logic

```bash
tsx ai-search-interface.ts ./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json "customer validation"
```

**Output:**
```
🔍 Searching: "customer validation"

📊 Found 8 results:

1. CustomerService.ts → method createCustomer
   Score: 25
   Legacy: custmntr.txt:170-212 (AddCustomer subroutine)
   Description: Create new customer with comprehensive validation

2. CustomerModel.ts → method validateCreate
   Score: 18
   Legacy: CUSTMST.txt:15-80 (DDS field definitions)
   Description: Customer data validation matching DDS specifications

...and 6 more results
```

### Example 2: Find All Modern Implementations of a Legacy File

```typescript
import { AISearchInterface } from './ai-search-interface';

const search = new AISearchInterface('./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json');

// Find all modern code that references custmntr.txt
const modernImpl = search.findModernImplementations('custmntr.txt');

console.log(`Found ${modernImpl.length} modern implementations`);
modernImpl.forEach(impl => {
  console.log(`- ${impl.modernFile} → ${impl.modernLocation}`);
});
```

### Example 3: Find Business Logic Flow

```typescript
const search = new AISearchInterface('./documentation.json');

// Find all methods with date validation in business logic
const dateValidations = search.findBusinessLogicFlow('validate date');

dateValidations.forEach(mapping => {
  console.log(`\n${mapping.modernFile} → ${mapping.modernLocation}`);
  console.log('Business Logic Flow:');
  mapping.businessLogicFlow.forEach(step => console.log(`  ${step}`));
});
```

---

## 🎓 For AI Assistants / LLMs

### How to Use This Documentation

1. **Load the AI-searchable JSON file:**
   ```typescript
   const docs = JSON.parse(fs.readFileSync('Enterprise_Platform_System_ai_searchable_2025-10-29.json', 'utf-8'));
   ```

2. **Search for relevant mappings:**
   - Use `searchableChunks` array for full-text search
   - Use `indexes.byLegacyFile` to find modern implementations
   - Use `indexes.byModernFile` to find legacy sources
   - Use `indexes.byCategory` to filter by file type

3. **Extract information:**
   ```typescript
   // Find all Service methods
   const serviceMethods = docs.indexes.byCategory.Service;
   
   // Find what replaced custmntr.txt
   const modernCustomer = docs.indexes.byLegacyFile['custmntr.txt'];
   
   // Find legacy sources for CustomerService.ts
   const legacySources = docs.indexes.byModernFile['CustomerService.ts'];
   ```

### Embedding-Ready Structure

Each `searchableChunk` has a `searchText` field optimized for creating embeddings:

```json
{
  "searchText": "Modern File: CustomerService.ts\nModern Location: method createCustomer\nLegacy Source: custmntr.txt:170-212 (AddCustomer subroutine)\nDescription: Create new customer with comprehensive validation\nBusiness Logic: 1. Validate required fields (ID, Name, Date) - line 175 2. Trim customer ID..."
}
```

You can:
1. Generate embeddings from `searchText`
2. Store in vector database
3. Perform semantic search
4. Find similar code patterns

---

## 📊 Example Use Cases

### 1. Migration Impact Analysis
**Question:** "What modern code will be affected if I change the customer validation logic in custmntr.txt line 180?"

**Solution:**
```bash
tsx ai-search-interface.ts documentation.json "custmntr.txt 180"
```

### 2. Developer Onboarding
**Question:** "Where is the order creation logic now?"

**Solution:**
```bash
tsx ai-search-interface.ts documentation.json "order creation"
```

### 3. Code Review
**Question:** "Show me all Service methods that use the date validation pattern"

**Solution:**
```typescript
const search = new AISearchInterface('./documentation.json');
const services = search.findByCategory('Service');
const dateValidators = services.filter(s => 
  s.businessLogicFlow?.some(step => step.includes('date'))
);
```

### 4. Legacy Code Understanding
**Question:** "What does the CHAIN operation on line 322 of custmntr.txt do now?"

**Solution:**
```bash
tsx ai-search-interface.ts documentation.json "custmntr.txt 322 CHAIN"
```

---

## 🔧 Advanced Usage

### Custom Extraction

```typescript
import { JSDocExtractor } from './jsdoc-extractor';

const extractor = new JSDocExtractor();

// Extract from specific files
extractor.extractFromFile('./src/services/CustomerService.ts');
extractor.extractFromFile('./src/models/CustomerModel.ts');

// Generate documentation
const docs = extractor.generateDocumentation('My Project');

// Export to custom location
extractor.exportToJSON('./my-docs/output.json', 'My Project');
extractor.exportToMarkdown('./my-docs/README.md', 'My Project');
```

### Programmatic Search

```typescript
import { AISearchInterface } from './ai-search-interface';

const search = new AISearchInterface('./documentation.json');

// Natural language search
const results = search.search('customer validation with date checking');

// Filter by score threshold
const highRelevance = results.filter(r => r.relevanceScore > 10);

// Export filtered results
search.exportSearchResults(highRelevance, './filtered_results.md');
```

---

## 📈 Metrics & Statistics

After extraction, the console will show:

```
✅ JSON documentation exported to: ./Documentation_Outputs/Enterprise_Platform_System_mappings_2025-10-29.json
📊 Total mappings: 150

✅ Markdown documentation exported to: ./Documentation_Outputs/Enterprise_Platform_System_documentation_2025-10-29.md

✅ AI-searchable format exported to: ./Documentation_Outputs/Enterprise_Platform_System_ai_searchable_2025-10-29.json

✅ All documentation formats generated successfully!
```

The generated documentation includes:
- Total number of legacy-to-modern mappings
- Number of legacy files referenced
- Number of modern files with annotations
- Breakdown by category (Controller, Service, Repository, Model)

---

## 🎯 Integration with AI Tools

### Vector Database Integration

```python
# Example: Load into Pinecone/Weaviate
import json
from sentence_transformers import SentenceTransformer

# Load AI-searchable format
with open('Enterprise_Platform_System_ai_searchable_2025-10-29.json') as f:
    data = json.load(f)

# Generate embeddings
model = SentenceTransformer('all-MiniLM-L6-v2')

for chunk in data['searchableChunks']:
    embedding = model.encode(chunk['searchText'])
    
    # Store in vector DB
    # vector_db.upsert(
    #     id=chunk['id'],
    #     vector=embedding,
    #     metadata=chunk['data']
    # )
```

### RAG (Retrieval Augmented Generation)

```typescript
// Example: Use with LangChain
import { AISearchInterface } from './ai-search-interface';

async function answerQuestionWithContext(question: string) {
  const search = new AISearchInterface('./documentation.json');
  const results = search.search(question);
  
  // Get top 5 results as context
  const context = results.slice(0, 5).map(r => ({
    modernFile: r.mapping.modernFile,
    legacySource: r.mapping.legacySource,
    description: r.mapping.description,
    businessLogic: r.mapping.businessLogicFlow,
    code: r.mapping.codeSnippet
  }));
  
  // Send to LLM with context
  const prompt = `
    Question: ${question}
    
    Relevant Code Context:
    ${JSON.stringify(context, null, 2)}
    
    Answer based on the provided context:
  `;
  
  // Call LLM API...
}
```

---

## 🛠️ Troubleshooting

### No mappings found
**Issue:** Extractor returns 0 mappings

**Solution:**
- Ensure your TypeScript files have JSDoc comments with `@legacySource` tags
- Check file paths are correct
- Verify the source directory contains `.ts` or `.tsx` files

### Search returns no results
**Issue:** AI search finds 0 results for valid query

**Solution:**
- Check the JSON file path is correct
- Verify the query matches content in `searchText` fields
- Try broader search terms (e.g., "customer" instead of "CustomerService.ts")

---

## 📝 Output Examples

### Example JSON Mapping
```json
{
  "modernFile": "CustomerService.ts",
  "modernLocation": "method createCustomer",
  "legacySource": "custmntr.txt:170-212 (AddCustomer subroutine)",
  "description": "Create new customer with comprehensive validation",
  "businessLogicFlow": [
    "1. Validate required fields (ID, Name, Date) - line 175",
    "2. Trim customer ID (custmntr.txt:180) - %trim(CUSTID)",
    "3. Check for duplicate ID via CHAIN operation (custmntr.txt:185-190)"
  ],
  "codeSnippet": "async createCustomer(data: ICustomerCreate): Promise<ICustomerRecord> {\n  ...\n}",
  "category": "Service",
  "lineNumber": 42
}
```

### Example Markdown Output (Excerpt)
```markdown
## By Category

### Service (45 mappings)

- **CustomerService.ts** → `method createCustomer`
  - Legacy: `custmntr.txt:170-212 (AddCustomer subroutine)`
  - Create new customer with comprehensive validation

- **ItemService.ts** → `method createItem`
  - Legacy: `mastpgm.txt:150-180 (AddItem subroutine)`
  - Create new item with validation
```

---

## 🚀 Future Enhancements

Planned features:
- [ ] Support for other languages (Java, Python, C#)
- [ ] Visual dependency graph generation
- [ ] Integration with GitHub Actions for auto-documentation
- [ ] Web-based search interface
- [ ] Export to Confluence/SharePoint
- [ ] Diff tracking between documentation versions
- [ ] AI-powered similarity detection

---

## 📄 License

MIT License - See LICENSE file for details

---

## 🤝 Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request

---

## 📧 Support

For questions or issues, please open a GitHub issue or contact the development team.

---

**Generated by JSDoc Documentation Generator v1.0.0**
