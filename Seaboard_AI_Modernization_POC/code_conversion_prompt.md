# 🎯 Universal IBM i to Full-Stack TypeScript/React Modernization Prompt

## ⚠️ CRITICAL TRACEABILITY REQUIREMENTS - READ FIRST ⚠️

**MANDATORY LEGACY TRACEABILITY FOR ALL GENERATED CODE:**

**❌ UNACCEPTABLE OUTPUT:**
- Controllers without file-level JSDoc or RPG command key mappings
- Services without Business Logic Flow documentation
- Repository methods without RPG operation comments (CHAIN, WRITE, UPDATE, DELETE)
- Models without DDS line references on every Joi field
- ANY code missing legacy source line numbers

**✅ ACCEPTABLE OUTPUT - EVERY FILE MUST HAVE:**

1. **Controllers**: 
   - File-level JSDoc with `@legacySource {file}:1-50 (Command Key Processing)`
   - RPG command key mappings (F3, F6, F9, F11, F12, F15, Enter)
   - Method-level JSDoc for every endpoint

2. **Services**:
   - JSDoc with `@legacySource {file}:{lines} ({subroutine})`
   - Numbered Business Logic Flow (1. Step - line X, 2. Step - line Y, etc.)
   - Inline comments with line numbers: `// Line 180: Trim ID (%trim(FIELD))`

3. **Repositories**:
   - JSDoc with RPG operation: `@legacySource {file}:{lines} ({subroutine})`
   - Inline RPG mapping: `// Line 322: CHAIN {FILE} (Primary key lookup)`
   - %found indicator comments: `// Matches RPG *INLr indicator (line 325)`

4. **Models**:
   - Class-level JSDoc: `@legacySource {DDS_file}.txt:15-80 (DDS field definitions)`
   - Every Joi field: `// Line 20: {FIELD} - 7A (7 characters, alphanumeric)`
   - Validation methods with numbered Business Logic Flow

**🛑 ENFORCEMENT RULE:**
**IF you generate ANY file without the above traceability, STOP immediately and regenerate with proper legacy source references. NO EXCEPTIONS.**

Use CustomerService.ts as the gold standard pattern - EVERY module must have same detail level.

## 📚 COMPREHENSIVE TRACEABILITY PATTERNS - DETAILED EXAMPLES

See the complete traceability patterns in the **Enterprise Platform Conversion** documentation for detailed examples of:
- Controller file-level and method-level JSDoc patterns
- Service Business Logic Flow documentation with line references
- Repository RPG operation mapping patterns (CHAIN, WRITE, UPDATE, DELETE, SETLL/READ)
- Model DDS field line references in Joi schemas
- Validation method documentation with numbered Business Logic Flow
- PROGRAM_METADATA usage pattern (Services use, Models receive as parameter)

**Reference File:** `Enterprise_Platform_Conversion/Legacy Source Files/Conversion_prompt.md`
**Section:** "COMPREHENSIVE TRACEABILITY PATTERNS (NEW - MANDATORY)"

These patterns ensure complete traceability from modern TypeScript code back to legacy IBM i source files.

---

## Quick Setup - Just Paste and Go!
1. Replace `{PROGRAM_NAME}` with your program (e.g., PBGREFR, CUSTMAST, INVCTL)
2. Replace `{program_name_lowercase}` with lowercase version (e.g., pbgrefr, custmast, invctl)
3. Attach your 3 files: RPG, DDS, AD
4. Paste this prompt in any LLM → Get complete production-ready application!

## 📋 INPUT FILES - FLEXIBLE ATTACHMENT SYSTEM (Attach Available Files)
**INTELLIGENT FILE DETECTION**: The system will automatically detect file types and adapt modernization accordingly.

### 🎯 **STANDARD FILES (Attach if Available):**
1. **{PROGRAM_NAME}_RPG.txt** ✅ **[REQUIRED]** - RPG source code with business logic
2. **{PROGRAM_NAME}_DDS.txt** ⚡ **[OPTIONAL]** - Display/Data Description Specs for UI/database structure  
3. **{PROGRAM_NAME}_AD.txt** ⚡ **[OPTIONAL]** - Action Diagram with program flow and validations
4. **{PROGRAM_NAME}_CL.txt** ⚡ **[OPTIONAL]** - Control Language for job control and batch processing

### 🔍 **UNKNOWN FILES (Attach Any Additional Files):**
5. **{PROGRAM_NAME}_UNKNOWN1.txt** 🔍 **[AUTO-DETECT]** - Any additional file (system will analyze content)
6. **{PROGRAM_NAME}_UNKNOWN2.txt** 🔍 **[AUTO-DETECT]** - Any additional file (system will analyze content)
7. **{PROGRAM_NAME}_UNKNOWN3.txt** 🔍 **[AUTO-DETECT]** - Any additional file (system will analyze content)

### 📋 **MODERNIZATION ADAPTS BASED ON AVAILABLE FILES:**
- **RPG Only**: Backend API with database operations (no frontend)
- **RPG + DDS**: Full-stack application with React frontend
- **RPG + AD**: Backend with enhanced validation and business logic
- **RPG + CL**: Backend with background jobs and batch processing
- **All Files**: Complete enterprise application with all features

## Complete Prompt Template

```bash
# IBM i Legacy System Modernization - {PROGRAM_NAME} Module Conversion

**CONTEXT**: I'm converting {PROGRAM_NAME} module from IBM i AS/400 to modern full-stack TypeScript/React application. 
I have attached the RPG source, DDS specifications, and Action Diagram files.

**GOAL**: Generate a complete, production-ready application that I can immediately run with `npm install && npm run dev:fullstack:mock`

Convert {PROGRAM_NAME} module from RPG/AD/DDS to TypeScript/Node.js following enterprise patterns.

## 📦 ADAPTIVE FILE GENERATION (Based on Available Input Files)

### **🎯 CORE BACKEND FILES (Always Generated if RPG exists):**

1. src/interfaces/{PROGRAM_NAME}.ts - TypeScript interfaces (from RPG data structures)
2. src/models/{PROGRAM_NAME}Model.ts - Data models with Joi validation  
3. src/repository/{PROGRAM_NAME}Repository.ts - PostgreSQL CRUD operations
4. src/services/{PROGRAM_NAME}Service.ts - Business logic from RPG subroutines
5. src/controllers/{PROGRAM_NAME}Controller.ts - REST API endpoints
6. src/routes/{PROGRAM_NAME}Routes.ts - Express routing
7. src/index.ts - Main server with database mode switching

### **🎯 INFRASTRUCTURE FILES (Always Generated):**
8. src/_shared/database/transaction.ts - Transaction wrapper utility
9. src/_shared/constants/metadata.ts - Program metadata constants  
10. src/_shared/utils/validation.ts - Reusable validation functions
11. src/_shared/utils/stringUtils.ts - String manipulation utilities

### **🎯 MOCK DATABASE SYSTEM (Always Generated):**
12. src/_shared/local/{PROGRAM_NAME}MockDB.ts - In-memory mock database
13. src/database/connection.ts - PostgreSQL connection pool

### **⚡ REACT FRONTEND (Generated ONLY if DDS file is provided):**
10. src/ui/{PROGRAM_NAME}View.tsx - React component replacing green-screen
11. src/ui/main.tsx - React application entry point
12. src/ui/index.html - HTML template
13. src/ui/styles.css - Component styling

### **⚡ ADVANCED VALIDATION (Generated ONLY if AD file is provided):**
14. src/middleware/{PROGRAM_NAME}Validator.ts - Enhanced validation from Action Diagram
15. src/utils/{PROGRAM_NAME}BusinessRules.ts - Complex business logic validation

### **⚡ BACKGROUND JOBS (Generated ONLY if CL file is provided):**
16. src/jobs/{PROGRAM_NAME}Job.ts - Background job processing
17. src/services/{PROGRAM_NAME}PrintService.ts - Print/PDF generation
18. src/queues/{PROGRAM_NAME}Queue.ts - Queue management

### **⚡ AUTO-DETECTED FILES (Generated based on unknown file analysis):**
19. src/sql/{PROGRAM_NAME}Schema.sql - Database schema (if SQL detected)
20. src/copybooks/{PROGRAM_NAME}Types.ts - Shared type definitions (if copybook detected)

### **🎯 CONFIGURATION & BUILD (Always Generated):**
21. vite.config.ts - Vite configuration (only if DDS exists for React development)
22. .env - Environment configuration for database switching
23. package.json - Updated with appropriate dependencies

## 🤖 INTELLIGENT FILE ANALYSIS (Execute First)

### **STEP 1: ANALYZE ALL ATTACHED FILES**
Before generating any code, perform this analysis:

```typescript
// File Analysis Template - Execute this logic first
const fileAnalysis = {
  rpgFile: null,      // Always required - contains business logic
  ddsFile: null,      // Optional - determines if frontend is generated  
  adFile: null,       // Optional - determines validation complexity
  clFile: null,       // Optional - determines if background jobs are needed
  unknownFiles: []    // Auto-detect content and classify
};

// For each unknown file, analyze content and classify:
function analyzeUnknownFile(content: string): FileType {
  if (content.includes('DCL VAR(') || content.includes('PGM PARM(')) return 'CL';
  if (content.includes('INDTXT(') || content.includes('SFLCTL') || content.includes('DSPATR(')) return 'DDS';
  if (content.includes('EXECUTE FUNCTION(') || content.includes('CASE;') || content.includes('//?')) return 'AD';
  if (content.includes('     H/TITLE') || content.includes('FCAVLLSL0IF') || content.includes('     D ')) return 'RPG';
  if (content.includes('CREATE TABLE') || content.includes('SELECT') || content.includes('INSERT')) return 'SQL';
  if (content.includes('COPY ') || content.includes('/INCLUDE') || content.includes('     D*')) return 'COPYBOOK';
  return 'UNKNOWN';
}
```

### **STEP 2: DETERMINE MODERNIZATION SCOPE**
```typescript
const modernizationScope = {
  generateBackend: fileAnalysis.rpgFile !== null,           // Always true if RPG exists
  generateFrontend: fileAnalysis.ddsFile !== null,         // Only if DDS found
  generateAdvancedValidation: fileAnalysis.adFile !== null, // Only if AD found  
  generateBackgroundJobs: fileAnalysis.clFile !== null,    // Only if CL found
  generateDatabase: true,                                   // Always generate
  generateMockSystem: true                                  // Always generate
};
```

## CRITICAL REQUIREMENTS (Must Follow Exactly):

### **LEGACY BUSINESS RULE PRESERVATION** (Enterprise-Level Traceability)

**MANDATORY - Legacy Source Traceability**: Every service method MUST include:
1. **JSDoc with legacy source reference**: `@legacySource {filename}:{line_start}-{line_end} ({subroutine_name})`
2. **Numbered business logic flow**: Document exact sequence from legacy
3. **Inline comments with line numbers**: Reference specific legacy lines for each operation
4. **Exact error messages**: Match legacy error messages precisely (including punctuation)

**⚠️ CRITICAL ENFORCEMENT RULES**:
- **EVERY Controller function** → MUST have file-level JSDoc + RPG command key mappings + method-level JSDoc
- **EVERY Service method** → MUST have comprehensive JSDoc with Business Logic Flow + inline line number comments
- **EVERY Repository method** → MUST have JSDoc with RPG operation mapping + inline RPG file operation comments
- **EVERY Model field in Joi schema** → MUST have inline DDS line reference comment
- **EVERY Model validation method** → MUST have JSDoc with Business Logic Flow
- **IF you generate code WITHOUT the above** → **STOP and regenerate with proper traceability**

**Example Pattern - SERVICE METHOD**:
```typescript
/**
 * Create new {PROGRAM_NAME} record with comprehensive validation
 * @legacySource {program_name}.txt:170-212 (Add{PROGRAM_NAME} subroutine)
 * @description Create new {program_name} with comprehensive validation
 *
 * Business Logic Flow (from RPG lines 170-212):
 * 1. Validate required fields (ID, Name, Date) - line 175
 * 2. Trim key ID ({program_name}.txt:180)
 * 3. Check for duplicate ID via database lookup ({program_name}.txt:185-190)
 * 4. Validate date format ({program_name}.txt:195-200)
 * 5. Validate business rules (status, category, etc.) ({program_name}.txt:202-205)
 * 6. Create audit trail with timestamp and program name ({program_name}.txt:208-209)
 * 7. Write to database ({program_name}.txt:210)
 */
async create{PROGRAM_NAME}(data: I{PROGRAM_NAME}Create): Promise<I{PROGRAM_NAME}Record> {
  try {
    logger.info('Creating new {program_name}', { id: data.{id_field} });

    // Line 175: Validate input data (Validate{PROGRAM_NAME}Fields subroutine)
    const validation = {PROGRAM_NAME}Model.validateCreate(data);
    if (validation.error) {
      const errorMessage = validation.error.details.map(d => d.message).join(', ');
      logger.error('Validation error:', { error: errorMessage });
      throw new Error(errorMessage);
    }

    // Line 180: Trim key ID (%trim(KEYID))
    const trimmedKey = data.key_id.trim();

    // Line 185-190: Check for duplicate ID (Chain AKEYID {FILE}; If %found...)
    const exists = await this.repository.exists(trimmedKey);
    if (exists) {
      const errorMessage = `ID ${trimmedKey} already exists.`;  // Exact legacy message (line 188)
      logger.error(errorMessage);
      throw new Error(errorMessage);
    }

    // Line 195-200: Validate date format (monitor; DateVal = %date(ADATE: *ISO))
    const dateValidation = {PROGRAM_NAME}Model.validateDate(data.date_field);
    if (!dateValidation.valid) {
      const errorMessage = dateValidation.error || 'Invalid Date.';
      logger.error('Date validation failed:', { error: errorMessage });
      throw new Error(errorMessage);
    }

    // Line 202-205: Validate business rules
    // Add specific business rule validations here with line comments

    // Line 208-209: Create audit trail
    // Fields: {CREATE_TS} = %char(%timestamp()), {CREATE_PGM} = PgmDS.PgmName
    const record = {PROGRAM_NAME}Model.toRecord(
      { ...data, key_id: trimmedKey },
      PROGRAM_METADATA.{PROGRAM_NAME}_PROGRAM
    );

    // Line 210: Write to database
    const new{PROGRAM_NAME} = await this.repository.create(record);

    // Success message matches {program_name}.txt:211
    logger.info(`{PROGRAM_NAME} ${trimmedKey} added successfully.`);

    return new{PROGRAM_NAME};
  } catch (error) {
    logger.error('Error creating {program_name}:', error);
    throw error;
  }
}
```

**Example Pattern - CONTROLLER FILE-LEVEL**:
```typescript
/**
 * @legacySource {program_name}.txt:1-50 (Command Key Processing section)
 * @description {PROGRAM_NAME} maintenance controller
 * 
 * RPG Command Key Mappings (from {program_name}.txt):
 * - F3 (Exit) → Client-side navigation (line 25)
 * - F6 (Add) → POST endpoint (line 28)
 * - F9 (Change) → PUT endpoint (line 32)
 * - F11 (Display Names) → Query parameter toggle (line 35)
 * - F12 (Cancel) → Client-side cancel (line 38)
 * - F15 (Batch Update) → POST /batch-update endpoint (line 42)
 * - Enter (Process) → Submit form to appropriate endpoint (line 45)
 */
export function create{PROGRAM_NAME}Controller(pool: Pool) {
  const service = new {PROGRAM_NAME}Service(pool);

  return {
    /**
     * @legacySource {program_name}.txt:60-95 (Display subfile list - DspSfl subroutine)
     * @description Retrieve all records with filtering/pagination
     * Maps to RPG subfile display logic (SFLPAG/SFLSIZ operations)
     */
    async getAll{PROGRAM_NAME}s(req: Request, res: Response): Promise<void> {
      // Implementation...
    },
    // ... more methods with JSDoc
  };
}
```

**Example Pattern - REPOSITORY METHOD**:
```typescript
/**
 * @legacySource {program_name}.txt:320-335 (Get{PROGRAM_NAME} subroutine)
 * @description Retrieve {program_name} by ID (CHAIN operation from line 322)
 */
async findById(id: string): Promise<I{PROGRAM_NAME}Record | null> {
  if (this.useMockDB) {
    return this.mockDB.get{PROGRAM_NAME}ById(id) || null;
  }

  // Line 322: CHAIN {KEY_FIELD} {FILE_NAME} (Primary key lookup)
  const query = 'SELECT * FROM {table} WHERE {key_field} = $1 AND record_status = $2';
  const result: QueryResult<any> = await this.pool.query(query, [id, 'A']);

  // Line 324: Check if record found (%found indicator)
  if (result.rows.length === 0 || !result.rows[0]) {
    logger.warn(`{PROGRAM_NAME} not found`, { id });
    return null;  // Matches RPG *INLr indicator (line 325)
  }

  return this.mapRowTo{PROGRAM_NAME}(result.rows[0]);
}
```

**Example Pattern - MODEL JOI SCHEMA**:
```typescript
/**
 * @legacySource {DDS_FILE}.txt:15-80 (DDS field definitions)
 * @description {PROGRAM_NAME} data validation matching DDS specifications
 */
export class {PROGRAM_NAME}Model {
  private static createSchema = Joi.object({
    // Line 20: {FIELD1} field - 7A (7 characters, alphanumeric)
    {field1}: Joi.string().length(7).required().messages({
      'string.length': '{Field1} must be 7 characters',  // DDS length check (line 21)
      'string.empty': '{Field1} cannot be blank.',       // DDS DSPATR(*PR) (line 22)
      'any.required': '{Field1} cannot be blank.'
    }),
    
    // Line 25: {FIELD2} field - 30A
    {field2}: Joi.string().max(30).required().messages({
      'string.max': '{Field2} cannot exceed 30 characters',  // DDS length (line 26)
      'any.required': '{Field2} is required'
    }),
    
    // Line 30: {STATUS} field - 1A (values: A=Active, I=Inactive)
    {status}: Joi.string().valid('A', 'I').required().messages({
      'any.only': 'Status must be A or I',  // DDS VALUES keyword (line 31)
      'any.required': 'Status required'
    })
  });

  /**
   * @legacySource {program_name}.txt:277-295 (Validate{DATE} subroutine)
   * @description Validate {date} field with business rules
   * 
   * Validation Rules (from RPG lines 277-295):
   * 1. Check format YYYYMMDD - line 280
   * 2. Check calendar validity - line 285
   * 3. Check not future date - line 290
   */
  public static validate{DATE}(dateStr: string): { valid: boolean; error?: string } {
    // Line 280: Format check
    if (!/^\d{8}$/.test(dateStr)) {
      return { valid: false, error: 'Invalid Date. Must be YYYYMMDD format' };  // Exact RPG error (line 281)
    }

    // Line 285: Calendar validity check
    // ... implementation

    // Line 290: No future dates
    // ... implementation

    return { valid: true };
  }
}
```

### **INFRASTRUCTURE UTILITIES** (Enhanced Patterns)

**MANDATORY - Generate these utilities**:

#### 1. Transaction Support (`src/_shared/database/transaction.ts`)
```typescript
import { Pool, PoolClient } from 'pg';
import winston from 'winston';

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.json(),
  transports: [new winston.transports.Console()]
});

/**
 * Transaction wrapper for database operations requiring atomicity
 * LEGACY CONTEXT: IBM i had implicit commitment control - operations were atomic
 */
export async function withTransaction<T>(
  pool: Pool,
  callback: (client: PoolClient) => Promise<T>
): Promise<T> {
  const client = await pool.connect();
  try {
    logger.info('Transaction started');
    await client.query('BEGIN');
    const result = await callback(client);
    await client.query('COMMIT');
    logger.info('Transaction committed successfully');
    return result;
  } catch (error) {
    await client.query('ROLLBACK');
    logger.error('Transaction rolled back due to error:', {
      error: error instanceof Error ? error.message : 'Unknown error'
    });
    throw error;
  } finally {
    client.release();
  }
}
```

#### 2. Program Metadata Constants (`src/_shared/constants/metadata.ts`)
```typescript
/**
 * Centralized program metadata for audit trail consistency
 * Maps modern modules to legacy IBM i program names
 */
export const PROGRAM_METADATA = {
  {PROGRAM_NAME}_PROGRAM: '{PROGRAM_NAME}',
  UNKNOWN: 'MODERNAPI'
} as const;

export const getCurrentTimestamp = (): string => new Date().toISOString();
export const getCurrentUser = (): string => process.env.USER || 'SYSTEM';
```

#### 3. Enhanced Validation Utils (`src/_shared/utils/validation.ts`)
```typescript
/**
 * Date validation with business rules
 * Pattern from RPG: monitor; DateVal = %date(FIELD: *ISO); on-error; endmon;
 */
export function validateDate(dateStr: string): { valid: boolean; error?: string } {
  // Format check (YYYYMMDD)
  if (!/^\d{8}$/.test(dateStr)) {
    return { valid: false, error: 'Invalid Date. Must be YYYYMMDD format' };
  }

  // Calendar validity check
  const year = parseInt(dateStr.substring(0, 4));
  const month = parseInt(dateStr.substring(4, 6));
  const day = parseInt(dateStr.substring(6, 8));
  const date = new Date(year, month - 1, day);

  if (date.getFullYear() !== year || date.getMonth() !== month - 1 || date.getDate() !== day) {
    return { valid: false, error: 'Invalid Date.' };
  }

  // Business rule: No future dates
  const today = new Date();
  today.setHours(0, 0, 0, 0);
  if (date > today) {
    return { valid: false, error: 'Date cannot be in the future.' };
  }

  return { valid: true };
}

export function validateRequired(value: any, fieldName: string): { valid: boolean; error?: string } {
  if (value === null || value === undefined || (typeof value === 'string' && value.trim() === '')) {
    return { valid: false, error: `${fieldName} cannot be blank.` };
  }
  return { valid: true };
}
```

#### 4. String Utils (`src/_shared/utils/stringUtils.ts`)
```typescript
/**
 * String manipulation utilities matching RPG built-in functions
 */

// RPG: %trim(field)
export function trim(str: string): string {
  return str.trim();
}

// RPG: %char(value)
export function toChar(value: any): string {
  return String(value);
}

// RPG: %subst(string: start: length)
export function substr(str: string, start: number, length?: number): string {
  return str.substring(start - 1, length ? start - 1 + length : undefined);
}

// RPG: %xlate(from: to: string)
export function translate(str: string, from: string, to: string): string {
  let result = str;
  for (let i = 0; i < from.length; i++) {
    result = result.replace(new RegExp(from[i], 'g'), to[i] || '');
  }
  return result;
}
```

### **CRITICAL FIXES** (Prevent Common Issues)

**MANDATORY - Apply these fixes in generated code**:

#### Fix 1: Vite Environment Variables (CRITICAL)
```typescript
// ❌ WRONG - Causes "process is not defined" in browser
const API_BASE_URL = process.env.REACT_APP_API_BASE_URL;

// ✅ CORRECT - Use import.meta.env for Vite projects
const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:3000/api/v1';
```

#### Fix 2: API Endpoint Consistency (HIGH Priority)
```typescript
// Backend routes: MUST use /api/v1/ prefix
app.use('/api/v1/{program_name_lowercase}', {PROGRAM_NAME}Routes);

// Frontend API calls: MUST match backend prefix
const response = await fetch(`${API_BASE_URL}/{program_name_lowercase}`);

// Vite proxy configuration
server: {
  proxy: {
    '/api': {
      target: 'http://localhost:3000',
      rewrite: (path) => path.replace(/^\/api/, '/api/v1')
    }
  }
}
```

#### Fix 3: Cross-Platform Scripts (HIGH Priority)
```json
{
  "scripts": {
    "dev:fullstack:mock": "cross-env USE_MOCK_DB=true concurrently \"npm run dev\" \"npm run dev:frontend\""
  }
}
```

#### Fix 4: Enhanced Null Safety in Repository
```typescript
// ALWAYS check both conditions
if (result.rows.length === 0 || !result.rows[0]) {
  logger.warn(`Operation returned no rows for ${id}`);
  return null;
}
return this.mapRowToRecord(result.rows[0]);
```

#### Fix 5: Audit Trail Initialization
```typescript
// Initialize BOTH create and update fields on record creation
const timestamp = new Date().toISOString();
return {
  ...data,
  create_timestamp: timestamp,
  create_program: PROGRAM_METADATA.{PROGRAM_NAME}_PROGRAM,
  update_timestamp: timestamp,  // Initialize update fields too
  update_program: PROGRAM_METADATA.{PROGRAM_NAME}_PROGRAM
};
```

## 🔧 CONDITIONAL LOGIC FOR FILE-SPECIFIC GENERATION

### **IF DDS FILE EXISTS** → Generate React Frontend
```typescript
if (ddsFile) {
  // Generate React components, UI files, Vite config
  // Add React dependencies to package.json
  // Create frontend development scripts
}
```

### **IF ACTION DIAGRAM EXISTS** → Generate Advanced Validation  
```typescript
if (adFile) {
  // Generate complex validation middleware
  // Create business rules utilities
  // Add enhanced error handling
}
```

### **IF CL FILE EXISTS** → Generate Background Job System
```typescript
if (clFile) {
  // Generate job processing services
  // Create print/PDF services
  // Add queue management system
}
```

### **IF UNKNOWN FILES EXIST** → Auto-Detect and Generate
```typescript
unknownFiles.forEach(file => {
  switch(file.detectedType) {
    case 'SQL': generateDatabaseSchema(file);
    case 'COPYBOOK': generateSharedTypes(file);
    case 'CONFIG': generateConfigurationFiles(file);
    // ... handle other detected types
  }
});
```

### Dependencies - ADAPTIVE Package.json Update
**MANDATORY**: Update package.json with dependencies based on detected files:
```bash
# CORE Dependencies (Always Installed)
npm install pg joi winston express cors typescript tsx

# CONDITIONAL Dependencies (Install based on detected files):

# IF DDS FILE EXISTS (Frontend)
npm install react react-dom axios
npm install --save-dev @types/react @types/react-dom @vitejs/plugin-react vite

# IF CL FILE EXISTS (Background Jobs) 
npm install bull redis pdf-kit nodemailer

# IF ADVANCED VALIDATION NEEDED (AD File)
npm install joi-date express-validator

# Development Dependencies (Always)
npm install --save-dev @types/pg @types/express @types/cors @types/node cross-env concurrently eslint prettier

# ADAPTIVE Scripts (Generated based on available files):
"scripts": {
  "dev": "tsx src/index.ts",
  "build": "tsc",
  
  // IF DDS EXISTS - Add frontend scripts
  "dev:frontend": "vite --config vite.config.ts",
  "dev:fullstack:mock": "cross-env USE_MOCK_DB=true concurrently \"npm run dev\" \"npm run dev:frontend\"",
  "dev:fullstack:prod": "cross-env USE_MOCK_DB=false NODE_ENV=production concurrently \"npm run dev\" \"npm run dev:frontend\"",
  "build:frontend": "vite build --config vite.config.ts",
  
  // IF CL EXISTS - Add job scripts  
  "job:start": "tsx src/jobs/{PROGRAM_NAME}Job.ts",
  "queue:worker": "tsx src/queues/{PROGRAM_NAME}Queue.ts",
  
  // IF NO DDS - Backend only scripts
  "dev:backend": "tsx src/index.ts",
  "dev:mock": "cross-env USE_MOCK_DB=true tsx src/index.ts",
  "dev:prod": "cross-env USE_MOCK_DB=false tsx src/index.ts"
}
```

### TypeScript Type Safety
**MANDATORY**: Use explicit typing, NO 'any' types:
- PostgreSQL operations: `(row: any) => this.mapRow{PROGRAM_NAME}Model(row)`
- Query parameters: `params: (string | number | boolean)[]`
- Database results: `result: QueryResult<any>`

### Logging Standards
**MANDATORY**: Use Winston logger, NO console.log:
```typescript
import winston from 'winston';
logger.info('Processing {PROGRAM_NAME}:', data);
logger.error('Database error:', error);
```

### Variable Declarations
**MANDATORY**: Use 'const' for variables never reassigned:
```typescript
const new{PROGRAM_NAME} = { ...existing{PROGRAM_NAME}, ...updates };
```

## File Generation Requirements:

### 1. Interface ({PROGRAM_NAME}.ts)
- Extract all DDS field definitions
- Create main interface: `I{PROGRAM_NAME}Record`
- Include: `I{PROGRAM_NAME}Filter`, `I{PROGRAM_NAME}Create`, `I{PROGRAM_NAME}Update`
- Define API request/response contracts

### 2. Model ({PROGRAM_NAME}Model.ts)
- Class name: `{PROGRAM_NAME}Model`
- Joi validation schemas for all fields
- Data transformation methods
- **MANDATORY**: `import Joi from 'joi';`

### 3. Repository ({PROGRAM_NAME}Repository.ts)
- Class name: `{PROGRAM_NAME}Repository`
- Convert RPG file ops to PostgreSQL:
  - SETLL/READ → SELECT with WHERE/ORDER BY
  - CHAIN → SELECT by PRIMARY KEY
  - WRITE → INSERT
  - UPDATE → UPDATE with WHERE
  - DELETE → UPDATE SET record_status = 'D' (soft delete)
- **MANDATORY**: `import { Pool, PoolClient, QueryResult } from 'pg';`
- Type all database operations properly

### 4. Service ({PROGRAM_NAME}Service.ts)
- Class name: `{PROGRAM_NAME}Service`
- Extract business logic from RPG subroutines
- Implement calculations and validations
- **MANDATORY**: Winston logging, no console.log
- **MANDATORY**: `import { Pool } from 'pg';`

### 5. Controller ({PROGRAM_NAME}Controller.ts)
- Export function: `create{PROGRAM_NAME}Controller(pool: Pool)`
- Map RPG command keys to HTTP methods:
  - F3 (Exit) → Client navigation
  - F9 (Add/Change) → POST/PUT endpoints
  - F15 (Batch) → POST batch-update
- **MANDATORY**: Winston logging
- **MANDATORY**: `import { Pool } from 'pg';`

### 6. Routes ({PROGRAM_NAME}Routes.ts)
- Export function: `create{PROGRAM_NAME}Routes(pool: Pool)`
- **MANDATORY**: `import { Pool } from 'pg';`

### 7. UI ({PROGRAM_NAME}View.tsx)
- Component name: `{PROGRAM_NAME}View`
- Convert DDS subfiles to React data grids
- Convert command keys to buttons
- Include form validation

### 8. Main Server (src/index.ts)
- **MANDATORY**: Include database mode switching:
```typescript
const USE_MOCK_DB = process.env.USE_MOCK_DB === 'true';
// Initialize mock or PostgreSQL based on environment
```
- Health check endpoint: `/health`
- CORS enabled for frontend communication

### 9. Mock Database ({PROGRAM_NAME}MockDB.ts)
- Singleton pattern: `{PROGRAM_NAME}MockDB.getInstance()`
- In-memory data with sample records from DDS analysis
- Implements same interface as PostgreSQL repository
- **MANDATORY**: `import { I{PROGRAM_NAME}, I{PROGRAM_NAME}Filter } from '../../interfaces/{PROGRAM_NAME}';`

### 10. React Application Files
- **main.tsx**: React app entry with `ReactDOM.createRoot`
- **index.html**: HTML template with root div and module script
- **styles.css**: Complete styling for data grids, forms, buttons
- **MANDATORY**: Axios for API calls to backend

### 11. Vite Configuration (vite.config.ts)
```typescript
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { resolve } from 'path';

export default defineConfig({
  plugins: [react()],
  root: resolve(__dirname, 'src', 'ui'),
  build: { outDir: resolve(__dirname, 'dist', 'ui'), emptyOutDir: true },
  server: { 
    port: 3001, 
    proxy: { '/api': { target: 'http://localhost:3000', changeOrigin: true } } 
  },
  resolve: { 
    alias: { 
      '@': resolve(__dirname, 'src'),
      '@ui': resolve(__dirname, 'src', 'ui'),
      '@interfaces': resolve(__dirname, 'src', 'interfaces') 
    } 
  }
});
```

### 12. Environment Configuration (.env)
```bash
# Database Configuration Mode
USE_MOCK_DB=true

# Application Configuration  
PORT=3000
NODE_ENV=development

# PostgreSQL Configuration (when USE_MOCK_DB=false)
DB_HOST=localhost
DB_PORT=5432
DB_NAME={program_name_lowercase}_dev
DB_USER=postgres
DB_PASSWORD=password

# Logging Configuration
LOG_LEVEL=debug
```

## Database Setup
Generate complete PostgreSQL setup:

### SQL DDL Script
```sql
-- Table: {program_name_lowercase}
-- Based on DDS field definitions
-- Include audit fields: create_date, create_time, create_user, create_program, 
--                      change_date, change_time, change_user, change_program
-- Add: record_status CHAR(1) DEFAULT 'A'
```

### Connection Pool
```typescript
import { Pool } from 'pg';
const pool = new Pool({
  user: process.env.DB_USER || 'postgres',
  host: process.env.DB_HOST || 'localhost', 
  database: process.env.DB_NAME || 'seaboard_modernization',
  password: process.env.DB_PASSWORD || 'password',
  port: parseInt(process.env.DB_PORT || '5432'),
});
```

## REST API Endpoints Pattern
Generate these standard endpoints (universal for all programs):
- GET /api/v1/{program_name_lowercase} - List with filters/pagination
- GET /api/v1/{program_name_lowercase}/:key - Get single record
- POST /api/v1/{program_name_lowercase} - Create new record
- PUT /api/v1/{program_name_lowercase}/:key - Update existing
- DELETE /api/v1/{program_name_lowercase}/:key - Soft delete
- POST /api/v1/{program_name_lowercase}/batch-update - Batch operations

Additional endpoints may be added based on specific RPG program functions:
- POST /api/v1/{program_name_lowercase}/validate - Validation endpoint
- GET /api/v1/{program_name_lowercase}/statistics - Analytics/reporting
- POST /api/v1/{program_name_lowercase}/calculate - Business calculations

## Technical Stack
- TypeScript 5.9+ with strict mode
- Express.js for REST API
- PostgreSQL with pg driver
- React for UI components
- Winston for logging (NOT console.log)
- Joi for input validation
- ESLint/Prettier compliance

## Conversion Patterns
- DDS screen fields → React form inputs
- DDS subfiles → React data grids with pagination
- RPG command keys → Button click handlers
- RPG validation subroutines → Joi schemas
- RPG business subroutines → TypeScript service methods

## Quality Requirements
Ensure generated code:
1. Compiles without TypeScript errors
2. Passes ESLint validation (no 'any', no console.log, proper 'const')
3. Includes all required dependencies
4. Has proper error handling
5. Preserves 100% of RPG business logic
6. Is production-ready and cloud-deployable

## 🚀 IMMEDIATE STARTUP COMMANDS - Copy/Paste Ready!

After LLM generates all files, run these commands in sequence:

### 1. Install All Dependencies
```bash
npm install pg joi winston express cors axios react react-dom
npm install --save-dev @types/pg @types/express @types/cors @types/node @types/react @types/react-dom typescript @vitejs/plugin-react vite cross-env concurrently eslint prettier tsx
```

### 2. Start Development (Mock Database)
```bash
npm run dev:fullstack:mock
```
**Result**: 
- Backend API: http://localhost:3000
- React Frontend: http://localhost:3001  
- Health Check: http://localhost:3000/health

### 3. Switch to Production Database
```bash
npm run dev:fullstack:prod
```

### 4. Build for Production
```bash
npm run build
npm run build:frontend
```

## 🎯 VERIFICATION CHECKLIST
After generation, these should work immediately:
- ✅ `npm install` - No dependency errors
- ✅ `npm run build` - TypeScript compiles successfully  
- ✅ `npm run dev:fullstack:mock` - Both servers start
- ✅ http://localhost:3000/health - Shows database mode
- ✅ http://localhost:3001 - React app loads
- ✅ API calls work between frontend/backend
- ✅ CRUD operations function in UI

## 🔧 READY-TO-USE OUTPUT
The generated application will be immediately production-ready with:
- ✅ Full TypeScript compilation
- ✅ React hot-reload development  
- ✅ API proxy configuration
- ✅ Mock database for development
- ✅ PostgreSQL production setup
- ✅ Environment variable switching
- ✅ ESLint/Prettier compliance
- ✅ Complete CRUD operations
- ✅ Responsive UI components

**No additional configuration needed - just run the commands above!**

## 🎯 FINAL OUTPUT REQUIREMENTS

### **MANDATORY ANALYSIS FIRST:**
1. **Analyze all attached files** and determine their types (RPG, DDS, AD, CL, or unknown)
2. **Auto-detect unknown files** by analyzing content patterns  
3. **Determine modernization scope** based on available files
4. **Report what will be generated** before starting code generation

### **ADAPTIVE CODE GENERATION:**
Generate files based on this decision matrix:

| Input Files Available | Files Generated | Functionality |
|----------------------|-----------------|---------------|
| **RPG Only** | Backend API (7 files) + Mock DB (2 files) + Config (2 files) | REST API, Database operations, No frontend |
| **RPG + DDS** | Backend (7) + Mock (2) + Frontend (4) + Config (3) | Full-stack web application |
| **RPG + AD** | Backend (7) + Mock (2) + Advanced Validation (2) + Config (2) | Enhanced business logic validation |
| **RPG + CL** | Backend (7) + Mock (2) + Background Jobs (3) + Config (2) | API with batch processing |
| **RPG + DDS + AD + CL** | All files (up to 23 files) | Complete enterprise application |

### **SMART SCRIPT GENERATION:**
- **If DDS exists**: Generate full-stack scripts (`dev:fullstack:mock`, `dev:fullstack:prod`)
- **If DDS missing**: Generate backend-only scripts (`dev:backend`, `dev:mock`, `dev:prod`)
- **If CL exists**: Add job processing scripts (`job:start`, `queue:worker`)

### **IMMEDIATE FUNCTIONALITY GUARANTEE:**
The generated application must start successfully with the appropriate command:
- **With DDS**: `npm run dev:fullstack:mock`
- **Without DDS**: `npm run dev:mock`
- **Production mode**: `npm run dev:prod` or `npm run dev:fullstack:prod`

**All generated code must be production-ready, properly typed, and immediately functional after `npm install`.**

---

## 🎯 HOW TO USE THIS PROMPT

### Step 1: Prepare Your Files (Flexible System)
- **REQUIRED**: Locate your RPG source file → Rename to `{PROGRAM_NAME}_RPG.txt`
- **OPTIONAL**: Find DDS file (for frontend) → Rename to `{PROGRAM_NAME}_DDS.txt`  
- **OPTIONAL**: Find Action Diagram (for validation) → Rename to `{PROGRAM_NAME}_AD.txt`
- **OPTIONAL**: Find Control Language (for jobs) → Rename to `{PROGRAM_NAME}_CL.txt`
- **UNKNOWN**: Any other files → Rename to `{PROGRAM_NAME}_UNKNOWN1.txt`, `{PROGRAM_NAME}_UNKNOWN2.txt`, etc.

### Step 2: Customize the Prompt  
- Replace `{PROGRAM_NAME}` with your actual program name (e.g., PBGREFR)
- Replace `{program_name_lowercase}` with lowercase version (e.g., pbgrefr)

### Step 3: Run in Any LLM
- Copy the complete prompt template above (lines 17-332)
- Attach your available source files (minimum: RPG file)
- Paste and execute
- **LLM will analyze files and adapt modernization scope automatically**

### Step 4: Get Adaptive Application
- **LLM analyzes your files** and reports what will be generated
- **Generates appropriate files** (7-23 files depending on input)
- **Copy files** to your project directory
- **Run appropriate startup command** based on generated files
- **Application is ready!**

### Step 5: Immediate Testing (Adaptive Commands)
```bash
npm install

# IF DDS FILE WAS PROVIDED (Full-stack):
npm run dev:fullstack:mock
# Visit http://localhost:3001 for React UI
# Visit http://localhost:3000/health for API status

# IF NO DDS FILE (Backend only):
npm run dev:mock  
# Visit http://localhost:3000/health for API status
# Test API endpoints with Postman/curl

# IF CL FILE PROVIDED (Background jobs):
npm run job:start
npm run queue:worker
```

**Result**: Intelligently modernized application adapted to your specific input files - from simple backend API to complete enterprise application with frontend, background jobs, and advanced validation!
