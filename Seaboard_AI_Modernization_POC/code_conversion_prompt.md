# 🎯 Universal IBM i to Full-Stack TypeScript/React Modernization Prompt

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

### **🎯 MOCK DATABASE SYSTEM (Always Generated):**
8. src/_shared/local/{PROGRAM_NAME}MockDB.ts - In-memory mock database
9. src/database/connection.ts - PostgreSQL connection pool

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
