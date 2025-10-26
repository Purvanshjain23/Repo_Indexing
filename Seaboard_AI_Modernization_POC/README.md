# Seaboard AI Modernization POC

A proof-of-concept project for modernizing IBM i (AS/400) legacy systems to cloud-native solutions using AI-driven analysis and transformation.

## Overview

This project demonstrates the modernization of legacy IBM i systems by analyzing RPG, DDS, and CL code and transforming them into modern, cloud-ready applications. The solution leverages AI to understand legacy code patterns and generate equivalent modern implementations.

## Project Structure

```
Seaboard_AI_Modernization_POC/
├── blueprint/           # Project documentation and blueprints
├── inputs/             # Legacy source files and analysis data
├── configs/            # Development tool configurations
├── metadata/           # Project metadata and instruction sets
├── src/               # Modern source code implementation
├── scripts/           # Utility and automation scripts
├── README.md          # This file
├── azure-pipelines.yml # CI/CD pipeline
└── package.json       # Node.js dependencies
```

## Key Components

### Legacy Analysis

- **RPG Programs**: Business logic from legacy RPG applications
- **DDS Files**: Data Description Specifications for file layouts
- **CL Programs**: Control Language scripts for system operations
- **Dependency Analysis**: CSV files mapping program relationships

### Modern Implementation

- **TypeScript/Node.js**: Modern runtime environment
- **Cloud-Ready Architecture**: Designed for containerization and cloud deployment
- **API-First Design**: RESTful services replacing legacy program calls
- **Modern Data Access**: Database abstraction replacing native file access

## Getting Started

### Prerequisites

- Node.js 18+ and npm
- TypeScript compiler
- Docker (for containerization)
- Azure CLI (for cloud deployment)

### Installation

1. Clone and navigate to the project:

   ```bash
   cd Seaboard_AI_Modernization_POC
   ```

2. Install dependencies:

   ```bash
   npm install
   ```

3. Build the project:

   ```bash
   npm run build
   ```

4. Validate project structure:
   ```bash
   npm run validate
   ```

## Running the Mock Server

### Quick Start

```powershell
# Build and start the mock server
npm run build && npm start
```

### Step-by-Step Instructions

1. **Build the project** (compiles TypeScript):
   ```powershell
   npm run build
   ```

2. **Start the server**:
   ```powershell
   npm start
   ```

3. **Expected output**:
   ```
   info: Database mock initialized with sample freight rate data
   info: 🚀 Seaboard Modernization POC server running on port 3000
   info: 📊 Health check available at http://localhost:3000/health
   info: 🔧 Environment: development
   ```

### Troubleshooting

#### Port 3000 Already in Use (EADDRINUSE Error)

**Problem**: Error message like `Error: listen EADDRINUSE: address already in use :::3000`

**Solution 1 - Kill process on port 3000**:
```powershell
# Find processes using port 3000
netstat -ano | findstr :3000

# Kill the process (replace PID with actual process ID)
taskkill /PID <PID_NUMBER> /F

# Or use this one-liner to kill any process on port 3000
Get-Process -Id (Get-NetTCPConnection -LocalPort 3000).OwningProcess -ErrorAction SilentlyContinue | Stop-Process -Force
```

**Solution 2 - Use different port**:
```powershell
# Set different port via environment variable
$env:PORT = "3001"
npm start

# Or create .env file with: PORT=3001
```

#### Build Errors with PostgreSQL Dependencies

**Problem**: TypeScript errors about missing `Pool` imports

**Solution**: The project is configured to exclude PostgreSQL-dependent files for mock-only operation. Ensure `configs/tsconfig.json` excludes:
```json
"exclude": [
  "../src/services/PBGREFRService.ts",
  "../src/repository/PBGREFRRepository.ts", 
  "../src/controllers/PBGREFRController.ts"
]
```

## API Testing

### Health Check

**PowerShell**:
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/health" -Method GET
```

**Browser**: Navigate to `http://localhost:3000/health`

**Expected Response**:
```json
{
  "status": "healthy",
  "timestamp": "2025-10-21T21:23:14.000Z",
  "version": "1.0.0",
  "environment": "development",
  "uptime": 80.9651145
}
```

### Freight Rate API Endpoints

#### Get All Freight Rates

**PowerShell**:
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/" -Method GET
```

**Browser**: Navigate to `http://localhost:3000/api/v1/pbgrefr/`

**curl** (if available):
```bash
curl http://localhost:3000/api/v1/pbgrefr/
```

#### Get Specific Freight Rate

**PowerShell**:
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102" -Method GET
```

**Browser**: Navigate to `http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102`

#### Get Statistics

**PowerShell**:
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/statistics" -Method GET
```

**Browser**: Navigate to `http://localhost:3000/api/v1/pbgrefr/statistics`

#### Create New Freight Rate

**PowerShell**:
```powershell
$body = @{
    mprCountry = "USA"
    mprRegion = 4
    mprStateCode = "TX"
    mprZipCode = 75001
    mprFobRatePerMile = 2.80
    mprFobMilesToOmaha = 450
} | ConvertTo-Json

Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/" -Method POST -Body $body -ContentType "application/json"
```

#### Update Freight Rate

**PowerShell**:
```powershell
$body = @{
    mprFobRatePerMile = 3.00
} | ConvertTo-Json

Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102" -Method PUT -Body $body -ContentType "application/json"
```

#### Delete Freight Rate

**PowerShell**:
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102" -Method DELETE
```

#### Batch Update Rates

**PowerShell**:
```powershell
$body = @{
    newRatePerMile = 2.95
    targetRegion = 1
} | ConvertTo-Json

Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/batch-update" -Method POST -Body $body -ContentType "application/json"
```

### Complete API Reference

| Method | Endpoint | Description | Browser Testable |
|--------|----------|-------------|------------------|
| GET | `/health` | Health check | ✅ Yes |
| GET | `/api/v1/pbgrefr/` | Get all freight rates | ✅ Yes |
| GET | `/api/v1/pbgrefr/statistics` | Get statistics | ✅ Yes |
| GET | `/api/v1/pbgrefr/{country}/{region}/{state}/{zip}` | Get specific rate | ✅ Yes |
| POST | `/api/v1/pbgrefr/` | Create new rate | ❌ No (requires body) |
| PUT | `/api/v1/pbgrefr/{country}/{region}/{state}/{zip}` | Update rate | ❌ No (requires body) |
| DELETE | `/api/v1/pbgrefr/{country}/{region}/{state}/{zip}` | Delete rate | ❌ No (requires method) |
| POST | `/api/v1/pbgrefr/batch-update` | Batch update | ❌ No (requires body) |

### Mock Data

The server starts with 3 sample freight rate records:
- **USA/1/NE/68102** - Rate: $2.50/mile, 0 miles to Omaha
- **USA/2/IA/50001** - Rate: $2.25/mile, 135 miles to Omaha  
- **USA/3/KS/66002** - Rate: $2.75/mile, 200 miles to Omaha

### Important Notes About Mock Database

⚠️ **Data Persistence**: The mock database stores data **in-memory only**. All changes (create, update, delete) are:
- ✅ **Immediately effective** during the server session
- ❌ **Lost when server restarts** - data resets to original 3 records
- ✅ **Fully functional** for testing CRUD operations

**Example - DELETE actually works**:
```powershell
# This will actually remove the record from memory
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102" -Method DELETE

# Subsequent GET requests will return "not found"
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/1/NE/68102" -Method GET
# Returns: "Freight rate not found"

# Restart server to restore all original data
```

**Data Reset**: To restore original data, simply restart the server:
```powershell
# Stop server (Ctrl+C) then restart
npm start
```

### Browser Testing Limitations

**❌ Cannot DELETE via Browser URL**: Browsers only send GET requests when you type URLs. For DELETE operations, use:

**Option 1 - Browser Developer Tools (F12)**:
```javascript
// Open any page on localhost:3000, press F12, Console tab, paste:
fetch('http://localhost:3000/api/v1/pbgrefr/USA/2/IA/50001', {
    method: 'DELETE'
}).then(response => response.json()).then(data => console.log(data));
```

**Option 2 - Use REST Client Tools**:
- **Postman** (downloadable app)
- **Online tools**: [reqbin.com](https://reqbin.com/), [hoppscotch.io](https://hoppscotch.io/)

**Option 3 - PowerShell** (recommended for testing):
```powershell
Invoke-RestMethod -Uri "http://localhost:3000/api/v1/pbgrefr/USA/2/IA/50001" -Method DELETE
```

## Testing with Postman

### Prerequisites
1. **Download Postman**: [https://www.postman.com/downloads/](https://www.postman.com/downloads/)
2. **Install and launch Postman**
3. **Ensure your server is running**: `npm start` (should show port 3000)

### Step-by-Step Postman Testing

#### **1. Health Check (GET)**
- **Method**: `GET`
- **URL**: `http://localhost:3000/health`
- **Headers**: None needed
- **Body**: Not applicable
- **Click**: `Send`
- **Expected Response**: 
  ```json
  {
    "status": "healthy",
    "timestamp": "2025-10-21T21:23:14.000Z",
    "version": "1.0.0",
    "environment": "development",
    "uptime": 80.9651145
  }
  ```

#### **2. Get All Freight Rates (GET)**
- **Method**: `GET`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/`
- **Headers**: None needed
- **Body**: Not applicable
- **Click**: `Send`
- **Expected Response**: List of all freight rates with pagination info

#### **3. Get Specific Freight Rate (GET)**
- **Method**: `GET`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/USA/2/IA/50001`
- **Headers**: None needed
- **Body**: Not applicable
- **Click**: `Send`
- **Expected Response**: Single freight rate record

#### **4. Get Statistics (GET)**
- **Method**: `GET`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/statistics`
- **Headers**: None needed
- **Body**: Not applicable
- **Click**: `Send`
- **Expected Response**: Database statistics and breakdowns

#### **5. Create New Freight Rate (POST)**
- **Method**: `POST`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/`
- **Headers**: 
  - `Content-Type`: `application/json`
- **Body** → **raw** → **JSON**:
  ```json
  {
    "mprCountry": "USA",
    "mprRegion": 4,
    "mprStateCode": "TX",
    "mprZipCode": 75001,
    "mprFobRatePerMile": 2.80,
    "mprFobMilesToOmaha": 450
  }
  ```
- **Click**: `Send`
- **Expected Response**: Created freight rate with success message

#### **6. Update Freight Rate (PUT)**
- **Method**: `PUT`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/USA/2/IA/50001`
- **Headers**: 
  - `Content-Type`: `application/json`
- **Body** → **raw** → **JSON**:
  ```json
  {
    "mprFobRatePerMile": 3.00,
    "mprFobMilesToOmaha": 150
  }
  ```
- **Click**: `Send`
- **Expected Response**: Updated freight rate data

#### **7. Delete Freight Rate (DELETE)**
- **Method**: `DELETE`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/USA/3/KS/66002`
- **Headers**: None needed
- **Body**: Not applicable
- **Click**: `Send`
- **Expected Response**: Success message confirming deletion

#### **8. Batch Update Rates (POST)**
- **Method**: `POST`
- **URL**: `http://localhost:3000/api/v1/pbgrefr/batch-update`
- **Headers**: 
  - `Content-Type`: `application/json`
- **Body** → **raw** → **JSON**:
  ```json
  {
    "newRatePerMile": 2.95,
    "targetRegion": 2
  }
  ```
- **Click**: `Send`
- **Expected Response**: Batch update results

### Postman Collection Setup

**Create a Collection for Easy Testing**:

1. **New Collection**: Click `+` → "Create Collection" → Name: "Seaboard Mock API"

2. **Add Requests**: For each endpoint above:
   - Click "Add Request" in your collection
   - Name it (e.g., "Health Check", "Get All Rates", etc.)
   - Configure Method, URL, Headers, Body as specified
   - Save

3. **Environment Variables** (Optional):
   - Create Environment: "Local Development"
   - Add variable: `baseUrl` = `http://localhost:3000`
   - Use `{{baseUrl}}` in requests instead of full URL

### Testing Workflow

**Recommended Testing Order**:

1. ✅ **Health Check** - Verify server is running
2. ✅ **Get All Rates** - See initial data (3 records)
3. ✅ **Get Statistics** - View data breakdown
4. ✅ **Create New Rate** - Add 4th record
5. ✅ **Get All Rates** - Verify 4 records now
6. ✅ **Update Rate** - Modify existing record
7. ✅ **Get Specific Rate** - Verify update worked
8. ✅ **Delete Rate** - Remove a record
9. ✅ **Get All Rates** - Verify deletion worked
10. ✅ **Batch Update** - Update multiple records

### Troubleshooting in Postman

**Common Issues**:

1. **Connection Error**: 
   - ❌ Error: "Could not get any response"
   - ✅ Solution: Ensure server is running (`npm start`)

2. **404 Not Found**:
   - ❌ Error: "Route not found"
   - ✅ Solution: Check URL spelling, ensure trailing `/` in main endpoint

3. **400 Bad Request**:
   - ❌ Error: "Validation failed"
   - ✅ Solution: Check JSON format, required fields, data types

4. **Content-Type Issues**:
   - ❌ Error: Server can't parse body
   - ✅ Solution: Set `Content-Type: application/json` in headers

### Expected Status Codes

| Operation | Success Code | Error Codes |
|-----------|-------------|-------------|
| GET | 200 OK | 404 Not Found |
| POST | 201 Created | 400 Bad Request, 409 Conflict |
| PUT | 200 OK | 400 Bad Request, 404 Not Found |
| DELETE | 200 OK | 404 Not Found |

### Development Scripts

- `npm run validate` - Validate project structure
- `npm run report` - Generate project analysis reports
- `npm run build` - Compile TypeScript code
- `npm run test` - Run test suites
- `npm run lint` - Check code quality
- `npm run format` - Format code with Prettier

## Architecture

### Legacy System Analysis

The project analyzes legacy IBM i components:

- **Programs**: RPG business logic transformation
- **Files**: DDS to modern database schema conversion
- **Jobs**: CL script to modern automation conversion
- **Dependencies**: Cross-program call analysis

### Modern System Design

Target architecture includes:

- **Microservices**: Decomposed business functions
- **REST APIs**: Modern integration patterns
- **Cloud Database**: Managed database services
- **Container Deployment**: Docker and Kubernetes ready
- **CI/CD Pipeline**: Automated testing and deployment

## Legacy Code Transformation

### Supported Transformations

1. **RPG to TypeScript**: Business logic conversion
2. **DDS to Schema**: Database design modernization
3. **CL to Scripts**: Automation modernization
4. **File I/O to APIs**: Modern data access patterns

### AI-Driven Analysis

- **Pattern Recognition**: Identifies common legacy patterns
- **Dependency Mapping**: Traces program relationships
- **Business Logic Extraction**: Isolates core functionality
- **Modern Equivalents**: Suggests contemporary implementations

## Deployment

### Local Development

```bash
npm start
```

### Docker Containerization

```bash
docker build -t seaboard-modernization .
docker run -p 3000:3000 seaboard-modernization
```

### Azure Deployment

```bash
az container create --resource-group myRG --name seaboard-app --image seaboard-modernization
```

## File Organization

### inputs/

Contains legacy source files and analysis data:

- `PBGREFR_*.txt` - Sample legacy program files
- `*.csv` - Dependency and call analysis data

### src/

Modern implementation organized by function:

- `controllers/` - Business logic handlers
- `models/` - Data models and schemas
- `routes/` - API endpoint definitions
- `interfaces/` - TypeScript type definitions

### scripts/

Automation and utility scripts:

- `validate-structure.js` - Project structure validation
- `generate-report.js` - Analysis report generation

## Configuration

### Development Tools

- **ESLint**: Code quality enforcement
- **Prettier**: Code formatting standards
- **TypeScript**: Type safety and modern JavaScript features

### CI/CD Pipeline

The Azure Pipelines configuration includes:

- Automated testing
- Code quality checks
- Container image building
- Automated deployment

## Contributing

1. Follow the established project structure
2. Run validation scripts before committing
3. Ensure all tests pass
4. Maintain code quality standards

## Documentation

- See `blueprint/` for detailed project documentation
- Review `target_repo_structure.txt` for structure guidelines
- Check generated reports for analysis insights

## Support

For questions about legacy system modernization or this POC implementation, refer to the blueprint documentation or generated analysis reports.

---

**Note**: This is a proof-of-concept project demonstrating AI-driven legacy system modernization. Production implementations should include additional security, monitoring, and compliance considerations.
