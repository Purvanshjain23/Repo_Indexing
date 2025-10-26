/**
 * PBGREFR - MPR FOB Omaha Rate Table Routes
 *
 * Express routing configuration
 * Maps HTTP endpoints to controller methods
 */

import { Router } from 'express';
import { IFreightRateDatabase } from '../interfaces/Database';
import { PBGREFRMockController } from '../controllers/PBGREFRMockController';

/**
 * Create and configure PBGREFR routes
 *
 * @param database Database implementation (PostgreSQL or Mock)
 * @returns Configured Express router
 */
export function createPBGREFRRoutes(database: IFreightRateDatabase): Router {
  const router = Router();
  const controller = new PBGREFRMockController(database);

  /**
   * GET /api/v1/pbgrefr
   * Get paginated list of freight rates with optional filters
   *
   * Query params:
   * - country: Filter by MPR Country
   * - region: Filter by MPR Region (1-14)
   * - stateCode: Filter by MPR State Code
   * - zipCode: Filter by MPR Zip Code
   * - rate: Filter by MPR FOB Rate per Mile
   * - page: Page number (default: 0)
   * - pageSize: Records per page (default: 13)
   * - sortBy: Sort field
   * - sortDirection: ASC or DESC
   */
  router.get('/', (req, res) => controller.getFreightRates(req, res));

  /**
   * GET /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Get single freight rate by composite key
   *
   * Path params:
   * - country: MPR Country (6 chars)
   * - region: MPR Region (1-14)
   * - stateCode: MPR State Code (2 chars)
   * - zipCode: MPR Zip Code (5 digits)
   */
  router.get('/:country/:region/:stateCode/:zipCode', (req, res) =>
    controller.getFreightRateByKey(req, res),
  );

  /**
   * POST /api/v1/pbgrefr
   * Create new freight rate
   *
   * Request body: IFreightRateCreateRequest
   * {
   *   "mprCountry": "USA",
   *   "mprRegion": 5,
   *   "mprStateCode": "NE",
   *   "mprZipCode": 68102,
   *   "mprFobRatePerMile": 2.5,
   *   "mprFobMilesToOmaha": 150
   * }
   */
  router.post('/', (req, res) => controller.createFreightRate(req, res));

  /**
   * PUT /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Update existing freight rate
   *
   * Path params: Composite key (country, region, stateCode, zipCode)
   * Request body: Partial<IFreightRateUpdateRequest>
   * {
   *   "mprFobRatePerMile": 2.75,
   *   "mprFobMilesToOmaha": 155
   * }
   */
  router.put('/:country/:region/:stateCode/:zipCode', (req, res) =>
    controller.updateFreightRate(req, res),
  );

  /**
   * DELETE /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Delete freight rate (soft delete - sets status to 'D')
   *
   * Path params: Composite key (country, region, stateCode, zipCode)
   */
  router.delete('/:country/:region/:stateCode/:zipCode', (req, res) =>
    controller.deleteFreightRate(req, res),
  );

  /**
   * POST /api/v1/pbgrefr/batch-update
   * Batch update freight rates based on filter criteria
   *
   * Maps to RPG CF15 (F15) command key
   *
   * Request body: IBatchRateUpdateRequest
   * {
   *   "targetRegion": 5,
   *   "newRatePerMile": 2.85,
   *   "filter": {
   *     "mprCountry": "USA",
   *     "mprRegion": 5
   *   }
   * }
   */
  router.post('/batch-update', (req, res) => controller.batchUpdateFreightRates(req, res));

  /**
   * POST /api/v1/pbgrefr/validate
   * Validate freight rate data without persisting
   *
   * Useful for client-side validation before submission
   *
   * Request body: Partial<IFreightRate>
   * Response: { valid: boolean, errors: string[] }
   */
  router.get('/statistics', (req, res) => controller.getStatistics(req, res));

  return router;
}

/**
 * API Documentation
 *
 * Base URL: /api/v1/pbgrefr
 *
 * Endpoints:
 * 1. GET    /                                        - List freight rates (with filters)
 * 2. GET    /:country/:region/:stateCode/:zipCode   - Get single freight rate
 * 3. POST   /                                        - Create new freight rate
 * 4. PUT    /:country/:region/:stateCode/:zipCode   - Update freight rate
 * 5. DELETE /:country/:region/:stateCode/:zipCode   - Delete freight rate
 * 6. POST   /batch-update                            - Batch update rates
 * 7. POST   /validate                                - Validate freight rate data
 *
 * Authentication:
 * - All endpoints require authentication (implement JWT middleware)
 * - User identity extracted from 'x-user-name' header (development)
 *
 * Error Responses:
 * - 400: Bad Request (validation errors)
 * - 404: Not Found (record not found)
 * - 500: Internal Server Error (database or server errors)
 *
 * Success Responses:
 * - 200: OK (GET, PUT, DELETE, batch-update, validate)
 * - 201: Created (POST)
 */

export default createPBGREFRRoutes;
