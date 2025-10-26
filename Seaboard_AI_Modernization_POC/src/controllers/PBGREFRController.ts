/**
 * PBGREFR - MPR FOB Omaha Rate Table Controller
 *
 * REST API controller replacing green-screen command key handlers
 * Maps RPG function keys to HTTP endpoints
 *
 * RPG Command Keys → HTTP Methods Mapping:
 * - F3 (Exit) → Client-side navigation
 * - F4 (Prompt) → Client-side search/autocomplete
 * - F9 (Mode Switch) → Handled by separate endpoints (POST vs PUT)
 * - F10 (Edit Control) → Separate endpoint or related resource
 * - F15 (Batch Update) → POST /api/v1/pbgrefr/batch-update
 * - ENTER (Confirm) → POST/PUT operations
 */

import { Request, Response } from 'express';
import { Pool } from 'pg';
import { IFreightRateDatabase } from '../interfaces/Database';
import PBGREFRService from '../services/PBGREFRService';
import {
  IBatchRateUpdateRequest,
  IFreightRateCreateRequest,
  IFreightRateFilter,
  IFreightRateUpdateRequest,
} from '../interfaces/PBGREFR';

/**
 * PBGREFRController - REST API controller for freight rates
 * Replaces RPG screen processing with HTTP request/response handling
 */
export class PBGREFRController {
  private service: PBGREFRService;

  constructor(pool: Pool) {
    this.service = new PBGREFRService(pool);
  }

  /**
   * GET /api/v1/pbgrefr
   * Get paginated list of freight rates with optional filters
   *
   * Maps to RPG: Initial screen display and BAIZSF/BBLDSF subroutines
   * Query params: country, region, stateCode, zipCode, rate, page, pageSize
   *
   * @param req Express request
   * @param res Express response
   */
  async getFreightRates(req: Request, res: Response): Promise<void> {
    try {
      const filter: IFreightRateFilter = {
        mprCountry: req.query.country as string,
        mprRegion: req.query.region ? parseInt(req.query.region as string, 10) : undefined,
        mprStateCode: req.query.stateCode as string,
        mprZipCode: req.query.zipCode ? parseInt(req.query.zipCode as string, 10) : undefined,
        mprFobRatePerMile: req.query.rate ? parseFloat(req.query.rate as string) : undefined,
        page: req.query.page ? parseInt(req.query.page as string, 10) : 0,
        pageSize: req.query.pageSize ? parseInt(req.query.pageSize as string, 10) : 13,
        sortBy: req.query.sortBy as string,
        sortDirection: (req.query.sortDirection as 'ASC' | 'DESC') || 'ASC',
      };

      const userName = this.extractUserName(req);
      const result = await this.service.getFreightRates(filter, userName);

      res.status(200).json({
        success: true,
        message: 'Freight rates retrieved successfully',
        data: result.data,
        pagination: {
          totalRecords: result.totalRecords,
          currentPage: result.currentPage,
          pageSize: result.pageSize,
          totalPages: result.totalPages,
          hasMore: result.hasMore,
        },
      });
    } catch (error) {
      this.handleError(res, error, 'Error retrieving freight rates');
    }
  }

  /**
   * GET /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Get single freight rate by composite key
   *
   * Maps to RPG: CHAIN operation for record retrieval
   *
   * @param req Express request
   * @param res Express response
   */
  async getFreightRateByKey(req: Request, res: Response): Promise<void> {
    try {
      const key = {
        mprCountry: req.params.country,
        mprRegion: parseInt(req.params.region, 10),
        mprStateCode: req.params.stateCode,
        mprZipCode: parseInt(req.params.zipCode, 10),
      };

      // Validate key parameters
      if (
        !key.mprCountry ||
        isNaN(key.mprRegion) ||
        !key.mprStateCode ||
        isNaN(key.mprZipCode)
      ) {
        res.status(400).json({
          success: false,
          message: 'Invalid key parameters',
          error: {
            code: 'INVALID_KEY',
            details: 'All key fields must be provided and valid',
          },
        });
        return;
      }

      const result = await this.service.getFreightRateByKey(key);

      if (!result.success) {
        res.status(404).json(result);
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate retrieved successfully',
        data: result.data,
      });
    } catch (error) {
      this.handleError(res, error, 'Error retrieving freight rate');
    }
  }

  /**
   * POST /api/v1/pbgrefr
   * Create new freight rate
   *
   * Maps to RPG: ADD mode processing and SACRRC subroutine
   * Replaces F9 (Add Mode) + ENTER sequence
   *
   * @param req Express request
   * @param res Express response
   */
  async createFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const createRequest: IFreightRateCreateRequest = req.body;
      const userName = this.extractUserName(req);

      const result = await this.service.createFreightRate(createRequest, userName);

      if (!result.success) {
        res.status(400).json(result);
        return;
      }

      res.status(201).json({
        success: true,
        message: 'Freight rate created successfully',
        data: result.data,
      });
    } catch (error) {
      this.handleError(res, error, 'Error creating freight rate');
    }
  }

  /**
   * PUT /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Update existing freight rate
   *
   * Maps to RPG: CHANGE mode processing and SBCHRC subroutine
   * Replaces F9 (Change Mode) + ENTER sequence
   *
   * @param req Express request
   * @param res Express response
   */
  async updateFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const updateRequest: IFreightRateUpdateRequest = {
        mprCountry: req.params.country,
        mprRegion: parseInt(req.params.region, 10),
        mprStateCode: req.params.stateCode,
        mprZipCode: parseInt(req.params.zipCode, 10),
        ...req.body,
      };

      const userName = this.extractUserName(req);
      const result = await this.service.updateFreightRate(updateRequest, userName);

      if (!result.success) {
        const statusCode = result.error?.code === 'Y2U0009' ? 404 : 400;
        res.status(statusCode).json(result);
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate updated successfully',
        data: result.data,
      });
    } catch (error) {
      this.handleError(res, error, 'Error updating freight rate');
    }
  }

  /**
   * DELETE /api/v1/pbgrefr/:country/:region/:stateCode/:zipCode
   * Delete freight rate (soft delete)
   *
   * Note: Original RPG program doesn't support delete
   * Included for REST API completeness
   *
   * @param req Express request
   * @param res Express response
   */
  async deleteFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const key = {
        mprCountry: req.params.country,
        mprRegion: parseInt(req.params.region, 10),
        mprStateCode: req.params.stateCode,
        mprZipCode: parseInt(req.params.zipCode, 10),
      };

      const result = await this.service.deleteFreightRate(key);

      if (!result.success) {
        res.status(404).json(result);
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate deleted successfully',
      });
    } catch (error) {
      this.handleError(res, error, 'Error deleting freight rate');
    }
  }

  /**
   * POST /api/v1/pbgrefr/batch-update
   * Batch update freight rates
   *
   * Maps to RPG: CF15 (F15) command key calling PBIBPVR program
   * Replaces "Pmt Chg Rate per Regn PV" function
   *
   * @param req Express request
   * @param res Express response
   */
  async batchUpdateRates(req: Request, res: Response): Promise<void> {
    try {
      const batchRequest: IBatchRateUpdateRequest = req.body;
      const userName = this.extractUserName(req);

      const result = await this.service.batchUpdateRates(batchRequest, userName);

      if (!result.success) {
        res.status(400).json(result);
        return;
      }

      res.status(200).json({
        success: true,
        message: result.message,
        recordsUpdated: result.recordsUpdated,
      });
    } catch (error) {
      this.handleError(res, error, 'Error performing batch update');
    }
  }

  /**
   * POST /api/v1/pbgrefr/validate
   * Validate freight rate data without persisting
   *
   * Maps to RPG: DEV1RC/DFV2RC validation subroutines
   * Useful for client-side validation before submission
   *
   * @param req Express request
   * @param res Express response
   */
  async validateFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const freightRateData = req.body;
      const result = this.service.validateFreightRate(freightRateData);

      res.status(200).json({
        success: result.valid,
        message: result.valid ? 'Validation passed' : 'Validation failed',
        errors: result.errors,
      });
    } catch (error) {
      this.handleError(res, error, 'Error validating freight rate');
    }
  }

  /**
   * Extract username from request
   * In production, this would come from JWT token or session
   *
   * @param req Express request
   * @returns Username
   */
  private extractUserName(req: Request): string {
    // In production: extract from JWT token or session
    // return (req.user as any)?.username || 'UNKNOWN';

    // For now, use a header or default value
    return (req.headers['x-user-name'] as string) || 'SYSTEM';
  }

  /**
   * Handle controller errors
   *
   * @param res Express response
   * @param error Error object
   * @param message Error message
   */
  private handleError(res: Response, error: unknown, message: string): void {
    console.error(message, error);

    res.status(500).json({
      success: false,
      message,
      error: {
        code: 'INTERNAL_SERVER_ERROR',
        details: error instanceof Error ? error.message : 'Unknown error occurred',
      },
    });
  }
}

export default PBGREFRController;
