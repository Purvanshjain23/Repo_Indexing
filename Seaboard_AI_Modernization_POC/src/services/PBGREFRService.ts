/**
 * PBGREFR - MPR FOB Omaha Rate Table Service
 *
 * Business logic layer extracted from RPG subroutines
 * Maps RPG program logic to modern TypeScript service methods
 *
 * Original RPG subroutines:
 * - ZZINIT: Initialize program
 * - BAIZSF/BBLDSF: Load subfile (pagination logic)
 * - DAPR##: Process screen input
 * - DEV1RC/DFV2RC: Validate record
 * - SACRRC: Create record
 * - SBCHRC: Change record
 * - ECADRQ/EECHRQ: Process add/change requests
 */

import { Pool } from 'pg';
import { IFreightRateDatabase } from '../interfaces/Database';
import {
  IBatchRateUpdateRequest,
  IFreightRate,
  IFreightRateCreateRequest,
  IFreightRateFilter,
  IFreightRateListResponse,
  IFreightRateResponse,
  IFreightRateUpdateRequest,
  PBGREFR_CONSTANTS,
} from '../interfaces/PBGREFR';
import {
  getCurrentDateRPG,
  getCurrentTimeRPG,
  validateBatchUpdate,
  validateCreateRequest,
  validateFilter,
  validateUpdateRequest,
} from '../models/PBGREFRModel';
import PBGREFRRepository from '../repository/PBGREFRRepository';

/**
 * Service layer for freight rate business logic
 * Replaces RPG program PBGREFR with modern TypeScript patterns
 */
export class PBGREFRService {
  private repository: PBGREFRRepository;
  private programName = 'PBGREFR';

  constructor(pool: Pool) {
    this.repository = new PBGREFRRepository(pool);
  }

  /**
   * Get freight rate by composite key
   * Maps to RPG: CHAIN operation in various subroutines
   *
   * @param key Composite key
   * @returns Freight rate or null
   */
  async getFreightRateByKey(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: number;
  }): Promise<IFreightRateResponse> {
    try {
      const freightRate = await this.repository.findByKey(key);

      if (!freightRate) {
        return {
          success: false,
          message: 'Freight rate not found',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.RECORD_NOT_FOUND,
            details: 'No record found with the specified key',
          },
        };
      }

      return {
        success: true,
        message: 'Freight rate retrieved successfully',
        data: freightRate,
      };
    } catch (error) {
      return this.handleError(error, 'Error retrieving freight rate');
    }
  }

  /**
   * Get paginated list of freight rates with optional filters
   * Maps to RPG: BAIZSF/BBLDSF subroutines (lines 132-346)
   * Implements subfile loading logic with filter support
   *
   * @param filter Filter criteria
   * @param userName User making the request
   * @returns Paginated freight rate list
   */
  async getFreightRates(
    filter: IFreightRateFilter,
    userName: string,
  ): Promise<IFreightRateListResponse> {
    try {
      // Validate filter criteria
      const { error, value } = validateFilter(filter);
      if (error) {
        throw new Error(`Validation error: ${error.message}`);
      }

      // Apply filter and retrieve records (maps to SETLL + READ loop)
      const result = await this.repository.findAll(value!);

      // Check if no data found (maps to RPG indicator 82 and line 182-187)
      if (result.totalRecords === 0) {
        console.log(`No freight rates found for user ${userName} with filters`, filter);
      }

      return result;
    } catch (error) {
      console.error('Error retrieving freight rates:', error);
      throw error;
    }
  }

  /**
   * Create new freight rate record
   * Maps to RPG: SACRRC subroutine (lines 1019-1081)
   * Implements ECADRQ logic (lines 850-875)
   *
   * @param request Create request payload
   * @param userName User creating the record
   * @returns Created freight rate
   */
  async createFreightRate(
    request: IFreightRateCreateRequest,
    userName: string,
  ): Promise<IFreightRateResponse> {
    try {
      // Validate input (maps to RPG DEV1RC lines 599-687)
      const { error, value } = validateCreateRequest(request);
      if (error) {
        return {
          success: false,
          message: 'Validation failed',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.VALUE_REQUIRED,
            details: error.details.map((d) => d.message).join('; '),
          },
        };
      }

      // Check for duplicate primary key (maps to RPG line 1064)
      const exists = await this.repository.exists({
        mprCountry: value!.mprCountry,
        mprRegion: value!.mprRegion,
        mprStateCode: value!.mprStateCode,
        mprZipCode: value!.mprZipCode,
      });

      if (exists) {
        return {
          success: false,
          message: 'Freight rate already exists',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.RECORD_EXISTS,
            details: 'A record with this key already exists (USR4552)',
          },
        };
      }

      // Build freight rate record with audit fields
      // Maps to RPG Set Crt Date/Time logic (lines 1045-1057)
      const currentDate = getCurrentDateRPG();
      const currentTime = getCurrentTimeRPG();

      const freightRate: IFreightRate = {
        mprCountry: value!.mprCountry,
        mprRegion: value!.mprRegion,
        mprStateCode: value!.mprStateCode,
        mprZipCode: value!.mprZipCode,
        mprFobRatePerMile: value!.mprFobRatePerMile,
        mprFobMilesToOmaha: value!.mprFobMilesToOmaha || 0,
        mprFobUnusedDate: value!.mprFobUnusedDate || 0,
        mprFobUnusedText: value!.mprFobUnusedText || '',
        mprFobUnusedText2: value!.mprFobUnusedText2 || '',
        // Record Status = 'A' (Active) - line 1047
        recordStatus: PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE,
        // Set CREATE fields from JOB context - lines 1049-1052
        createDate: currentDate,
        createTime: currentTime,
        createUser: userName,
        createProgram: this.programName,
        // Clear CHANGE fields - lines 1054-1057
        changeDate: 0,
        changeTime: 0,
        changeUser: '',
        changeProgram: '',
      };

      // Write to database (maps to RPG WRITE @ARCPOT line 1073)
      const created = await this.repository.create(freightRate);

      return {
        success: true,
        message: 'Freight rate created successfully',
        data: created,
      };
    } catch (error) {
      return this.handleError(error, 'Error creating freight rate');
    }
  }

  /**
   * Update existing freight rate record
   * Maps to RPG: SBCHRC subroutine (lines 1083-1194)
   * Implements EECHRQ logic (lines 877-905)
   *
   * @param request Update request payload
   * @param userName User updating the record
   * @returns Updated freight rate
   */
  async updateFreightRate(
    request: IFreightRateUpdateRequest,
    userName: string,
  ): Promise<IFreightRateResponse> {
    try {
      // Validate input
      const { error, value } = validateUpdateRequest(request);
      if (error) {
        return {
          success: false,
          message: 'Validation failed',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.VALUE_REQUIRED,
            details: error.details.map((d) => d.message).join('; '),
          },
        };
      }

      // Retrieve existing record for change detection
      // Maps to RPG CHAIN @ARCPOT (line 1102)
      const key = {
        mprCountry: value!.mprCountry,
        mprRegion: value!.mprRegion,
        mprStateCode: value!.mprStateCode,
        mprZipCode: value!.mprZipCode,
      };

      const existing = await this.repository.findByKey(key);

      if (!existing) {
        return {
          success: false,
          message: 'Freight rate not found',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.RECORD_NOT_FOUND,
            details: 'Record no longer exists (Y2U0009)',
          },
        };
      }

      // Check if data has changed (null update check)
      // Maps to RPG lines 1143-1144, 1162-1175
      const hasChanges = this.hasRecordChanged(existing, value!);

      if (!hasChanges) {
        // No changes detected, return existing record
        return {
          success: true,
          message: 'No changes detected',
          data: existing,
        };
      }

      // Set CHANGE fields - maps to RPG lines 1168-1171
      const currentDate = getCurrentDateRPG();
      const currentTime = getCurrentTimeRPG();

      const updates: Partial<IFreightRate> = {
        mprFobRatePerMile: value!.mprFobRatePerMile,
        mprFobMilesToOmaha: value!.mprFobMilesToOmaha,
        mprFobUnusedDate: value!.mprFobUnusedDate,
        mprFobUnusedText: value!.mprFobUnusedText,
        mprFobUnusedText2: value!.mprFobUnusedText2,
        changeDate: currentDate,
        changeTime: currentTime,
        changeUser: userName,
        changeProgram: this.programName,
      };

      // Update record (maps to RPG UPDATE @ARCPOT line 1179)
      const updated = await this.repository.update(key, updates);

      if (!updated) {
        return {
          success: false,
          message: 'Update failed',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.DATABASE_ERROR,
            details: 'Database operation error (Y2U0004)',
          },
        };
      }

      return {
        success: true,
        message: 'Freight rate updated successfully',
        data: updated,
      };
    } catch (error) {
      return this.handleError(error, 'Error updating freight rate');
    }
  }

  /**
   * Delete freight rate record (soft delete)
   * Note: Original RPG program doesn't have delete functionality
   * Included for API completeness
   *
   * @param key Composite key
   * @returns Success indicator
   */
  async deleteFreightRate(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: number;
  }): Promise<IFreightRateResponse> {
    try {
      const deleted = await this.repository.delete(key);

      if (!deleted) {
        return {
          success: false,
          message: 'Freight rate not found',
          error: {
            code: PBGREFR_CONSTANTS.MESSAGES.RECORD_NOT_FOUND,
            details: 'Record not found or already deleted',
          },
        };
      }

      return {
        success: true,
        message: 'Freight rate deleted successfully',
      };
    } catch (error) {
      return this.handleError(error, 'Error deleting freight rate');
    }
  }

  /**
   * Batch update freight rates
   * Maps to CF15 command key calling PBIBPVR program (lines 449-470)
   *
   * @param request Batch update request
   * @param userName User performing the batch update
   * @returns Number of records updated
   */
  async batchUpdateRates(
    request: IBatchRateUpdateRequest,
    userName: string,
  ): Promise<{
    success: boolean;
    message: string;
    recordsUpdated: number;
  }> {
    try {
      // Validate batch update request
      const { error, value } = validateBatchUpdate(request);
      if (error) {
        return {
          success: false,
          message: `Validation failed: ${error.message}`,
          recordsUpdated: 0,
        };
      }

      // Calculate new rate (fixed rate or percentage adjustment)
      const newRate = value!.newRatePerMile;
      if (value!.percentageAdjustment !== undefined) {
        // For percentage adjustment, we'll need to retrieve and update each record individually
        throw new Error('Percentage adjustment not yet implemented - requires record-by-record processing');
      }

      // Execute batch update
      const currentDate = getCurrentDateRPG();
      const currentTime = getCurrentTimeRPG();

      const recordsUpdated = await this.repository.batchUpdateRates(
        value!.filter || {},
        newRate!,
        userName,
        this.programName,
        currentDate,
        currentTime,
      );

      return {
        success: true,
        message: `Batch update completed successfully. ${recordsUpdated} records updated.`,
        recordsUpdated,
      };
    } catch (error) {
      console.error('Error in batch update:', error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Unknown error during batch update',
        recordsUpdated: 0,
      };
    }
  }

  /**
   * Validate freight rate record
   * Maps to RPG: DEV1RC/DFV2RC subroutines (lines 599-695)
   *
   * @param freightRate Freight rate to validate
   * @returns Validation result
   */
  validateFreightRate(freightRate: Partial<IFreightRate>): {
    valid: boolean;
    errors: string[];
  } {
    const errors: string[] = [];

    // MPR Country required (line 643-650)
    if (!freightRate.mprCountry || freightRate.mprCountry.trim() === '') {
      errors.push('MPR Country is required (Y2U0001)');
    }

    // MPR Region required, must be 1-14 (line 652-659)
    if (
      freightRate.mprRegion === undefined ||
      freightRate.mprRegion < PBGREFR_CONSTANTS.MIN_REGION ||
      freightRate.mprRegion > PBGREFR_CONSTANTS.MAX_REGION
    ) {
      errors.push('MPR Region is required and must be between 1 and 14 (Y2U0001)');
    }

    // MPR State Code required (line 661-668)
    if (!freightRate.mprStateCode || freightRate.mprStateCode.trim() === '') {
      errors.push('MPR State Code is required (Y2U0001)');
    }

    // MPR Zip Code required (line 670-677)
    if (freightRate.mprZipCode === undefined || freightRate.mprZipCode < 0) {
      errors.push('MPR Zip Code is required (Y2U0001)');
    }

    // MPR FOB Rate per Mile required, must be > 0 (line 679-686)
    if (freightRate.mprFobRatePerMile === undefined || freightRate.mprFobRatePerMile <= 0) {
      errors.push('MPR FOB Rate per Mile is required and must be greater than 0 (Y2U0001)');
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }

  /**
   * Check if record data has changed
   * Maps to RPG null update check (lines 1162-1175)
   *
   * @param existing Existing record
   * @param updates Proposed updates
   * @returns True if data has changed
   */
  private hasRecordChanged(
    existing: IFreightRate,
    updates: Partial<IFreightRate>,
  ): boolean {
    if (updates.mprFobRatePerMile !== undefined && updates.mprFobRatePerMile !== existing.mprFobRatePerMile) {
      return true;
    }

    if (updates.mprFobMilesToOmaha !== undefined && updates.mprFobMilesToOmaha !== existing.mprFobMilesToOmaha) {
      return true;
    }

    if (updates.mprFobUnusedDate !== undefined && updates.mprFobUnusedDate !== existing.mprFobUnusedDate) {
      return true;
    }

    if (updates.mprFobUnusedText !== undefined && updates.mprFobUnusedText !== existing.mprFobUnusedText) {
      return true;
    }

    if (updates.mprFobUnusedText2 !== undefined && updates.mprFobUnusedText2 !== existing.mprFobUnusedText2) {
      return true;
    }

    return false;
  }

  /**
   * Handle service errors and format response
   *
   * @param error Error object
   * @param message Error message
   * @returns Error response
   */
  private handleError(error: unknown, message: string): IFreightRateResponse {
    console.error(message, error);

    return {
      success: false,
      message,
      error: {
        code: PBGREFR_CONSTANTS.MESSAGES.DATABASE_ERROR,
        details: error instanceof Error ? error.message : 'Unknown error occurred',
      },
    };
  }
}

export default PBGREFRService;
