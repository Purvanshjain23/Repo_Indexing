/**
 * PBGREFR - MPR FOB Omaha Rate Table Data Models
 *
 * Data models with Joi validation schemas
 * Extracted from legacy IBM i RPG/DDS validation rules
 * Original files: PBGREFR_RPG.txt, PBGREFR_DDS.txt
 */

import Joi from 'joi';
import {
  IBatchRateUpdateRequest,
  IFreightRate,
  IFreightRateCreateRequest,
  IFreightRateFilter,
  IFreightRateUpdateRequest,
  PBGREFR_CONSTANTS,
} from '../interfaces/PBGREFR';

/**
 * FreightRateModel - Core data model for freight rates
 * Maps to RPG data structure @1DBRC and database file PBARCPL0
 */
export class FreightRateModel implements IFreightRate {
  // Primary Key Fields
  mprCountry: string;
  mprRegion: number;
  mprStateCode: string;
  mprZipCode: number;

  // Rate Fields
  mprFobRatePerMile: number;
  mprFobMilesToOmaha: number;

  // Unused Fields (reserved)
  mprFobUnusedDate?: number;
  mprFobUnusedText?: string;
  mprFobUnusedText2?: string;

  // Audit Fields
  recordStatus: string;
  createDate: number;
  createTime: number;
  createUser: string;
  createProgram: string;
  changeDate: number;
  changeTime: number;
  changeUser: string;
  changeProgram: string;

  constructor(data: IFreightRate) {
    this.mprCountry = data.mprCountry;
    this.mprRegion = data.mprRegion;
    this.mprStateCode = data.mprStateCode;
    this.mprZipCode = data.mprZipCode;
    this.mprFobRatePerMile = data.mprFobRatePerMile;
    this.mprFobMilesToOmaha = data.mprFobMilesToOmaha;
    this.mprFobUnusedDate = data.mprFobUnusedDate;
    this.mprFobUnusedText = data.mprFobUnusedText;
    this.mprFobUnusedText2 = data.mprFobUnusedText2;
    this.recordStatus = data.recordStatus;
    this.createDate = data.createDate;
    this.createTime = data.createTime;
    this.createUser = data.createUser;
    this.createProgram = data.createProgram;
    this.changeDate = data.changeDate;
    this.changeTime = data.changeTime;
    this.changeUser = data.changeUser;
    this.changeProgram = data.changeProgram;
  }

  /**
   * Generate composite key for record identification
   * Maps to RPG KLIST (KPOS, KLCRSA, KLCHSB)
   */
  getCompositeKey(): string {
    return `${this.mprCountry}-${this.mprRegion}-${this.mprStateCode}-${this.mprZipCode}`;
  }

  /**
   * Convert model to plain object
   */
  toJSON(): IFreightRate {
    return {
      mprCountry: this.mprCountry,
      mprRegion: this.mprRegion,
      mprStateCode: this.mprStateCode,
      mprZipCode: this.mprZipCode,
      mprFobRatePerMile: this.mprFobRatePerMile,
      mprFobMilesToOmaha: this.mprFobMilesToOmaha,
      mprFobUnusedDate: this.mprFobUnusedDate,
      mprFobUnusedText: this.mprFobUnusedText,
      mprFobUnusedText2: this.mprFobUnusedText2,
      recordStatus: this.recordStatus,
      createDate: this.createDate,
      createTime: this.createTime,
      createUser: this.createUser,
      createProgram: this.createProgram,
      changeDate: this.changeDate,
      changeTime: this.changeTime,
      changeUser: this.changeUser,
      changeProgram: this.changeProgram,
    };
  }

  /**
   * Check if record is active
   * Maps to RPG ARVSST = 'A'
   */
  isActive(): boolean {
    return this.recordStatus === PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE;
  }
}

/**
 * Joi Validation Schemas
 * Extracted from DDS field definitions and RPG validation logic
 */

/**
 * Schema for MPR Country field
 * DDS: #1PZAA//#2PZAA - 6 chars, CHECK(FE)
 * RPG: Line 643-650, 589 - Required, non-blank
 */
export const mprCountrySchema = Joi.string()
  .trim()
  .max(6)
  .required()
  .messages({
    'string.empty': 'MPR Country is required (Y2U0001)',
    'string.max': 'MPR Country must not exceed 6 characters',
    'any.required': 'MPR Country is required (Y2U0001)',
  });

/**
 * Schema for MPR Region field
 * DDS: #1W5NY//#2W5NY - 2 digits, RANGE(1 14), CHECK(RB AB), EDTCDE(4)
 * RPG: Line 652-659, 590 - Required, must be 1-14
 */
export const mprRegionSchema = Joi.number()
  .integer()
  .min(PBGREFR_CONSTANTS.MIN_REGION)
  .max(PBGREFR_CONSTANTS.MAX_REGION)
  .required()
  .messages({
    'number.base': 'MPR Region must be a number',
    'number.integer': 'MPR Region must be an integer',
    'number.min': 'MPR Region must be between 1 and 14',
    'number.max': 'MPR Region must be between 1 and 14',
    'any.required': 'MPR Region is required (Y2U0001)',
  });

/**
 * Schema for MPR State Code field
 * DDS: #1P0AA//#2P0AA - 2 chars, CHECK(FE)
 * RPG: Line 661-668, 591 - Required, non-blank
 */
export const mprStateCodeSchema = Joi.string()
  .trim()
  .length(2)
  .uppercase()
  .required()
  .messages({
    'string.empty': 'MPR State Code is required (Y2U0001)',
    'string.length': 'MPR State Code must be exactly 2 characters',
    'any.required': 'MPR State Code is required (Y2U0001)',
  });

/**
 * Schema for MPR Zip Code field
 * DDS: #1W6NY//#2W6NY - 5 digits, CHECK(RB), EDTCDE(4)
 * RPG: Line 670-677, 592 - Required, numeric
 */
export const mprZipCodeSchema = Joi.number()
  .integer()
  .min(0)
  .max(99999)
  .required()
  .messages({
    'number.base': 'MPR Zip Code must be a number',
    'number.integer': 'MPR Zip Code must be an integer',
    'number.min': 'MPR Zip Code must be a valid 5-digit code',
    'number.max': 'MPR Zip Code must be a valid 5-digit code',
    'any.required': 'MPR Zip Code is required (Y2U0001)',
  });

/**
 * Schema for MPR FOB Rate per Mile field
 * DDS: #1HSPR//#CHSPR - 9.4 decimal, COMP(GT 0), CHECK(RB AB), EDTCDE(4)
 * RPG: Line 679-686, 593 - Required, must be > 0
 */
export const mprFobRatePerMileSchema = Joi.number()
  .precision(4)
  .greater(0)
  .required()
  .messages({
    'number.base': 'MPR FOB Rate per Mile must be a number',
    'number.greater': 'MPR FOB Rate per Mile must be greater than 0',
    'any.required': 'MPR FOB Rate per Mile is required (Y2U0001)',
  });

/**
 * Schema for MPR FOB Miles to Omaha field
 * DDS: #1W7NY//#CW7NY - 7 digits, CHECK(RB), EDTCDE(4)
 * RPG: Line 594 - Optional, numeric
 */
export const mprFobMilesToOmahaSchema = Joi.number()
  .integer()
  .min(0)
  .max(9999999)
  .optional()
  .allow(0)
  .messages({
    'number.base': 'MPR FOB Miles to Omaha must be a number',
    'number.integer': 'MPR FOB Miles to Omaha must be an integer',
    'number.min': 'MPR FOB Miles to Omaha must be non-negative',
    'number.max': 'MPR FOB Miles to Omaha must not exceed 9999999',
  });

/**
 * Validation schema for creating a new freight rate
 * Maps to RPG subroutine SACRRC (lines 1019-1081)
 */
export const createFreightRateSchema = Joi.object<IFreightRateCreateRequest>({
  mprCountry: mprCountrySchema,
  mprRegion: mprRegionSchema,
  mprStateCode: mprStateCodeSchema,
  mprZipCode: mprZipCodeSchema,
  mprFobRatePerMile: mprFobRatePerMileSchema,
  mprFobMilesToOmaha: mprFobMilesToOmahaSchema,
  mprFobUnusedDate: Joi.number().integer().optional(),
  mprFobUnusedText: Joi.string().max(25).optional(),
  mprFobUnusedText2: Joi.string().max(10).optional(),
}).options({ abortEarly: false, stripUnknown: true });

/**
 * Validation schema for updating an existing freight rate
 * Maps to RPG subroutine SBCHRC (lines 1083-1194)
 */
export const updateFreightRateSchema = Joi.object<IFreightRateUpdateRequest>({
  // Key fields (required for identification, but immutable)
  mprCountry: mprCountrySchema,
  mprRegion: mprRegionSchema,
  mprStateCode: mprStateCodeSchema,
  mprZipCode: mprZipCodeSchema,
  // Updatable fields
  mprFobRatePerMile: mprFobRatePerMileSchema.optional(),
  mprFobMilesToOmaha: mprFobMilesToOmahaSchema,
  mprFobUnusedDate: Joi.number().integer().optional(),
  mprFobUnusedText: Joi.string().max(25).optional(),
  mprFobUnusedText2: Joi.string().max(10).optional(),
}).options({ abortEarly: false, stripUnknown: true });

/**
 * Validation schema for filter criteria
 * Maps to DDS subfile control fields #2PZAA, #2W5NY, #2P0AA, #2W6NY
 */
export const filterSchema = Joi.object<IFreightRateFilter>({
  mprCountry: Joi.string().trim().max(6).optional(),
  mprRegion: Joi.number()
    .integer()
    .min(PBGREFR_CONSTANTS.MIN_REGION)
    .max(PBGREFR_CONSTANTS.MAX_REGION)
    .optional(),
  mprStateCode: Joi.string().trim().length(2).uppercase().optional(),
  mprZipCode: Joi.number().integer().min(0).max(99999).optional(),
  mprFobRatePerMile: Joi.number().precision(4).greater(0).optional(),
  mprFobMilesToOmaha: Joi.number().integer().min(0).max(9999999).optional(),
  // Pagination
  page: Joi.number().integer().min(0).optional().default(0),
  pageSize: Joi.number()
    .integer()
    .min(1)
    .max(100)
    .optional()
    .default(PBGREFR_CONSTANTS.SUBFILE_PAGE_SIZE),
  sortBy: Joi.string().optional(),
  sortDirection: Joi.string().valid('ASC', 'DESC').optional().default('ASC'),
}).options({ stripUnknown: true });

/**
 * Validation schema for batch rate update
 * Maps to CF15 command key calling PBIBPVR program
 */
export const batchRateUpdateSchema = Joi.object<IBatchRateUpdateRequest>({
  targetRegion: Joi.number()
    .integer()
    .min(PBGREFR_CONSTANTS.MIN_REGION)
    .max(PBGREFR_CONSTANTS.MAX_REGION)
    .optional(),
  newRatePerMile: Joi.number().precision(4).greater(0).when('percentageAdjustment', {
    is: Joi.exist(),
    then: Joi.optional(),
    otherwise: Joi.required(),
  }),
  percentageAdjustment: Joi.number().min(-100).max(1000).optional(),
  filter: filterSchema.optional(),
})
  .or('newRatePerMile', 'percentageAdjustment')
  .options({ abortEarly: false, stripUnknown: true })
  .messages({
    'object.missing': 'Either newRatePerMile or percentageAdjustment must be provided',
  });

/**
 * Validation schema for record status
 * Maps to RPG field ARVSST
 */
export const recordStatusSchema = Joi.string()
  .valid(...Object.values(PBGREFR_CONSTANTS.RECORD_STATUS))
  .required()
  .messages({
    'any.only': 'Record status must be A (Active), I (Inactive), or D (Deleted)',
    'any.required': 'Record status is required',
  });

/**
 * Helper function to validate create request
 */
export function validateCreateRequest(
  data: IFreightRateCreateRequest,
): Joi.ValidationResult<IFreightRateCreateRequest> {
  return createFreightRateSchema.validate(data);
}

/**
 * Helper function to validate update request
 */
export function validateUpdateRequest(
  data: IFreightRateUpdateRequest,
): Joi.ValidationResult<IFreightRateUpdateRequest> {
  return updateFreightRateSchema.validate(data);
}

/**
 * Helper function to validate filter criteria
 */
export function validateFilter(data: IFreightRateFilter): Joi.ValidationResult<IFreightRateFilter> {
  return filterSchema.validate(data);
}

/**
 * Helper function to validate batch update request
 */
export function validateBatchUpdate(
  data: IBatchRateUpdateRequest,
): Joi.ValidationResult<IBatchRateUpdateRequest> {
  return batchRateUpdateSchema.validate(data);
}

/**
 * Utility function to generate current date in RPG CYYMMDD format
 * Maps to RPG ##JDT (Job Date)
 */
export function getCurrentDateRPG(): number {
  const now = new Date();
  const century = Math.floor(now.getFullYear() / 100) - 19; // 0 for 1900s, 1 for 2000s
  const year = now.getFullYear() % 100;
  const month = now.getMonth() + 1;
  const day = now.getDate();
  return century * 1000000 + year * 10000 + month * 100 + day;
}

/**
 * Utility function to generate current time in RPG HHMMSS format
 * Maps to RPG ##JTM (Job Time)
 */
export function getCurrentTimeRPG(): number {
  const now = new Date();
  const hours = now.getHours();
  const minutes = now.getMinutes();
  const seconds = now.getSeconds();
  return hours * 10000 + minutes * 100 + seconds;
}

/**
 * Utility function to format RPG date to JavaScript Date
 */
export function parseRPGDate(rpgDate: number): Date | null {
  if (!rpgDate || rpgDate === 0) return null;

  const dateStr = rpgDate.toString().padStart(7, '0');
  const century = parseInt(dateStr.substring(0, 1), 10);
  const year = parseInt(dateStr.substring(1, 3), 10);
  const month = parseInt(dateStr.substring(3, 5), 10);
  const day = parseInt(dateStr.substring(5, 7), 10);

  const fullYear = (century + 19) * 100 + year;
  return new Date(fullYear, month - 1, day);
}

/**
 * Utility function to format RPG time to readable string
 */
export function parseRPGTime(rpgTime: number): string {
  if (!rpgTime || rpgTime === 0) return '00:00:00';

  const timeStr = rpgTime.toString().padStart(6, '0');
  const hours = timeStr.substring(0, 2);
  const minutes = timeStr.substring(2, 4);
  const seconds = timeStr.substring(4, 6);

  return `${hours}:${minutes}:${seconds}`;
}
