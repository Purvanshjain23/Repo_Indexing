/**
 * PBGREFR - MPR FOB Omaha Rate Table Interfaces
 *
 * TypeScript type definitions extracted from legacy IBM i RPG/DDS
 * Original files: PBGREFR_RPG.txt, PBGREFR_DDS.txt, PBGREFR_AD.txt
 *
 * Purpose: Manages freight rates for MPR FOB Omaha shipping
 * Legacy Database: PBARCPL0/PBARCPL1 (MPR FOB Omaha Rate Table)
 */

/**
 * Core freight rate record interface
 * Maps to RPG data structure @1DBRC and DDS subfile record #SFLRCD
 */
export interface IFreightRate {
  // Primary Key Fields
  /** MPR Country (6 chars) - ARPZAA in RPG */
  mprCountry: string;

  /** MPR Region (1-14) - ARW5NY in RPG */
  mprRegion: number;

  /** MPR State Code (2 chars) - ARP0AA in RPG */
  mprStateCode: string;

  /** MPR Zip Code (5 digits) - ARW6NY in RPG */
  mprZipCode: number;

  // Rate Fields
  /** MPR FOB Rate per Mile (decimal 9,4) - ARHSPR in RPG */
  mprFobRatePerMile: number;

  /** MPR FOB Miles to Omaha (7 digits) - ARW7NY in RPG */
  mprFobMilesToOmaha: number;

  // Unused Fields (reserved for future use)
  /** MPR FOB Unused Date (7 digits) - ARU2DT in RPG */
  mprFobUnusedDate?: number;

  /** MPR FOB Unused Text (25 chars) - ARZWT1 in RPG */
  mprFobUnusedText?: string;

  /** MPR FOB Unused Text 2 (10 chars) - ARZXT1 in RPG */
  mprFobUnusedText2?: string;

  // Audit Fields
  /** Record Status ('A' = Active) - ARVSST in RPG */
  recordStatus: string;

  /** Create Date (CYYMMDD format) - ARMJDT in RPG */
  createDate: number;

  /** Create Time (HHMMSS format) - ARBETM in RPG */
  createTime: number;

  /** Create User (10 chars) - ARCCVN in RPG */
  createUser: string;

  /** Create Program (10 chars) - ARCDVN in RPG */
  createProgram: string;

  /** Change Date (CYYMMDD format) - ARMKDT in RPG */
  changeDate: number;

  /** Change Time (HHMMSS format) - ARBFTM in RPG */
  changeTime: number;

  /** Change User (10 chars) - ARCEVN in RPG */
  changeUser: string;

  /** Change Program (10 chars) - ARCFVN in RPG */
  changeProgram: string;
}

/**
 * Filter criteria for searching freight rates
 * Maps to DDS subfile control fields #2PZAA, #2W5NY, #2P0AA, #2W6NY, #CHSPR
 */
export interface IFreightRateFilter {
  /** Filter by MPR Country */
  mprCountry?: string;

  /** Filter by MPR Region (1-14) */
  mprRegion?: number;

  /** Filter by MPR State Code */
  mprStateCode?: string;

  /** Filter by MPR Zip Code */
  mprZipCode?: number;

  /** Filter by MPR FOB Rate per Mile */
  mprFobRatePerMile?: number;

  /** Filter by MPR FOB Miles to Omaha */
  mprFobMilesToOmaha?: number;

  // Pagination
  /** Page number (0-based) */
  page?: number;

  /** Page size (default: 13 to match subfile) */
  pageSize?: number;

  /** Sort field */
  sortBy?: string;

  /** Sort direction */
  sortDirection?: 'ASC' | 'DESC';
}

/**
 * Paginated response for freight rate lists
 * Replaces RPG subfile pagination logic
 */
export interface IFreightRateListResponse {
  /** Array of freight rate records */
  data: IFreightRate[];

  /** Total number of records matching filter */
  totalRecords: number;

  /** Current page number */
  currentPage: number;

  /** Page size */
  pageSize: number;

  /** Total number of pages */
  totalPages: number;

  /** Has more records (replaces RPG indicator 82) */
  hasMore: boolean;
}

/**
 * Request payload for creating new freight rate
 * Maps to RPG subroutine SACRRC (Create record)
 */
export interface IFreightRateCreateRequest {
  // Required Fields
  mprCountry: string;
  mprRegion: number;
  mprStateCode: string;
  mprZipCode: number;
  mprFobRatePerMile: number;

  // Optional Fields
  mprFobMilesToOmaha?: number;
  mprFobUnusedDate?: number;
  mprFobUnusedText?: string;
  mprFobUnusedText2?: string;
}

/**
 * Request payload for updating existing freight rate
 * Maps to RPG subroutine SBCHRC (Change record)
 */
export interface IFreightRateUpdateRequest {
  // Key Fields (immutable)
  mprCountry: string;
  mprRegion: number;
  mprStateCode: string;
  mprZipCode: number;

  // Updatable Fields
  mprFobRatePerMile?: number;
  mprFobMilesToOmaha?: number;
  mprFobUnusedDate?: number;
  mprFobUnusedText?: string;
  mprFobUnusedText2?: string;
}

/**
 * Request payload for batch rate update
 * Maps to CF15 command key calling PBIBPVR program
 */
export interface IBatchRateUpdateRequest {
  /** Target region for batch update */
  targetRegion?: number;

  /** New rate to apply */
  newRatePerMile: number;

  /** Optional filter criteria */
  filter?: IFreightRateFilter;

  /** Percentage adjustment (alternative to fixed rate) */
  percentageAdjustment?: number;
}

/**
 * Request payload for editing region details
 * Maps to selection '2' calling PBIDPVR program
 */
export interface IRegionEditRequest {
  mprCountry: string;
  mprRegion: number;
  mprStateCode: string;
  mprZipCode: number;
  mprFobRatePerMile: number;
  mprFobMilesToOmaha: number;
}

/**
 * Response for single freight rate operation
 */
export interface IFreightRateResponse {
  /** Success indicator */
  success: boolean;

  /** Response message */
  message: string;

  /** Freight rate data (if applicable) */
  data?: IFreightRate;

  /** Error details (if applicable) */
  error?: {
    code: string;
    details?: string;
  };
}

/**
 * Validation schema types for Joi
 */
export interface IFreightRateValidation {
  /** Country must be non-blank, max 6 chars */
  mprCountry: {
    required: boolean;
    maxLength: number;
  };

  /** Region must be 1-14 */
  mprRegion: {
    required: boolean;
    min: number;
    max: number;
  };

  /** State code must be non-blank, 2 chars */
  mprStateCode: {
    required: boolean;
    length: number;
  };

  /** Zip code must be 5 digits */
  mprZipCode: {
    required: boolean;
    min: number;
    max: number;
  };

  /** FOB rate must be > 0, max 9 digits with 4 decimals */
  mprFobRatePerMile: {
    required: boolean;
    min: number;
    precision: number;
    scale: number;
  };
}

/**
 * Constants for PBGREFR module
 * Extracted from RPG constants and DDS specifications
 */
export const PBGREFR_CONSTANTS = {
  /** Subfile page size (SFLPAG in DDS) */
  SUBFILE_PAGE_SIZE: 13,

  /** Subfile maximum size (SFLSIZ in DDS) */
  SUBFILE_MAX_SIZE: 14,

  /** Valid region range */
  MIN_REGION: 1,
  MAX_REGION: 14,

  /** Record status values */
  RECORD_STATUS: {
    ACTIVE: 'A',
    INACTIVE: 'I',
    DELETED: 'D',
  },

  /** Command keys (function keys in DDS) */
  COMMAND_KEYS: {
    EXIT: 'F3',
    PROMPT: 'F4',
    MODE_SWITCH: 'F9',
    EDIT_CONTROL: 'F10',
    BATCH_UPDATE: 'F15',
  },

  /** Operation modes */
  MODES: {
    ADD: 'ADD',
    CHANGE: 'CHG',
  },

  /** Subfile selection options */
  SELECTION: {
    EDIT_REGION: '2',
  },

  /** Message IDs (from RPG) */
  MESSAGES: {
    NO_DATA_TO_DISPLAY: 'Y2U0008',
    VALUE_REQUIRED: 'Y2U0001',
    RECORD_EXISTS: 'USR4552',
    RECORD_NOT_FOUND: 'Y2U0009',
    UPDATE_NOT_ACCEPTED: 'Y2U0007',
    DATABASE_ERROR: 'Y2U0004',
  },
} as const;

/**
 * Type for operation modes
 */
export type OperationMode = typeof PBGREFR_CONSTANTS.MODES[keyof typeof PBGREFR_CONSTANTS.MODES];

/**
 * Type for record status
 */
export type RecordStatus = typeof PBGREFR_CONSTANTS.RECORD_STATUS[keyof typeof PBGREFR_CONSTANTS.RECORD_STATUS];
