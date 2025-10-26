/**
 * Database Interface - Abstract database operations
 *
 * This interface provides a common abstraction for both PostgreSQL
 * and mock database implementations, enabling seamless switching
 * between development/demo and production environments.
 */

import { IFreightRate, IFreightRateFilter } from './PBGREFR';

/**
 * Abstract database interface for freight rate operations
 */
export interface IFreightRateDatabase {
  /**
   * Find all freight rates with optional filtering and pagination
   */
  findAll(filter?: IFreightRateFilter): Promise<{
    data: IFreightRate[];
    total: number;
    page: number;
    pageSize: number;
  }>;

  /**
   * Find single freight rate by composite key
   */
  findByKey(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string,
  ): Promise<IFreightRate | null>;

  /**
   * Create new freight rate
   */
  create(freightRate: Omit<IFreightRate, 'id'>): Promise<IFreightRate>;

  /**
   * Update existing freight rate
   */
  update(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string,
    updates: Partial<IFreightRate>,
  ): Promise<IFreightRate | null>;

  /**
   * Delete freight rate by composite key
   */
  delete(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string,
  ): Promise<boolean>;

  /**
   * Batch update multiple freight rates
   */
  batchUpdate(updates: Array<{
    key: {
      country: string;
      region: number;
      stateCode: string;
      zipCode: string;
    };
    data: Partial<IFreightRate>;
  }>): Promise<IFreightRate[]>;

  /**
   * Get statistics about freight rates
   */
  getStatistics(): Promise<{
    totalRecords: number;
    countryBreakdown: Record<string, number>;
    regionBreakdown: Record<number, number>;
    averageRate: number;
    rateRange: { min: number; max: number };
  }>;
}