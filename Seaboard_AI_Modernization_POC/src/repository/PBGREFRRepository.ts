/**
 * PBGREFR - MPR FOB Omaha Rate Table Repository
 *
 * Data access layer converting RPG file I/O to PostgreSQL operations
 * Maps legacy database file PBARCPL0/PBARCPL1 to modern SQL
 *
 * RPG File Operations → PostgreSQL Mapping:
 * - SETLL (Set Lower Limit) → SELECT with WHERE and ORDER BY
 * - READ (Read next record) → SELECT with OFFSET/LIMIT
 * - CHAIN (Random read by key) → SELECT with WHERE on PK
 * - WRITE (Add record) → INSERT
 * - UPDATE (Update record) → UPDATE with WHERE
 * - DELETE (Delete record) → DELETE or UPDATE status
 */

import { Pool, PoolClient, QueryResult } from 'pg';
import {
  IFreightRate,
  IFreightRateFilter,
  IFreightRateListResponse,
  PBGREFR_CONSTANTS,
} from '../interfaces/PBGREFR';

/**
 * PBGREFRRepository - Data access layer for freight rates
 * Replaces RPG file operations with PostgreSQL queries
 */
export class PBGREFRRepository {
  private pool: Pool;
  private tableName = 'pbarcpl0'; // MPR FOB Omaha Rate Table

  constructor(pool: Pool) {
    this.pool = pool;
  }

  /**
   * Find freight rate by composite primary key
   * Maps to RPG: CHAIN operation (lines 539, 1102, 1358, 1401)
   *
   * @param key Composite key components
   * @returns Freight rate record or null
   */
  async findByKey(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: number;
  }): Promise<IFreightRate | null> {
    const query = `
      SELECT
        mpr_country,
        mpr_region,
        mpr_state_code,
        mpr_zip_code,
        mpr_fob_rate_per_mile,
        mpr_fob_miles_to_omaha,
        mpr_fob_unused_date,
        mpr_fob_unused_text,
        mpr_fob_unused_text2,
        record_status,
        create_date,
        create_time,
        create_user,
        create_program,
        change_date,
        change_time,
        change_user,
        change_program
      FROM ${this.tableName}
      WHERE mpr_country = $1
        AND mpr_region = $2
        AND mpr_state_code = $3
        AND mpr_zip_code = $4
        AND record_status = $5
    `;

    const values = [
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
      PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE,
    ];

    const result: QueryResult = await this.pool.query(query, values);

    if (result.rows.length === 0) {
      return null;
    }

    return this.mapRowToModel(result.rows[0]);
  }

  /**
   * Check if freight rate exists (for duplicate key validation)
   * Maps to RPG: SETLL operation for duplicate check (line 1064)
   *
   * @param key Composite key components
   * @returns True if record exists
   */
  async exists(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: number;
  }): Promise<boolean> {
    const query = `
      SELECT 1
      FROM ${this.tableName}
      WHERE mpr_country = $1
        AND mpr_region = $2
        AND mpr_state_code = $3
        AND mpr_zip_code = $4
        AND record_status = $5
      LIMIT 1
    `;

    const values = [
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
      PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE,
    ];

    const result: QueryResult = await this.pool.query(query, values);
    return result.rows.length > 0;
  }

  /**
   * Find freight rates with filters and pagination
   * Maps to RPG: SETLL + READ loop (lines 164-328)
   *
   * @param filter Filter criteria
   * @returns Paginated list of freight rates
   */
  async findAll(filter: IFreightRateFilter): Promise<IFreightRateListResponse> {
    const {
      mprCountry,
      mprRegion,
      mprStateCode,
      mprZipCode,
      mprFobRatePerMile,
      mprFobMilesToOmaha,
      page = 0,
      pageSize = PBGREFR_CONSTANTS.SUBFILE_PAGE_SIZE,
      sortBy = 'mpr_country, mpr_region, mpr_state_code, mpr_zip_code',
      sortDirection = 'ASC',
    } = filter;

    // Build WHERE clause dynamically (maps to RPG filter logic lines 243-310)
    const conditions: string[] = ['record_status = $1'];
    const values: any[] = [PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE];
    let paramIndex = 2;

    if (mprCountry) {
      conditions.push(`mpr_country = $${paramIndex}`);
      values.push(mprCountry);
      paramIndex++;
    }

    if (mprRegion !== undefined) {
      conditions.push(`mpr_region = $${paramIndex}`);
      values.push(mprRegion);
      paramIndex++;
    }

    if (mprStateCode) {
      conditions.push(`mpr_state_code = $${paramIndex}`);
      values.push(mprStateCode);
      paramIndex++;
    }

    if (mprZipCode !== undefined) {
      conditions.push(`mpr_zip_code = $${paramIndex}`);
      values.push(mprZipCode);
      paramIndex++;
    }

    if (mprFobRatePerMile !== undefined) {
      conditions.push(`mpr_fob_rate_per_mile = $${paramIndex}`);
      values.push(mprFobRatePerMile);
      paramIndex++;
    }

    if (mprFobMilesToOmaha !== undefined) {
      conditions.push(`mpr_fob_miles_to_omaha = $${paramIndex}`);
      values.push(mprFobMilesToOmaha);
      paramIndex++;
    }

    const whereClause = conditions.join(' AND ');

    // Count total records
    const countQuery = `SELECT COUNT(*) as total FROM ${this.tableName} WHERE ${whereClause}`;
    const countResult: QueryResult = await this.pool.query(countQuery, values);
    const totalRecords = parseInt(countResult.rows[0].total, 10);

    // Fetch paginated data
    const offset = page * pageSize;
    const dataQuery = `
      SELECT
        mpr_country,
        mpr_region,
        mpr_state_code,
        mpr_zip_code,
        mpr_fob_rate_per_mile,
        mpr_fob_miles_to_omaha,
        mpr_fob_unused_date,
        mpr_fob_unused_text,
        mpr_fob_unused_text2,
        record_status,
        create_date,
        create_time,
        create_user,
        create_program,
        change_date,
        change_time,
        change_user,
        change_program
      FROM ${this.tableName}
      WHERE ${whereClause}
      ORDER BY ${sortBy} ${sortDirection}
      LIMIT $${paramIndex} OFFSET $${paramIndex + 1}
    `;

    const dataValues = [...values, pageSize, offset];
    const dataResult: QueryResult = await this.pool.query(dataQuery, dataValues);

    const data = dataResult.rows.map((row) => this.mapRowToModel(row));
    const totalPages = Math.ceil(totalRecords / pageSize);

    return {
      data,
      totalRecords,
      currentPage: page,
      pageSize,
      totalPages,
      hasMore: page < totalPages - 1,
    };
  }

  /**
   * Create new freight rate record
   * Maps to RPG: WRITE @ARCPOT operation (line 1073)
   * Maps to RPG subroutine: SACRRC (lines 1019-1081)
   *
   * @param freightRate Freight rate data
   * @returns Created freight rate record
   */
  async create(freightRate: IFreightRate): Promise<IFreightRate> {
    const query = `
      INSERT INTO ${this.tableName} (
        mpr_country,
        mpr_region,
        mpr_state_code,
        mpr_zip_code,
        mpr_fob_rate_per_mile,
        mpr_fob_miles_to_omaha,
        mpr_fob_unused_date,
        mpr_fob_unused_text,
        mpr_fob_unused_text2,
        record_status,
        create_date,
        create_time,
        create_user,
        create_program,
        change_date,
        change_time,
        change_user,
        change_program
      ) VALUES (
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10,
        $11, $12, $13, $14, $15, $16, $17, $18
      )
      RETURNING *
    `;

    const values = [
      freightRate.mprCountry,
      freightRate.mprRegion,
      freightRate.mprStateCode,
      freightRate.mprZipCode,
      freightRate.mprFobRatePerMile,
      freightRate.mprFobMilesToOmaha || 0,
      freightRate.mprFobUnusedDate || 0,
      freightRate.mprFobUnusedText || '',
      freightRate.mprFobUnusedText2 || '',
      freightRate.recordStatus,
      freightRate.createDate,
      freightRate.createTime,
      freightRate.createUser,
      freightRate.createProgram,
      freightRate.changeDate || 0,
      freightRate.changeTime || 0,
      freightRate.changeUser || '',
      freightRate.changeProgram || '',
    ];

    const result: QueryResult = await this.pool.query(query, values);
    return this.mapRowToModel(result.rows[0]);
  }

  /**
   * Update existing freight rate record
   * Maps to RPG: UPDATE @ARCPOT operation (line 1179)
   * Maps to RPG subroutine: SBCHRC (lines 1083-1194)
   *
   * @param key Composite key
   * @param freightRate Updated freight rate data
   * @returns Updated freight rate record
   */
  async update(
    key: {
      mprCountry: string;
      mprRegion: number;
      mprStateCode: string;
      mprZipCode: number;
    },
    freightRate: Partial<IFreightRate>,
  ): Promise<IFreightRate | null> {
    const query = `
      UPDATE ${this.tableName}
      SET
        mpr_fob_rate_per_mile = COALESCE($5, mpr_fob_rate_per_mile),
        mpr_fob_miles_to_omaha = COALESCE($6, mpr_fob_miles_to_omaha),
        mpr_fob_unused_date = COALESCE($7, mpr_fob_unused_date),
        mpr_fob_unused_text = COALESCE($8, mpr_fob_unused_text),
        mpr_fob_unused_text2 = COALESCE($9, mpr_fob_unused_text2),
        change_date = $10,
        change_time = $11,
        change_user = $12,
        change_program = $13
      WHERE mpr_country = $1
        AND mpr_region = $2
        AND mpr_state_code = $3
        AND mpr_zip_code = $4
        AND record_status = $14
      RETURNING *
    `;

    const values = [
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
      freightRate.mprFobRatePerMile,
      freightRate.mprFobMilesToOmaha,
      freightRate.mprFobUnusedDate,
      freightRate.mprFobUnusedText,
      freightRate.mprFobUnusedText2,
      freightRate.changeDate,
      freightRate.changeTime,
      freightRate.changeUser,
      freightRate.changeProgram,
      PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE,
    ];

    const result: QueryResult = await this.pool.query(query, values);

    if (result.rows.length === 0) {
      return null;
    }

    return this.mapRowToModel(result.rows[0]);
  }

  /**
   * Soft delete freight rate record (update status)
   * Note: RPG program doesn't have explicit delete, but we include for completeness
   *
   * @param key Composite key
   * @returns True if deleted successfully
   */
  async delete(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: number;
  }): Promise<boolean> {
    const query = `
      UPDATE ${this.tableName}
      SET record_status = $5
      WHERE mpr_country = $1
        AND mpr_region = $2
        AND mpr_state_code = $3
        AND mpr_zip_code = $4
        AND record_status = $6
    `;

    const values = [
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
      PBGREFR_CONSTANTS.RECORD_STATUS.DELETED,
      PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE,
    ];

    const result: QueryResult = await this.pool.query(query, values);
    return result.rowCount !== null && result.rowCount > 0;
  }

  /**
   * Batch update freight rates based on filter criteria
   * Maps to CF15 command key calling PBIBPVR program (line 451)
   *
   * @param filter Filter criteria
   * @param newRatePerMile New rate to apply
   * @param changeUser User making the change
   * @param changeProgram Program making the change
   * @param changeDate Change date
   * @param changeTime Change time
   * @returns Number of records updated
   */
  async batchUpdateRates(
    filter: IFreightRateFilter,
    newRatePerMile: number,
    changeUser: string,
    changeProgram: string,
    changeDate: number,
    changeTime: number,
  ): Promise<number> {
    const conditions: string[] = ['record_status = $1'];
    const values: any[] = [PBGREFR_CONSTANTS.RECORD_STATUS.ACTIVE];
    let paramIndex = 2;

    // Build WHERE clause from filter
    if (filter.mprCountry) {
      conditions.push(`mpr_country = $${paramIndex}`);
      values.push(filter.mprCountry);
      paramIndex++;
    }

    if (filter.mprRegion !== undefined) {
      conditions.push(`mpr_region = $${paramIndex}`);
      values.push(filter.mprRegion);
      paramIndex++;
    }

    if (filter.mprStateCode) {
      conditions.push(`mpr_state_code = $${paramIndex}`);
      values.push(filter.mprStateCode);
      paramIndex++;
    }

    if (filter.mprZipCode !== undefined) {
      conditions.push(`mpr_zip_code = $${paramIndex}`);
      values.push(filter.mprZipCode);
      paramIndex++;
    }

    const whereClause = conditions.join(' AND ');

    const query = `
      UPDATE ${this.tableName}
      SET
        mpr_fob_rate_per_mile = $${paramIndex},
        change_date = $${paramIndex + 1},
        change_time = $${paramIndex + 2},
        change_user = $${paramIndex + 3},
        change_program = $${paramIndex + 4}
      WHERE ${whereClause}
    `;

    const updateValues = [
      ...values,
      newRatePerMile,
      changeDate,
      changeTime,
      changeUser,
      changeProgram,
    ];

    const result: QueryResult = await this.pool.query(query, updateValues);
    return result.rowCount || 0;
  }

  /**
   * Execute repository operation within a transaction
   * Maps to RPG record locking mechanism (CHAIN with lock, UPDATE, UNLCK)
   *
   * @param callback Transaction callback
   * @returns Result from callback
   */
  async transaction<T>(callback: (client: PoolClient) => Promise<T>): Promise<T> {
    const client = await this.pool.connect();
    try {
      await client.query('BEGIN');
      const result = await callback(client);
      await client.query('COMMIT');
      return result;
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  }

  /**
   * Map database row to FreightRate model
   * Converts snake_case column names to camelCase
   *
   * @param row Database row
   * @returns Freight rate model
   */
  private mapRowToModel(row: any): IFreightRate {
    return {
      mprCountry: row.mpr_country,
      mprRegion: row.mpr_region,
      mprStateCode: row.mpr_state_code,
      mprZipCode: row.mpr_zip_code,
      mprFobRatePerMile: parseFloat(row.mpr_fob_rate_per_mile),
      mprFobMilesToOmaha: row.mpr_fob_miles_to_omaha,
      mprFobUnusedDate: row.mpr_fob_unused_date,
      mprFobUnusedText: row.mpr_fob_unused_text,
      mprFobUnusedText2: row.mpr_fob_unused_text2,
      recordStatus: row.record_status,
      createDate: row.create_date,
      createTime: row.create_time,
      createUser: row.create_user,
      createProgram: row.create_program,
      changeDate: row.change_date,
      changeTime: row.change_time,
      changeUser: row.change_user,
      changeProgram: row.change_program,
    };
  }
}

export default PBGREFRRepository;
