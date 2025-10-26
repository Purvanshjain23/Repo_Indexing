/**
 * PBGREFR - Mock Service for Demo
 *
 * Simplified service that works directly with the database interface
 * for the modernization demo
 */

import { IFreightRateDatabase } from '../interfaces/Database';
import {
  IFreightRate,
  IFreightRateCreateRequest,
  IFreightRateFilter,
  IFreightRateUpdateRequest,
  IBatchRateUpdateRequest,
} from '../interfaces/PBGREFR';

export class PBGREFRMockService {
  private database: IFreightRateDatabase;

  constructor(database: IFreightRateDatabase) {
    this.database = database;
  }

  async getFreightRates(filter?: IFreightRateFilter, userName?: string) {
    const result = await this.database.findAll(filter);
    
    return {
      data: result.data,
      totalRecords: result.total,
      currentPage: result.page,
      pageSize: result.pageSize,
      totalPages: Math.ceil(result.total / result.pageSize),
    };
  }

  async getFreightRateByKey(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: string;
  }) {
    return await this.database.findByKey(
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
    );
  }

  async createFreightRate(data: IFreightRateCreateRequest, userName?: string) {
    // Convert the create request to the freight rate format
    const freightRate: Omit<IFreightRate, 'id'> = {
      mprCountry: data.mprCountry,
      mprRegion: data.mprRegion,
      mprStateCode: data.mprStateCode,
      mprZipCode: data.mprZipCode,
      mprFobRatePerMile: data.mprFobRatePerMile,
      mprFobMilesToOmaha: data.mprFobMilesToOmaha ?? 0,
      recordStatus: 'A',
      createDate: parseInt(new Date().toISOString().slice(0, 10).replace(/-/g, '')),
      createTime: parseInt(new Date().toTimeString().slice(0, 8).replace(/:/g, '')),
      createUser: userName || 'DEMO',
      createProgram: 'PBGREFR',
      changeDate: parseInt(new Date().toISOString().slice(0, 10).replace(/-/g, '')),
      changeTime: parseInt(new Date().toTimeString().slice(0, 8).replace(/:/g, '')),
      changeUser: userName || 'DEMO',
      changeProgram: 'PBGREFR',
    };

    return await this.database.create(freightRate);
  }

  async updateFreightRate(
    key: {
      mprCountry: string;
      mprRegion: number;
      mprStateCode: string;
      mprZipCode: string;
    },
    updates: IFreightRateUpdateRequest,
    userName?: string,
  ) {
    const updateData: Partial<IFreightRate> = {
      ...updates,
      changeDate: parseInt(new Date().toISOString().slice(0, 10).replace(/-/g, '')),
      changeTime: parseInt(new Date().toTimeString().slice(0, 8).replace(/:/g, '')),
      changeUser: userName || 'DEMO',
      changeProgram: 'PBGREFR',
    };

    return await this.database.update(
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
      updateData,
    );
  }

  async deleteFreightRate(key: {
    mprCountry: string;
    mprRegion: number;
    mprStateCode: string;
    mprZipCode: string;
  }) {
    return await this.database.delete(
      key.mprCountry,
      key.mprRegion,
      key.mprStateCode,
      key.mprZipCode,
    );
  }

  async batchUpdateFreightRates(request: IBatchRateUpdateRequest, userName?: string) {
    // First, get all records that match the filter
    const result = await this.database.findAll(request.filter);
    const targetRecords = result.data;

    // Filter by target region if specified
    const recordsToUpdate = request.targetRegion
      ? targetRecords.filter(record => record.mprRegion === request.targetRegion)
      : targetRecords;

    // Prepare batch updates
    const updates = recordsToUpdate.map(record => ({
      key: {
        country: record.mprCountry,
        region: record.mprRegion,
        stateCode: record.mprStateCode,
        zipCode: record.mprZipCode.toString(),
      },
      data: {
        mprFobRatePerMile: request.percentageAdjustment
          ? record.mprFobRatePerMile * (1 + request.percentageAdjustment / 100)
          : request.newRatePerMile,
        changeDate: parseInt(new Date().toISOString().slice(0, 10).replace(/-/g, '')),
        changeTime: parseInt(new Date().toTimeString().slice(0, 8).replace(/:/g, '')),
        changeUser: userName || 'DEMO',
        changeProgram: 'PBGREFR',
      } as Partial<IFreightRate>,
    }));

    return await this.database.batchUpdate(updates);
  }

  async getStatistics() {
    return await this.database.getStatistics();
  }
}