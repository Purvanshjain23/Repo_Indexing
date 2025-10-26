/**
 * PBGREFR Database Mock for Development and Demo
 * 
 * This provides an in-memory database implementation f  public async findByKey(
    country: string,
    reg  public async update(
    country: stri  public async delete(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string,
  ): Promise<boolean> {   region: number,
    stateCode: string,
    zipCode: string,
    updates: Partial<IFreightRate>,
  ): Promise<IFreightRate | null> {mber,
    stateCode: string,
    zipCode: string,
  ): Promise<IFreightRate | null> {REFR
 * that matches the PostgreSQL interface but runs without external dependencies.
 */

import { 
  IFreightRate, 
  IFreightRateFilter, 
  IFreightRateCreateRequest,
  IFreightRateUpdateRequest 
} from '../../interfaces/PBGREFR';
import { IFreightRateDatabase } from '../../interfaces/Database';

export class PBGREFRMockDB implements IFreightRateDatabase {
  private static instance: PBGREFRMockDB;
  private data: IFreightRate[] = [
    {
      mprCountry: 'USA',
      mprRegion: 1,
      mprStateCode: 'NE',
      mprZipCode: 68102,
      mprFobRatePerMile: 2.5000,
      mprFobMilesToOmaha: 0,
      recordStatus: 'A',
      createDate: 20251017,
      createTime: 143000,
      createUser: 'SYSTEM',
      createProgram: 'PBGREFR',
      changeDate: 20251017,
      changeTime: 143000,
      changeUser: 'SYSTEM',
      changeProgram: 'PBGREFR'
    },
    {
      mprCountry: 'USA',
      mprRegion: 2,
      mprStateCode: 'IA',
      mprZipCode: 50001,
      mprFobRatePerMile: 2.2500,
      mprFobMilesToOmaha: 135,
      recordStatus: 'A',
      createDate: 20251017,
      createTime: 143000,
      createUser: 'SYSTEM',
      createProgram: 'PBGREFR',
      changeDate: 20251017,
      changeTime: 143000,
      changeUser: 'SYSTEM',
      changeProgram: 'PBGREFR'
    },
    {
      mprCountry: 'USA',
      mprRegion: 3,
      mprStateCode: 'KS',
      mprZipCode: 66002,
      mprFobRatePerMile: 2.7500,
      mprFobMilesToOmaha: 200,
      recordStatus: 'A',
      createDate: 20251017,
      createTime: 143000,
      createUser: 'SYSTEM',
      createProgram: 'PBGREFR',
      changeDate: 20251017,
      changeTime: 143000,
      changeUser: 'SYSTEM',
      changeProgram: 'PBGREFR'
    }
  ];

  public static getInstance(): PBGREFRMockDB {
    if (!PBGREFRMockDB.instance) {
      PBGREFRMockDB.instance = new PBGREFRMockDB();
    }
    return PBGREFRMockDB.instance;
  }

  private getKey(record: IFreightRate): string {
    return `${record.mprCountry}-${record.mprRegion}-${record.mprStateCode}-${record.mprZipCode}`;
  }

  public async findAll(filter?: IFreightRateFilter): Promise<{
    data: IFreightRate[];
    total: number;
    page: number;
    pageSize: number;
  }> {
    let result = this.data.filter(record => record.recordStatus === 'A');

    if (filter) {
      if (filter.mprCountry) {
        result = result.filter(r => r.mprCountry.includes(filter.mprCountry!));
      }
      if (filter.mprRegion) {
        result = result.filter(r => r.mprRegion === filter.mprRegion);
      }
      if (filter.mprStateCode) {
        result = result.filter(r => r.mprStateCode.includes(filter.mprStateCode!));
      }
      if (filter.mprZipCode) {
        result = result.filter(r => r.mprZipCode === filter.mprZipCode);
      }
    }

    const page = filter?.page ?? 0;
    const pageSize = filter?.pageSize ?? 13;
    
    return {
      data: result,
      total: result.length,
      page,
      pageSize
    };
  }

  public async findByKey(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string
  ): Promise<IFreightRate | null> {
    const record = this.data.find(r => 
      r.mprCountry === country &&
      r.mprRegion === region &&
      r.mprStateCode === stateCode &&
      r.mprZipCode === parseInt(zipCode, 10) &&
      r.recordStatus === 'A'
    );
    return record || null;
  }

  public async create(data: IFreightRateCreateRequest): Promise<IFreightRate> {
    const now = new Date();
    const currentDate = parseInt(now.toISOString().slice(0, 10).replace(/-/g, ''));
    const currentTime = parseInt(now.toTimeString().slice(0, 8).replace(/:/g, ''));

    const record: IFreightRate = {
      ...data,
      mprFobMilesToOmaha: data.mprFobMilesToOmaha || 0,
      recordStatus: 'A',
      createDate: currentDate,
      createTime: currentTime,
      createUser: 'DEMO',
      createProgram: 'PBGREFR',
      changeDate: currentDate,
      changeTime: currentTime,
      changeUser: 'DEMO',
      changeProgram: 'PBGREFR'
    };

    // Check for duplicates
    const existing = await this.findByKey(
      data.mprCountry,
      data.mprRegion,
      data.mprStateCode,
      data.mprZipCode.toString()
    );

    if (existing) {
      throw new Error(`Record already exists for ${this.getKey(record)}`);
    }

    this.data.push(record);
    return record;
  }

  public async update(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string,
    updates: Partial<IFreightRate>
  ): Promise<IFreightRate | null> {
    const index = this.data.findIndex(r => 
      r.mprCountry === country &&
      r.mprRegion === region &&
      r.mprStateCode === stateCode &&
      r.mprZipCode === parseInt(zipCode, 10) &&
      r.recordStatus === 'A'
    );

    if (index === -1) {
      return null;
    }

    const now = new Date();
    const currentDate = parseInt(now.toISOString().slice(0, 10).replace(/-/g, ''));
    const currentTime = parseInt(now.toTimeString().slice(0, 8).replace(/:/g, ''));

    this.data[index] = {
      ...this.data[index],
      ...updates,
      changeDate: currentDate,
      changeTime: currentTime,
      changeUser: 'DEMO',
      changeProgram: 'PBGREFR'
    };

    return this.data[index];
  }

  public async delete(
    country: string,
    region: number,
    stateCode: string,
    zipCode: string
  ): Promise<boolean> {
    const index = this.data.findIndex(r => 
      r.mprCountry === country &&
      r.mprRegion === region &&
      r.mprStateCode === stateCode &&
      r.mprZipCode === parseInt(zipCode, 10) &&
      r.recordStatus === 'A'
    );

    if (index === -1) {
      return false;
    }

    // Soft delete
    this.data[index].recordStatus = 'D';
    return true;
  }

  public async batchUpdate(updates: Array<{
    key: { country: string; region: number; stateCode: string; zipCode: string };
    data: Partial<IFreightRate>;
  }>): Promise<IFreightRate[]> {
    const results: IFreightRate[] = [];

    for (const update of updates) {
      const result = await this.update(
        update.key.country,
        update.key.region,
        update.key.stateCode,
        update.key.zipCode,
        update.data
      );
      if (result) {
        results.push(result);
      }
    }

    return results;
  }

  public async getRecordCount(): Promise<number> {
    return this.data.filter(r => r.recordStatus === 'A').length;
  }

  public async getStatistics(): Promise<{
    totalRecords: number;
    countryBreakdown: Record<string, number>;
    regionBreakdown: Record<number, number>;
    averageRate: number;
    rateRange: { min: number; max: number };
  }> {
    const activeRecords = this.data.filter(r => r.recordStatus === 'A');
    
    const countryBreakdown: Record<string, number> = {};
    const regionBreakdown: Record<number, number> = {};
    let totalRate = 0;
    let minRate = Number.MAX_VALUE;
    let maxRate = Number.MIN_VALUE;

    for (const record of activeRecords) {
      // Country breakdown
      countryBreakdown[record.mprCountry] = (countryBreakdown[record.mprCountry] || 0) + 1;
      
      // Region breakdown
      regionBreakdown[record.mprRegion] = (regionBreakdown[record.mprRegion] || 0) + 1;
      
      // Rate statistics
      totalRate += record.mprFobRatePerMile;
      minRate = Math.min(minRate, record.mprFobRatePerMile);
      maxRate = Math.max(maxRate, record.mprFobRatePerMile);
    }

    return {
      totalRecords: activeRecords.length,
      countryBreakdown,
      regionBreakdown,
      averageRate: activeRecords.length > 0 ? totalRate / activeRecords.length : 0,
      rateRange: {
        min: activeRecords.length > 0 ? minRate : 0,
        max: activeRecords.length > 0 ? maxRate : 0
      }
    };
  }

  public clearAll(): void {
    this.data = [];
  }
}

export default PBGREFRMockDB;