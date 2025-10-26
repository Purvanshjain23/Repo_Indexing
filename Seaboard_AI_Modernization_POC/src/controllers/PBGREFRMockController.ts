/**
 * PBGREFR - Mock Controller for Demo
 *
 * Simplified controller that works with the mock service for the demo
 */

import { Request, Response } from 'express';
import { IFreightRateDatabase } from '../interfaces/Database';
import { PBGREFRMockService } from '../services/PBGREFRMockService';
import {
  IBatchRateUpdateRequest,
  IFreightRateCreateRequest,
  IFreightRateFilter,
  IFreightRateUpdateRequest,
} from '../interfaces/PBGREFR';

export class PBGREFRMockController {
  private service: PBGREFRMockService;

  constructor(database: IFreightRateDatabase) {
    this.service = new PBGREFRMockService(database);
  }

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

      const result = await this.service.getFreightRates(filter, 'DEMO');

      res.status(200).json({
        success: true,
        message: 'Freight rates retrieved successfully',
        data: result.data,
        pagination: {
          totalRecords: result.totalRecords,
          currentPage: result.currentPage,
          pageSize: result.pageSize,
          totalPages: result.totalPages,
        },
        filters: filter,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error getting freight rates:', error);
      res.status(500).json({
        success: false,
        message: 'Failed to retrieve freight rates',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async getFreightRateByKey(req: Request, res: Response): Promise<void> {
    try {
      const { country, region, stateCode, zipCode } = req.params;

      const freightRate = await this.service.getFreightRateByKey({
        mprCountry: country,
        mprRegion: parseInt(region, 10),
        mprStateCode: stateCode,
        mprZipCode: zipCode,
      });

      if (!freightRate) {
        res.status(404).json({
          success: false,
          message: 'Freight rate not found',
          timestamp: new Date().toISOString(),
        });
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate retrieved successfully',
        data: freightRate,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error getting freight rate by key:', error);
      res.status(500).json({
        success: false,
        message: 'Failed to retrieve freight rate',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async createFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const createRequest: IFreightRateCreateRequest = req.body;

      const newFreightRate = await this.service.createFreightRate(createRequest, 'DEMO');

      res.status(201).json({
        success: true,
        message: 'Freight rate created successfully',
        data: newFreightRate,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error creating freight rate:', error);
      res.status(400).json({
        success: false,
        message: 'Failed to create freight rate',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async updateFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const { country, region, stateCode, zipCode } = req.params;
      const updateRequest: IFreightRateUpdateRequest = req.body;

      const updatedFreightRate = await this.service.updateFreightRate(
        {
          mprCountry: country,
          mprRegion: parseInt(region, 10),
          mprStateCode: stateCode,
          mprZipCode: zipCode,
        },
        updateRequest,
        'DEMO',
      );

      if (!updatedFreightRate) {
        res.status(404).json({
          success: false,
          message: 'Freight rate not found',
          timestamp: new Date().toISOString(),
        });
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate updated successfully',
        data: updatedFreightRate,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error updating freight rate:', error);
      res.status(400).json({
        success: false,
        message: 'Failed to update freight rate',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async deleteFreightRate(req: Request, res: Response): Promise<void> {
    try {
      const { country, region, stateCode, zipCode } = req.params;

      const deleted = await this.service.deleteFreightRate({
        mprCountry: country,
        mprRegion: parseInt(region, 10),
        mprStateCode: stateCode,
        mprZipCode: zipCode,
      });

      if (!deleted) {
        res.status(404).json({
          success: false,
          message: 'Freight rate not found',
          timestamp: new Date().toISOString(),
        });
        return;
      }

      res.status(200).json({
        success: true,
        message: 'Freight rate deleted successfully',
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error deleting freight rate:', error);
      res.status(500).json({
        success: false,
        message: 'Failed to delete freight rate',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async batchUpdateFreightRates(req: Request, res: Response): Promise<void> {
    try {
      const batchRequest: IBatchRateUpdateRequest = req.body;

      const updatedRates = await this.service.batchUpdateFreightRates(batchRequest, 'DEMO');

      res.status(200).json({
        success: true,
        message: `Batch update completed successfully for ${updatedRates.length} freight rates`,
        data: updatedRates,
        summary: {
          totalUpdated: updatedRates.length,
          targetRegion: batchRequest.targetRegion,
          newRate: batchRequest.newRatePerMile,
          percentageAdjustment: batchRequest.percentageAdjustment,
        },
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error in batch update:', error);
      res.status(400).json({
        success: false,
        message: 'Failed to perform batch update',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }

  async getStatistics(req: Request, res: Response): Promise<void> {
    try {
      const stats = await this.service.getStatistics();

      res.status(200).json({
        success: true,
        message: 'Statistics retrieved successfully',
        data: stats,
        timestamp: new Date().toISOString(),
      });
    } catch (error) {
      console.error('Error getting statistics:', error);
      res.status(500).json({
        success: false,
        message: 'Failed to retrieve statistics',
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString(),
      });
    }
  }
}