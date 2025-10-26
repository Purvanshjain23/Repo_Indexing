/**
 * Seaboard AI Modernization POC - Main Entry Point
 *
 * This is the main entry point for the AI-driven IBM i modernization proof of concept.
 * It sets up an Express server with basic middleware and health check endpoint.
 */

import express, { NextFunction, Request, Response } from 'express';
import cors from 'cors';
import helmet from 'helmet';
import dotenv from 'dotenv';
import winston from 'winston';
import { PBGREFRMockDB } from './_shared/local/PBGREFRMockDB';
import { createPBGREFRRoutes } from './routes/PBGREFRRoutes';

// Load environment variables
dotenv.config();

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL ?? 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    winston.format.json(),
  ),
  defaultMeta: { service: 'seaboard-modernization-poc' },
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' }),
  ],
});

// Add console transport in development
if (process.env.NODE_ENV !== 'production') {
  logger.add(
    new winston.transports.Console({
      format: winston.format.simple(),
    }),
  );
}

// Create Express application
const app = express();
const port = process.env.PORT ?? 3000;

// Initialize database (mock or production based on environment)
const useMockDB = process.env.USE_MOCK_DB === 'true' || process.env.NODE_ENV === 'development';
const mockDB = new PBGREFRMockDB();

if (useMockDB) {
  logger.info('Database mock initialized with sample freight rate data');
} else {
  logger.info('Production database mode (PostgreSQL) - not yet implemented');
}

// Middleware
app.use(helmet()); // Security headers
app.use(cors()); // Enable CORS
app.use(express.json({ limit: '10mb' })); // Parse JSON bodies
app.use(express.urlencoded({ extended: true })); // Parse URL-encoded bodies

// Request logging middleware
app.use((req: Request, _res: Response, next: NextFunction) => {
  logger.info(`${req.method} ${req.path}`, {
    ip: req.ip,
    userAgent: req.get('User-Agent'),
  });
  next();
});

// Health check endpoint
app.get('/health', (_req: Request, res: Response) => {
  res.status(200).json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    version: process.env.npm_package_version ?? '1.0.0',
    environment: process.env.NODE_ENV ?? 'development',
    uptime: process.uptime(),
  });
});

// Root endpoint
app.get('/', (_req: Request, res: Response) => {
  res.json({
    message: 'Seaboard AI Modernization POC',
    description: 'AI-driven modernization of IBM i legacy systems to cloud-native solutions',
    version: process.env.npm_package_version ?? '1.0.0',
    endpoints: {
      health: '/health',
      api: '/api/v1',
    },
  });
});

// API routes
app.get('/api/v1', (_req: Request, res: Response) => {
  res.json({
    message: 'API v1 - Ready for legacy system modernization',
    capabilities: [
      'RPG to TypeScript conversion',
      'DDS to modern schema transformation',
      'CL to modern automation scripts',
      'Dependency analysis and mapping',
    ],
    endpoints: {
      pbgrefr: '/api/v1/pbgrefr - Freight Rate Management (PBGREFR legacy module)',
    },
    status: 'ready',
  });
});

// PBGREFR Routes - AI-modernized freight rate management
app.use('/api/v1/pbgrefr', createPBGREFRRoutes(mockDB));

// 404 handler
app.use('*', (req: Request, res: Response) => {
  res.status(404).json({
    error: 'Not Found',
    message: `Route ${req.originalUrl} not found`,
    timestamp: new Date().toISOString(),
  });
});

// Error handling middleware
app.use((err: Error, _req: Request, res: Response, _next: NextFunction) => {
  logger.error('Unhandled error:', err);
  res.status(500).json({
    error: 'Internal Server Error',
    message: process.env.NODE_ENV === 'production' ? 'Something went wrong' : err.message,
    timestamp: new Date().toISOString(),
  });
});

// Start server
const server = app.listen(port, () => {
  logger.info(`🚀 Seaboard Modernization POC server running on port ${port}`);
  logger.info(`📊 Health check available at http://localhost:${port}/health`);
  logger.info(`🔧 Environment: ${process.env.NODE_ENV ?? 'development'}`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  logger.info('SIGTERM received, shutting down gracefully');
  server.close(() => {
    logger.info('Process terminated');
    process.exit(0);
  });
});

process.on('SIGINT', () => {
  logger.info('SIGINT received, shutting down gracefully');
  server.close(() => {
    logger.info('Process terminated');
    process.exit(0);
  });
});

export default app;
