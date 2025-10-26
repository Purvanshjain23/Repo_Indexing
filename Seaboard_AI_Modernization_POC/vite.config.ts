import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { resolve } from 'path';

export default defineConfig({
  plugins: [react()],
  
  // Serve from src/ui directory where your React components are
  root: resolve(__dirname, 'src', 'ui'),
  
  // Build output directory
  build: {
    outDir: resolve(__dirname, 'dist', 'ui'),
    emptyOutDir: true,
  },
  
  // Development server configuration
  server: {
    port: 3001,
    proxy: {
      // Proxy API calls to your Express backend
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true,
      },
    },
  },
  
  // Path resolution
  resolve: {
    alias: {
      '@': resolve(__dirname, 'src'),
      '@ui': resolve(__dirname, 'src', 'ui'),
      '@interfaces': resolve(__dirname, 'src', 'interfaces'),
    },
  },
  
  // Public directory for static assets
  publicDir: resolve(__dirname, 'public'),
});