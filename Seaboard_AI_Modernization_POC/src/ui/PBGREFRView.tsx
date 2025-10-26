/**
 * PBGREFR - MPR FOB Omaha Rate Table View
 *
 * React component replacing IBM i green-screen interface
 * Maps DDS display file PBGREFR# to modern web UI
 *
 * Original DDS Records:
 * - #SFLRCD: Subfile record (data grid rows)
 * - #SFLCTL: Subfile control (filters and pagination)
 * - #CMDTXT1: Command key text (replaced by buttons)
 * - #CONFIRM: Confirmation prompt (replaced by modal dialogs)
 *
 * Command Keys → UI Elements:
 * - F3 (Exit) → Close/Back button
 * - F4 (Prompt) → Search/Autocomplete
 * - F9 (Mode Switch) → Add/Edit mode toggle
 * - F10 (Edit Control) → Control settings button
 * - F15 (Batch Update) → Batch update button
 */

import React, { useState, useEffect, useCallback } from 'react';
import axios from 'axios';
import {
  IFreightRate,
  IFreightRateCreateRequest,
  IFreightRateUpdateRequest,
  IFreightRateFilter,
  IFreightRateListResponse,
  PBGREFR_CONSTANTS,
} from '../interfaces/PBGREFR';

// API base URL
const API_BASE_URL = '/api/v1/pbgrefr';

/**
 * Operation mode enum
 */
enum OperationMode {
  VIEW = 'VIEW',
  ADD = 'ADD',
  EDIT = 'EDIT',
}

/**
 * PBGREFR View Component
 * Main React component for freight rate management
 */
const PBGREFRView: React.FC = () => {
  // State: Data grid
  const [freightRates, setFreightRates] = useState<IFreightRate[]>([]);
  const [selectedRate, setSelectedRate] = useState<IFreightRate | null>(null);

  // State: Pagination (maps to RPG subfile pagination)
  const [currentPage, setCurrentPage] = useState(0);
  const [pageSize] = useState(PBGREFR_CONSTANTS.SUBFILE_PAGE_SIZE); // 13 records
  const [totalPages, setTotalPages] = useState(0);
  const [totalRecords, setTotalRecords] = useState(0);

  // State: Filters (maps to DDS #2PZAA, #2W5NY, #2P0AA, #2W6NY)
  const [filters, setFilters] = useState<IFreightRateFilter>({
    mprCountry: '',
    mprRegion: undefined,
    mprStateCode: '',
    mprZipCode: undefined,
    mprFobRatePerMile: undefined,
  });

  // State: Operation mode (maps to RPG W0PMD: 'ADD' or 'CHG')
  const [mode, setMode] = useState<OperationMode>(OperationMode.VIEW);

  // State: Form data
  const [formData, setFormData] = useState<Partial<IFreightRate>>({});

  // State: UI control
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [showConfirmDialog, setShowConfirmDialog] = useState(false);
  const [pendingAction, setPendingAction] = useState<(() => void) | null>(null);

  /**
   * Load freight rates from API
   * Maps to RPG: BAIZSF/BBLDSF subroutines
   */
  const loadFreightRates = useCallback(async () => {
    setLoading(true);
    setError(null);

    try {
      const params: Record<string, any> = {
        page: currentPage,
        pageSize,
      };

      if (filters.mprCountry) params.country = filters.mprCountry;
      if (filters.mprRegion) params.region = filters.mprRegion;
      if (filters.mprStateCode) params.stateCode = filters.mprStateCode;
      if (filters.mprZipCode) params.zipCode = filters.mprZipCode;
      if (filters.mprFobRatePerMile) params.rate = filters.mprFobRatePerMile;

      const response = await axios.get<{
        success: boolean;
        data: IFreightRate[];
        pagination: {
          totalRecords: number;
          currentPage: number;
          totalPages: number;
        };
      }>(API_BASE_URL, { params });

      setFreightRates(response.data.data);
      setTotalPages(response.data.pagination.totalPages);
      setTotalRecords(response.data.pagination.totalRecords);

      // Maps to RPG message Y2U0008 (No data to display)
      if (response.data.data.length === 0) {
        setError('No freight rates found matching the filter criteria.');
      }
    } catch (err) {
      setError('Error loading freight rates. Please try again.');
      console.error('Error loading freight rates:', err);
    } finally {
      setLoading(false);
    }
  }, [currentPage, pageSize, filters]);

  /**
   * Load data on mount and when filters/pagination change
   */
  useEffect(() => {
    loadFreightRates();
  }, [loadFreightRates]);

  /**
   * Handle filter change
   * Maps to RPG: Lines 409-496 (filter field change detection)
   */
  const handleFilterChange = (field: keyof IFreightRateFilter, value: any) => {
    setFilters((prev) => ({ ...prev, [field]: value }));
    setCurrentPage(0); // Reset to first page on filter change
  };

  /**
   * Handle page navigation
   * Maps to RPG: ROLLUP command key (line 140)
   */
  const handlePageChange = (newPage: number) => {
    if (newPage >= 0 && newPage < totalPages) {
      setCurrentPage(newPage);
    }
  };

  /**
   * Switch to Add mode
   * Maps to RPG: F9 command key in CHANGE mode → ADD mode (lines 907-918)
   */
  const handleAddMode = () => {
    setMode(OperationMode.ADD);
    setFormData({
      mprCountry: '',
      mprRegion: undefined,
      mprStateCode: '',
      mprZipCode: undefined,
      mprFobRatePerMile: undefined,
      mprFobMilesToOmaha: 0,
    });
    setSelectedRate(null);
  };

  /**
   * Switch to Edit mode
   * Maps to RPG: Selection '2' (Edit) in subfile (lines 606-638)
   */
  const handleEditMode = (rate: IFreightRate) => {
    setMode(OperationMode.EDIT);
    setFormData(rate);
    setSelectedRate(rate);
  };

  /**
   * Cancel current operation
   * Maps to RPG: F3 command key (Exit)
   */
  const handleCancel = () => {
    setMode(OperationMode.VIEW);
    setFormData({});
    setSelectedRate(null);
    setError(null);
    setSuccessMessage(null);
  };

  /**
   * Create new freight rate
   * Maps to RPG: SACRRC subroutine (lines 1019-1081)
   */
  const handleCreate = async () => {
    try {
      const createRequest: IFreightRateCreateRequest = {
        mprCountry: formData.mprCountry!,
        mprRegion: formData.mprRegion!,
        mprStateCode: formData.mprStateCode!,
        mprZipCode: formData.mprZipCode!,
        mprFobRatePerMile: formData.mprFobRatePerMile!,
        mprFobMilesToOmaha: formData.mprFobMilesToOmaha,
      };

      const response = await axios.post(API_BASE_URL, createRequest);

      if (response.data.success) {
        setSuccessMessage('Freight rate created successfully.');
        setMode(OperationMode.VIEW);
        setFormData({});
        loadFreightRates();
      }
    } catch (err: any) {
      const errorMsg =
        err.response?.data?.error?.details || 'Error creating freight rate.';
      setError(errorMsg);
    }
  };

  /**
   * Update existing freight rate
   * Maps to RPG: SBCHRC subroutine (lines 1083-1194)
   */
  const handleUpdate = async () => {
    if (!selectedRate) return;

    try {
      const { mprCountry, mprRegion, mprStateCode, mprZipCode } = selectedRate;
      const updateRequest: IFreightRateUpdateRequest = {
        mprCountry,
        mprRegion,
        mprStateCode,
        mprZipCode,
        mprFobRatePerMile: formData.mprFobRatePerMile,
        mprFobMilesToOmaha: formData.mprFobMilesToOmaha,
      };

      const response = await axios.put(
        `${API_BASE_URL}/${mprCountry}/${mprRegion}/${mprStateCode}/${mprZipCode}`,
        updateRequest,
      );

      if (response.data.success) {
        setSuccessMessage('Freight rate updated successfully.');
        setMode(OperationMode.VIEW);
        setFormData({});
        setSelectedRate(null);
        loadFreightRates();
      }
    } catch (err: any) {
      const errorMsg =
        err.response?.data?.error?.details || 'Error updating freight rate.';
      setError(errorMsg);
    }
  };

  /**
   * Delete freight rate
   * (Not in original RPG, added for completeness)
   */
  const handleDelete = async (rate: IFreightRate) => {
    try {
      const { mprCountry, mprRegion, mprStateCode, mprZipCode } = rate;
      const response = await axios.delete(
        `${API_BASE_URL}/${mprCountry}/${mprRegion}/${mprStateCode}/${mprZipCode}`,
      );

      if (response.data.success) {
        setSuccessMessage('Freight rate deleted successfully.');
        loadFreightRates();
      }
    } catch (err: any) {
      const errorMsg =
        err.response?.data?.error?.details || 'Error deleting freight rate.';
      setError(errorMsg);
    }
  };

  /**
   * Prompt for confirmation
   * Maps to RPG: DHPRCF subroutine (lines 697-793) and #CONFIRM record
   */
  const promptConfirmation = (action: () => void) => {
    setPendingAction(() => action);
    setShowConfirmDialog(true);
  };

  /**
   * Handle confirmation dialog result
   */
  const handleConfirm = (confirmed: boolean) => {
    setShowConfirmDialog(false);

    if (confirmed && pendingAction) {
      pendingAction();
    }

    setPendingAction(null);
  };

  /**
   * Render component
   */
  return (
    <div className="pbgrefr-container">
      {/* Header - Maps to DDS screen title (line 179) */}
      <header className="header">
        <h1>Edit MPR FOB Omaha Rate Table</h1>
        <div className="header-info">
          <span>Mode: {mode}</span>
          <span>Records: {totalRecords}</span>
        </div>
      </header>

      {/* Error/Success Messages */}
      {error && (
        <div className="alert alert-error" role="alert">
          {error}
        </div>
      )}
      {successMessage && (
        <div className="alert alert-success" role="alert">
          {successMessage}
        </div>
      )}

      {/* Filter Section - Maps to DDS #SFLCTL filter fields (lines 196-237) */}
      {mode === OperationMode.VIEW && (
        <div className="filter-section">
          <h3>Filter Criteria</h3>
          <div className="filter-grid">
            <div className="form-group">
              <label>Country:</label>
              <input
                type="text"
                maxLength={6}
                value={filters.mprCountry || ''}
                onChange={(e) => handleFilterChange('mprCountry', e.target.value)}
                placeholder="MPR Country"
              />
            </div>
            <div className="form-group">
              <label>Region (1-14):</label>
              <input
                type="number"
                min={1}
                max={14}
                value={filters.mprRegion || ''}
                onChange={(e) =>
                  handleFilterChange(
                    'mprRegion',
                    e.target.value ? parseInt(e.target.value) : undefined,
                  )
                }
                placeholder="MPR Region"
              />
            </div>
            <div className="form-group">
              <label>State Code:</label>
              <input
                type="text"
                maxLength={2}
                value={filters.mprStateCode || ''}
                onChange={(e) =>
                  handleFilterChange('mprStateCode', e.target.value.toUpperCase())
                }
                placeholder="ST"
              />
            </div>
            <div className="form-group">
              <label>Zip Code:</label>
              <input
                type="number"
                min={0}
                max={99999}
                value={filters.mprZipCode || ''}
                onChange={(e) =>
                  handleFilterChange(
                    'mprZipCode',
                    e.target.value ? parseInt(e.target.value) : undefined,
                  )
                }
                placeholder="Zip"
              />
            </div>
          </div>
        </div>
      )}

      {/* Data Grid - Maps to DDS #SFLRCD subfile (lines 21-120) */}
      {mode === OperationMode.VIEW && (
        <div className="data-grid-section">
          <table className="data-grid">
            <thead>
              <tr>
                <th>Actions</th>
                <th>Country</th>
                <th>Region</th>
                <th>State Code</th>
                <th>Zip Code</th>
                <th>FOB Rate per Mile</th>
                <th>Miles to Omaha</th>
              </tr>
            </thead>
            <tbody>
              {freightRates.map((rate) => (
                <tr key={rate.mprCountry + rate.mprRegion + rate.mprStateCode + rate.mprZipCode}>
                  <td>
                    <button
                      className="btn btn-sm btn-edit"
                      onClick={() => handleEditMode(rate)}
                      title="Edit (Selection '2')"
                    >
                      Edit
                    </button>
                    <button
                      className="btn btn-sm btn-delete"
                      onClick={() => promptConfirmation(() => handleDelete(rate))}
                      title="Delete"
                    >
                      Delete
                    </button>
                  </td>
                  <td>{rate.mprCountry}</td>
                  <td>{rate.mprRegion}</td>
                  <td>{rate.mprStateCode}</td>
                  <td>{rate.mprZipCode}</td>
                  <td>{rate.mprFobRatePerMile.toFixed(4)}</td>
                  <td>{rate.mprFobMilesToOmaha}</td>
                </tr>
              ))}
            </tbody>
          </table>

          {/* Pagination - Maps to RPG subfile pagination */}
          <div className="pagination">
            <button
              className="btn"
              disabled={currentPage === 0}
              onClick={() => handlePageChange(currentPage - 1)}
            >
              Previous
            </button>
            <span>
              Page {currentPage + 1} of {totalPages}
            </span>
            <button
              className="btn"
              disabled={currentPage >= totalPages - 1}
              onClick={() => handlePageChange(currentPage + 1)}
            >
              Next (ROLLUP)
            </button>
          </div>
        </div>
      )}

      {/* Add/Edit Form - Maps to DDS subfile record fields */}
      {(mode === OperationMode.ADD || mode === OperationMode.EDIT) && (
        <div className="form-section">
          <h3>{mode === OperationMode.ADD ? 'Add New Freight Rate' : 'Edit Freight Rate'}</h3>
          <form>
            {/* Key Fields */}
            <div className="form-group">
              <label>
                Country<span className="required">*</span>:
              </label>
              <input
                type="text"
                maxLength={6}
                value={formData.mprCountry || ''}
                onChange={(e) =>
                  setFormData({ ...formData, mprCountry: e.target.value })
                }
                disabled={mode === OperationMode.EDIT}
                required
              />
            </div>

            <div className="form-group">
              <label>
                Region (1-14)<span className="required">*</span>:
              </label>
              <input
                type="number"
                min={1}
                max={14}
                value={formData.mprRegion || ''}
                onChange={(e) =>
                  setFormData({ ...formData, mprRegion: parseInt(e.target.value) })
                }
                disabled={mode === OperationMode.EDIT}
                required
              />
            </div>

            <div className="form-group">
              <label>
                State Code<span className="required">*</span>:
              </label>
              <input
                type="text"
                maxLength={2}
                value={formData.mprStateCode || ''}
                onChange={(e) =>
                  setFormData({ ...formData, mprStateCode: e.target.value.toUpperCase() })
                }
                disabled={mode === OperationMode.EDIT}
                required
              />
            </div>

            <div className="form-group">
              <label>
                Zip Code<span className="required">*</span>:
              </label>
              <input
                type="number"
                min={0}
                max={99999}
                value={formData.mprZipCode || ''}
                onChange={(e) =>
                  setFormData({ ...formData, mprZipCode: parseInt(e.target.value) })
                }
                disabled={mode === OperationMode.EDIT}
                required
              />
            </div>

            {/* Data Fields */}
            <div className="form-group">
              <label>
                FOB Rate per Mile<span className="required">*</span>:
              </label>
              <input
                type="number"
                step="0.0001"
                min="0.0001"
                value={formData.mprFobRatePerMile || ''}
                onChange={(e) =>
                  setFormData({ ...formData, mprFobRatePerMile: parseFloat(e.target.value) })
                }
                required
              />
            </div>

            <div className="form-group">
              <label>Miles to Omaha:</label>
              <input
                type="number"
                min={0}
                value={formData.mprFobMilesToOmaha || 0}
                onChange={(e) =>
                  setFormData({ ...formData, mprFobMilesToOmaha: parseInt(e.target.value) })
                }
              />
            </div>
          </form>
        </div>
      )}

      {/* Command Buttons - Maps to DDS #CMDTXT1 (lines 242-249) */}
      <footer className="command-buttons">
        {mode === OperationMode.VIEW && (
          <>
            <button className="btn btn-primary" onClick={handleAddMode}>
              Add (F9)
            </button>
            <button className="btn" onClick={() => loadFreightRates()}>
              Refresh (HOME)
            </button>
          </>
        )}

        {(mode === OperationMode.ADD || mode === OperationMode.EDIT) && (
          <>
            <button
              className="btn btn-primary"
              onClick={() =>
                promptConfirmation(
                  mode === OperationMode.ADD ? handleCreate : handleUpdate,
                )
              }
            >
              {mode === OperationMode.ADD ? 'Create' : 'Update'} (ENTER)
            </button>
            <button className="btn" onClick={handleCancel}>
              Cancel (F3)
            </button>
          </>
        )}
      </footer>

      {/* Confirmation Dialog - Maps to DDS #CONFIRM (lines 251-260) */}
      {showConfirmDialog && (
        <div className="modal-overlay">
          <div className="modal-dialog">
            <h3>Confirm Action</h3>
            <p>Are you sure you want to proceed with this action?</p>
            <div className="modal-buttons">
              <button className="btn btn-primary" onClick={() => handleConfirm(true)}>
                Yes (Y)
              </button>
              <button className="btn" onClick={() => handleConfirm(false)}>
                No (N)
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default PBGREFRView;
